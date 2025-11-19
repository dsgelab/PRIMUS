.libPaths("/shared-directory/sd-tools/apps/R/lib/")

#### Libraries:
suppressPackageStartupMessages({
    library(data.table)
    library(arrow)
    library(dplyr)
    library(tidyr)
    library(lubridate)
    library(did)
    library(ggplot2)
    library(patchwork)
})

##### Arguments
events_file = "/media/volume/Projects/DSGELabProject1/DiD_Experiments/Version5_Highthroughput/ProcessedEvents_20251119/processed_events.parquet"
outcomes_file = "/media/volume/Projects/DSGELabProject1/DiD_Experiments/Version5_Highthroughput/ProcessedOutcomes_20251119/processed_outcomes.parquet"
doctor_list = "/media/volume/Projects/DSGELabProject1/doctors_20250424.csv"
covariate_file = "/media/volume/Projects/DSGELabProject1/doctor_characteristics_20250520.csv"
outdir = "/media/volume/Projects/DSGELabProject1/DiD_Experiments/Version5_Highthroughput/Results/Validation_Statins/"
if (!dir.exists(outdir)) {dir.create(outdir, recursive = TRUE)}

# Test 1
event_code = 'Purch_C10AA' # all statins available
outcome_code = "C10AA07" # rosuvastatin

#### Main
N_THREADS = 10
setDTthreads(N_THREADS) 
# not using all threads to easily run in background

# STEP 1: Data Loading 
covariates = fread(covariate_file)
# Prepare covariates 
covariates[, `:=`(
    SPECIALTY = as.character(INTERPRETATION),
    BIRTH_YEAR = as.numeric(substr(BIRTH_DATE, 1, 4))
)]
covariates[, `:=`(
    BIRTH_DATE = NULL, 
    INTERPRETATION = NULL)
]
doctor_ids = fread(doctor_list, header = FALSE)$V1

events = as.data.table(read_parquet(events_file))
event_code_parts = strsplit(event_code, "_")[[1]]
event_source = event_code_parts[1]
event_actual_code = event_code_parts[2]

# Filter events based on the event code
events = events[SOURCE == event_source & startsWith(as.character(CODE), event_actual_code), ]
event_ids = unique(events$PATIENT_ID)

# split doctors based on previous use of statins 
# For each doctor, count distinct C10AA codes (excluding C10AA07) before first C10AA07 purchase
first_c10aa07 <- events[CODE == "C10AA07",.(first_date_c10aa07 = min(DATE, na.rm = TRUE)),by = PATIENT_ID]
# Distinct non-C10AA07 statin codes before first C10AA07
statin_before_c10aa07 <- events[CODE != "C10AA07"][
    first_c10aa07, on = .(PATIENT_ID)
][
    DATE < first_date_c10aa07,
    .(OTHER_STATIN_BEFORE = as.integer(uniqueN(CODE))),
    by = PATIENT_ID
]

# Finalize new event data (input 0 if no previous statin use)
events <- merge(events, statin_before_c10aa07, by = "PATIENT_ID", all.x = TRUE)
events[is.na(OTHER_STATIN_BEFORE), OTHER_STATIN_BEFORE := 0L]

# Load outcomes (N, Ni, and Y for desired medication)
outcomes_cols = c("DOCTOR_ID", "YEAR", paste0("N_", outcome_code), paste0("Y_", outcome_code), paste0("first_year_", outcome_code), paste0("last_year_", outcome_code))
outcomes = as.data.table(read_parquet(outcomes_file, col_select = outcomes_cols))

# STEP 2: Data Preparation
# Process and merge events and outcomes

events_new = events[CODE == outcome_code]
setnames(events_new, "PATIENT_ID", "DOCTOR_ID")
# QC: Keep only the first event per DOCTOR_ID, in case multiple codes exist
events_new = events_new[order(DOCTOR_ID, DATE)]
events_new = events_new[, .SD[1], by = DOCTOR_ID]
# QC: Ensure events are only for doctors in the doctor list
outcomes_filtered = outcomes[DOCTOR_ID %in% doctor_ids]

df_merged = events_new[outcomes_filtered, on = "DOCTOR_ID", allow.cartesian = TRUE]
df_merged[, DATE := as.Date(DATE)]
df_merged[, EVENT := ifelse(!is.na(DATE), 1, 0)]
df_merged[, EVENT_YEAR := ifelse(!is.na(DATE), as.numeric(format(DATE, "%Y")), NA_real_)]
df_merged[, DATE := NULL]

# Process prescription timeframe
# 1. Calculate original min and max year across all doctors in the cohort
original_min_year <- min(df_merged[[paste0("first_year_", outcome_code)]], na.rm = TRUE)
original_max_year <- max(df_merged[[paste0("last_year_", outcome_code)]], na.rm = TRUE)
# 2. Add buffer to min and max year to avoid bias due to medications entering or exiting the market
buffer_years = 1
buffered_min_year <- original_min_year + buffer_years
buffered_max_year <- original_max_year - buffer_years
cat(sprintf("Original range of outcomes: %d-%d | Buffered range of outcomes: %d-%d\n",
    original_min_year,
    original_max_year,
    buffered_min_year,
    buffered_max_year
))
# Remove all information outside of buffered range
df_merged <- df_merged[YEAR >= buffered_min_year & YEAR <= buffered_max_year]
# 3. Exclude events which happened before the first prescription of the outcome / or after the last one (using buffered range)
df_merged <- df_merged[is.na(EVENT_YEAR) | (EVENT_YEAR >= buffered_min_year & EVENT_YEAR <= buffered_max_year)]

# Merge covariates
df_complete = covariates[df_merged, on = "DOCTOR_ID"]
df_complete[, `:=`(
    AGE = YEAR - BIRTH_YEAR,
    AGE_IN_2023 = 2023 - BIRTH_YEAR,
    AGE_AT_EVENT = fifelse(is.na(EVENT_YEAR), NA_real_, EVENT_YEAR - BIRTH_YEAR)
)]

# Filter out events after 60 and prescriptions after 60
events_after60 = df_complete[AGE_AT_EVENT > 60 & !is.na(AGE_AT_EVENT), unique(DOCTOR_ID)]
df_complete = df_complete[!(DOCTOR_ID %in% events_after60) & AGE <= 60]

# STEP 3: Analysis using 'did' package
df_model <- as.data.table(df_complete)[
    , `:=`(
        SPECIALTY = factor(SPECIALTY, levels = c("", setdiff(unique(df_complete$SPECIALTY), ""))),
        SEX = factor(SEX, levels = c(1, 2), labels = c("Male", "Female")),
        Y = get(paste0("Y_", outcome_code)),
        Ni = get(paste0("N_", outcome_code))
    )
]

#prepare the model data
df_model[is.na(Y), Y := 0]
df_model$ID <- as.integer(factor(df_model$DOCTOR_ID))                       # create a numeric ID variable
df_model$G <- ifelse(is.na(df_model$EVENT_YEAR), 0, df_model$EVENT_YEAR)  # G = group of first treatment year, 0 for never-treated
df_model$T <- df_model$YEAR    

# Split analysis by OTHER_STATIN_BEFORE
controls_ids <- unique(df_model[EVENT == 0, DOCTOR_ID])
n_controls <- length(controls_ids)

case_groups <- unique(df_model[EVENT == 1 & !is.na(OTHER_STATIN_BEFORE), .(DOCTOR_ID, GROUP = OTHER_STATIN_BEFORE)])
groups <- sort(unique(case_groups$GROUP))

all_group_results <- list()

for (g in groups) {
    case_ids_g <- case_groups[GROUP == g, DOCTOR_ID]
    ids_g <- unique(c(controls_ids, case_ids_g))
    df_g <- df_model[DOCTOR_ID %in% ids_g]

    n_cases_g <- length(unique(df_g[EVENT == 1, DOCTOR_ID]))
    if (n_cases_g == 0) next

    att_gt_g <- att_gt(
        yname = "Y",
        tname = "T",
        idname = "ID",
        gname = "G",
        xformla = ~ BIRTH_YEAR + SEX + SPECIALTY,
        data = df_g,
        est_method = "dr",
        control_group = "notyettreated",
        clustervars = "ID",
        pl = TRUE,
        cores = N_THREADS
    )

    aggdyn_g <- aggte(att_gt_g, type = "dynamic", na.rm = TRUE)
    res_g <- data.frame(
        time = aggdyn_g$egt,
        att = aggdyn_g$att.egt,
        se = aggdyn_g$se.egt,
        outcome = "Y (Medication Ratio)",
        n_cases = n_cases_g,
        n_controls = n_controls,
        group = g
    )
    res_g <- res_g[res_g$time >= -3 & res_g$time <= 3, ]
    all_group_results[[as.character(g)]] <- res_g
}

res_all <- data.table::rbindlist(all_group_results, use.names = TRUE, fill = TRUE)
res_all[, group := factor(group)]

p_combined <- ggplot(res_all, aes(x = time, y = att, color = group, group = group)) +
    geom_line() +
    geom_point(position = position_dodge(width = 0.5)) +
    geom_errorbar(
        aes(ymin = att - 1.96 * se, ymax = att + 1.96 * se),
        width = 0.15,
        position = position_dodge(width = 0.5)
    ) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
    labs(
        title = "Effect of Rosuvastatin Prescription Behaviour, by previous statins use",
        subtitle = paste0(
            sprintf("Total Controls: %d\n", n_controls),
            paste(sapply(levels(res_all$group), function(g) {
                n_cases_g <- unique(res_all[group == g, n_cases])
                sprintf("Previous distinct statins used = %s: Cases = %d\n", g, n_cases_g)
            }), collapse = "")
        ),
        x = "Years from Event",
        y = "Change in Prescription Behavior\n(Difference in Difference ATT)",
        color = "N of (unique) statins used\n before Rosuvastatin"
    ) +
    theme_minimal()
ggsave(file.path(outdir, paste0("ValidationPlot2.png")), p_combined, width = 10, height = 6, dpi = 300)