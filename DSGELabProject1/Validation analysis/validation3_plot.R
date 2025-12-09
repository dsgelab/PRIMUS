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
events_file = "/media/volume/Projects/DSGELabProject1/DiD_Experiments/Version3_Highthroughput/ProcessedEvents_20251020/processed_events.parquet"
outcomes_file1 = "/media/volume/Projects/DSGELabProject1/DiD_Experiments/Version3_Highthroughput/ProcessedOutcomes_20251020/processed_outcomes.parquet"
outcomes_file2 = "/media/volume/Projects/DSGELabProject1/DiD_Experiments/Version2_Highthroughput_drop/ProcessedOutcomes_20251028/processed_outcomes.parquet"
doctor_list = "/media/volume/Projects/DSGELabProject1/doctors_20250424.csv"
covariate_file = "/media/volume/Projects/DSGELabProject1/doctor_characteristics_20250520.csv"
outdir = "/media/volume/Projects/DSGELabProject1/DiD_Validation/"

# Test 1
event_code = 'Purch_C10AA07'
outcome_code = 'C10AA07'
today <- format(Sys.Date(), "%Y%m%d")
outdir = file.path(outdir, paste0("ValidationPlots_", outcome_code, "_", today))
if (!dir.exists(outdir)) {dir.create(outdir, recursive = TRUE)}

#### Main
N_THREADS = 10
setDTthreads(N_THREADS) 
# not using all threads to easily run in background

# STEP 1: Data Loading 
covariates = fread(covariate_file)
doctor_ids = fread(doctor_list, header = FALSE)$V1

events = as.data.table(read_parquet(events_file))
event_code_parts = strsplit(event_code, "_")[[1]]
event_source = event_code_parts[1]
event_actual_code = event_code_parts[2]

# Filter events based on the event code
events = events[SOURCE == event_source & startsWith(as.character(CODE), event_actual_code), ]
event_ids = unique(events$PATIENT_ID)

# Load outcomes (N, Ni, and Y for desired medication)
outcomes_cols1 = c("DOCTOR_ID", "YEAR", paste0("N_", outcome_code), paste0("Y_", outcome_code), paste0("first_year_", outcome_code), paste0("last_year_", outcome_code))
outcomes1 = as.data.table(read_parquet(outcomes_file1, col_select = outcomes_cols1))
outcomes_cols2 = c("DOCTOR_ID", "YEAR","N")
outcomes2 = as.data.table(read_parquet(outcomes_file2, col_select = outcomes_cols2))
outcomes = merge(
    outcomes1,
    outcomes2,
    by = c("DOCTOR_ID", "YEAR"),
    all.x = TRUE,
    suffixes = c("", "_drop")
)

# STEP 2: Data Preparation
# Process and merge events and outcomes

events = events[, .(PATIENT_ID, CODE, DATE)]
setnames(events, "PATIENT_ID", "DOCTOR_ID")
# QC: Keep only the first event per DOCTOR_ID, in case multiple codes exist
events = events[order(DOCTOR_ID, DATE)]
events = events[, .SD[1], by = DOCTOR_ID]
# QC: Ensure events are only for doctors in the doctor list
outcomes_filtered = outcomes[DOCTOR_ID %in% doctor_ids]

df_merged = events[outcomes_filtered, on = "DOCTOR_ID", allow.cartesian = TRUE]
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

# Prepare covariates 
covariates[, `:=`(
    SPECIALTY = as.character(INTERPRETATION),
    BIRTH_YEAR = as.numeric(substr(BIRTH_DATE, 1, 4))
)]
covariates[, `:=`(
    BIRTH_DATE = NULL, 
    INTERPRETATION = NULL)
]

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

# Analysis requires only individuals with non-missing Y for all years in the required window
years_required <- (min(df_model$YEAR, na.rm = TRUE)):(max(df_model$YEAR, na.rm = TRUE))
ids_with_all_years <- df_model %>%
    filter(YEAR %in% years_required & !is.na(Y)) %>%
    group_by(DOCTOR_ID) %>%
    summarise(n_years = n_distinct(YEAR)) %>%
    filter(n_years == length(years_required)) %>%
    pull(DOCTOR_ID)

#prepare the model data
df_model <- df_model %>% filter(DOCTOR_ID %in% ids_with_all_years, YEAR %in% years_required)
df_model$ID <- as.integer(factor(df_model$DOCTOR_ID))                       # create a numeric ID variable
df_model$G <- ifelse(is.na(df_model$EVENT_YEAR), 0, df_model$EVENT_YEAR)  # G = group of first treatment year, 0 for never-treated
df_model$T <- df_model$YEAR    

# Calculate number of cases (events) and controls
n_cases <- length(unique(df_model[df_model$EVENT == 1, DOCTOR_ID]))
n_controls <- length(unique(df_model[df_model$EVENT == 0, DOCTOR_ID]))

# ---------------   
# VALIDATION ANALYSIS:  group ATT 
# ---------------

att_gt_res <- att_gt(
    yname = "Y",
    tname = "T",
    idname = "ID",
    gname = "G",
    xformla = ~ BIRTH_YEAR + SEX + SPECIALTY,
    data = df_model,
    est_method = "dr",
    control_group = "notyettreated",
    clustervars = "ID",
    pl = TRUE,
    cores = N_THREADS
)

# use "group" effect (time of event) instead of "dynamic" effect (time from event)
agg_group <- aggte(att_gt_res, type = "group", na.rm = TRUE)
df_group <- data.frame(
    time = agg_group$egt,
    att = agg_group$att.egt,
    se = agg_group$se.egt
)
p_group <- ggplot(df_group, aes(x = time, y = att)) +
    geom_line(color = "#2ca02c") +
    geom_point() +
    geom_errorbar(aes(ymin = att - 1.96 * se, ymax = att + 1.96 * se),width = 0.2, color = "#2ca02c") +
    geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
    labs(
        title = "Effect Comparison across Case Cohorts",
        x = "Event Year (Case Cohort)",
        y = "Change in Prescription Behavior\n(Difference in Difference  ATT)"
    ) +
    theme_minimal()

# and add number of cases and number of prescription by time to the plot
# Calculate number of events per year
events_per_year <- df_model %>%
    filter(EVENT == 1) %>%
    group_by(EVENT_YEAR) %>%
    summarise(n_events = n_distinct(DOCTOR_ID)) %>%
    rename(YEAR = EVENT_YEAR)
p_events <- ggplot(events_per_year, aes(x = YEAR, y = n_events)) +
    geom_bar(stat = "identity", fill = "steelblue") +
    labs(title = "Number of Events per Year",
         x = "Year",
         y = "Number of Events") +
    theme_minimal()
# Calculate total number of prescriptions (sum Ni) per year for all doctors
prescriptions_per_year <- df_model %>%
    group_by(YEAR) %>%
    summarise(total_prescriptions = sum(Ni, na.rm = TRUE))
p_prescriptions <- ggplot(prescriptions_per_year, aes(x = YEAR, y = total_prescriptions)) +
    geom_bar(stat = "identity", fill = "coral") +
    labs(title = sprintf("Number of %s Prescriptions per Year", outcome_code),
         x = "Year",
         y = "Total Prescriptions") +
    theme_minimal()

# Combine plots
combined_plot <- (p_group / p_events / p_prescriptions) +
    plot_annotation(
        title = paste0("Effect of ", event_code, " on ", outcome_code),
        subtitle = paste0("Cases: ", n_cases, ", Controls: ", n_controls)
    )
ggsave(file.path(outdir, paste0("Validation3.png")), combined_plot, width = 8, height = 12)