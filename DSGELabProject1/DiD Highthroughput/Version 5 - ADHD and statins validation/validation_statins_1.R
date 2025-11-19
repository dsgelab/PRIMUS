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

event_code = 'Purch_C10AA' # all statins available

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

all_outcomes = events$CODE %>% unique()
results_Y_list = list()
for (outcome_code in all_outcomes) {
    # Load outcomes (N, Ni, and Y for desired medication)
    outcomes_cols = c("DOCTOR_ID", "YEAR", paste0("N_", outcome_code), paste0("Y_", outcome_code), paste0("first_year_", outcome_code), paste0("last_year_", outcome_code))
    outcomes = as.data.table(read_parquet(outcomes_file, col_select = outcomes_cols))

    # STEP 2: Data Preparation
    # Process and merge events and outcomes

    events_new = events[, .(PATIENT_ID, CODE, DATE)]
    events_new = events_new[CODE == outcome_code]
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
    agg_dynamic_Y <- aggte(att_gt_res, type = "dynamic", na.rm = TRUE)
    results_Y <- data.frame(
        time = agg_dynamic_Y$egt,
        att = agg_dynamic_Y$att.egt,
        se = agg_dynamic_Y$se.egt,
        outcome = "Y (Medication Ratio)",
        n_cases = n_cases,
        n_controls = n_controls
    )
    results_Y_list[[outcome_code]] = results_Y
}

# Combine all results and add outcome_code column
results_combined <- rbindlist(results_Y_list, idcol = "outcome_code")

# Filter data for plotting
data_plot <- results_combined[time >= -3 & time <= 2]

# Create combined plot with all outcome codes on same panel
p <- ggplot(data_plot, aes(x = time, y = att, color = outcome_code, group = outcome_code)) +
    geom_line(linewidth = 0.8) +
    geom_point(size = 2) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
    labs(
        title = paste0("Effect of Statin use on their Prescription Behavior"),
        subtitle = paste0(
            "Unique cases/controls per outcome:\n",
            paste(sapply(unique(results_combined$outcome_code), function(oc) {
            sprintf("%s (Cases: %d, Controls: %d)\n", 
                oc, 
                unique(results_combined[outcome_code == oc, n_cases]),
                unique(results_combined[outcome_code == oc, n_controls]))
            }), collapse = "")
        ),
        x = "Years from Event",
        y = "Change in Prescription Behavior\n(Difference in Difference ATT)",
        color = "Medication Code"
    ) +
    theme_minimal() +
    theme(
        legend.position = "right",
        legend.text = element_text(size = 8)
    )
ggsave(file.path(outdir, paste0("ValidationPlot1.png")), p, width = 10, height = 6, dpi = 300)
