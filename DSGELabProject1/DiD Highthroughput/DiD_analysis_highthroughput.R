#### Info:

# This script takes as input a list of doctor ids (cases + controls) and two datasets Events.csv and Outcomes.csv
# It then performs a difference-in-differences analysis based on the input data
# The script is designed to be run in a high-throughput scenario, marginal effects are calculated using the marginaleffects package (slowest part of the script)

# If completed succesfully, average speed is around 1 minute per (event, outcome) pair

# Available sections currently commented out:
# - Optional script speed checks

.libPaths("/shared-directory/sd-tools/apps/R/lib/")

#### Libraries:
suppressPackageStartupMessages({
    library(data.table)
    library(arrow)
    library(dplyr)
    library(tidyr)
    library(lubridate)
    library(fixest)
    library(marginaleffects)
    library(ggplot2)
    library(patchwork)
})

##### Arguments
args = commandArgs(trailingOnly = TRUE)
events_file = args[1]
outcomes_file = args[2]
event_code = args[3]
outcome_code = args[4]
doctor_list = args[5]
covariate_file = args[6]
results_file = args[7]  

#### Main
setDTthreads(10) 
# not using all threads to easily run in background

# Initialize timing
start_time <- Sys.time()
step_times <- list()

cat("Starting DiD analysis...\n")

# STEP 1: Data Loading 
# Read parquet and CSV files, keep all as data.table throughout analysis
step_start <- Sys.time()
covariates = fread(covariate_file)
doctor_ids = fread(doctor_list, header = FALSE)$V1

events = as.data.table(read_parquet(events_file))
event_code_parts = strsplit(event_code, "_")[[1]]
event_source = event_code_parts[1]
event_actual_code = event_code_parts[2]
# Filter events based on the event code
events = events[SOURCE == event_source & startsWith(as.character(CODE), event_actual_code), ]
event_ids = unique(events$PATIENT_ID)

# step_times[["data_loading"]] <- difftime(Sys.time(), step_start, units = "secs")
# cat(paste0("Step 1 - Data Loading + Event Filtering: ", round(step_times[["data_loading"]], 2), " seconds\n"))

# STEP 2: Analysis Pre-Checks
# step_start <- Sys.time()

# CHECK 1: if N of events is less than 500, stop the analysis
cat(paste0("Cases: ", length(event_ids), "\n"))
cat(paste0("Controls: ", length(setdiff(doctor_ids, event_ids)), "\n"))
if (length(event_ids) < 500) {
    cat("Number of events (CHECK 1) is less than 500, SKIP ANALYSIS.\n")
    stop("Number of events (CHECK 1) is less than 500, SKIP ANALYSIS.")
}

# Load outcomes with specific columns only
outcomes_cols = c("DOCTOR_ID", "MONTH", "YEAR", paste0("N_", outcome_code), paste0("Y_", outcome_code))
outcomes = as.data.table(read_parquet(outcomes_file, col_select = outcomes_cols))

# CHECK 2: only keep doctors with at least 20 prescriptions of the outcome
prescriptions_per_doctor = outcomes[, .(total_prescriptions = sum(get(paste0("N_", outcome_code)), na.rm = TRUE)), by = DOCTOR_ID]
doctors_to_keep = prescriptions_per_doctor[total_prescriptions >= 20, DOCTOR_ID]
event_ids = intersect(intersect(event_ids, doctors_to_keep), doctor_ids)
control_ids = setdiff(intersect(doctor_ids, doctors_to_keep), event_ids)

# CHECK 1 (again): if N of events is less than 500, stop the analysis
cat(paste0("Cases, with at least 20 prescriptions of outcome: ", length(event_ids), "\n"))
cat(paste0("Controls, with at least 20 prescriptions of outcome: ", length(control_ids), "\n"))
if (length(event_ids) < 500) {
    cat("Number of events (post CHECK 2) is less than 500, SKIP ANALYSIS.\n")
    stop("Number of events (post CHECK 2) is less than 500, SKIP ANALYSIS.")
}

doctor_ids = c(event_ids, control_ids)

# step_times[["filtering"]] <- difftime(Sys.time(), step_start, units = "secs")
# cat(paste0("Step 2 - Case & Control checks: ", round(step_times[["filtering"]], 2), " seconds\n"))

# STEP 3: Data Preparation
step_start <- Sys.time()

# Process and merge events and outcomes
events = events[, .(PATIENT_ID, CODE, DATE)]
setnames(events, "PATIENT_ID", "DOCTOR_ID")
# Keep only the first event per DOCTOR_ID, in case multiple codes exist
events = events[order(DOCTOR_ID, DATE)]
events = events[, .SD[1], by = DOCTOR_ID]

outcomes_filtered = outcomes[DOCTOR_ID %in% doctor_ids]

df_merged = events[outcomes_filtered, on = "DOCTOR_ID", allow.cartesian = TRUE]
df_merged[, DATE := as.Date(DATE)]
df_merged[, EVENT := ifelse(!is.na(DATE), 1, 0)]
df_merged[, EVENT_YEAR := ifelse(!is.na(DATE), as.numeric(format(DATE, "%Y")), NA_real_)]
df_merged[, EVENT_MONTH := ifelse(!is.na(DATE), (as.numeric(format(DATE, "%Y")) - 1998) * 12 + as.numeric(format(DATE, "%m")), NA_real_)]
df_merged[, DATE := NULL]

# Prepare covariates 
covariates[, `:=`(
    SPECIALTY = as.character(INTERPRETATION),
    BIRTH_YEAR = as.numeric(substr(BIRTH_DATE, 1, 4))
)]
covariates[, `:=`(BIRTH_DATE = NULL, INTERPRETATION = NULL)]

# Merge covariates
df_complete = covariates[df_merged, on = "DOCTOR_ID"]
df_complete[, `:=`(
    AGE = YEAR - BIRTH_YEAR,
    AGE_IN_2023 = 2023 - BIRTH_YEAR,
    AGE_AT_EVENT = fifelse(is.na(EVENT_YEAR), NA_real_, EVENT_YEAR - BIRTH_YEAR)
)]

# Filter out events after 65 and prescriptions after 65
events_after65 = df_complete[AGE_AT_EVENT > 65 & !is.na(AGE_AT_EVENT), unique(DOCTOR_ID)]
df_complete = df_complete[!(DOCTOR_ID %in% events_after65) & AGE <= 65]

# step_times[["data_prep"]] <- difftime(Sys.time(), step_start, units = "secs")
# cat(paste0("Step 3 - Data Preparation: ", round(step_times[["data_prep"]], 2), " seconds\n"))

# STEP 4: Model Data Preparation
# step_start <- Sys.time()

df_complete[, `:=`(
    PERIOD = fcase(
        !is.na(EVENT_MONTH) & MONTH < EVENT_MONTH, "BEFORE",
        !is.na(EVENT_MONTH) & MONTH > EVENT_MONTH, "AFTER",
        default = NA_character_
    ),
    time_from_event = MONTH - EVENT_MONTH
)]
df_model = df_complete[!is.na(PERIOD) & time_from_event >= -36 & time_from_event <= 36]
df_model[, `:=`(
    PERIOD = factor(PERIOD, levels = c("BEFORE", "AFTER")),
    SPECIALTY = factor(SPECIALTY, levels = c("", setdiff(unique(df_complete$SPECIALTY), ""))),
    SEX = factor(SEX, levels = c(1, 2), labels = c("Male", "Female"))
)]

# step_times[["model_prep"]] <- difftime(Sys.time(), step_start, units = "secs")
# cat(paste0("Step 4 - Model Data Preparation: ", round(step_times[["model_prep"]], 2), " seconds\n"))

# STEP 5: Model Estimation
# step_start <- Sys.time()

# Convert to data.frame for fixest
df_model_df = as.data.frame(df_model)
outcome_var = paste0("Y_", outcome_code)
model_formula = as.formula(paste0(outcome_var, " ~ PERIOD + MONTH + AGE_IN_2023 + AGE_AT_EVENT + SEX + SPECIALTY + AGE_IN_2023:PERIOD + AGE_AT_EVENT:PERIOD + SEX:PERIOD + SPECIALTY:PERIOD"))
model = fixest::feols(model_formula, data = df_model_df, vcov = ~DOCTOR_ID)

#step_times[["model_estimation"]] <- difftime(Sys.time(), step_start, units = "secs")
#cat(paste0("Step 5 - Model Estimation: ", round(step_times[["model_estimation"]], 2), " seconds\n"))

# STEP 6: Marginal Effects Calculation 
# step_start <- Sys.time()

options(marginaleffects_parallel = TRUE)
marginal_pkg = avg_slopes(model, variables = "PERIOD")

effect_size = marginal_pkg$estimate
p_value = marginal_pkg$p.value
ci_lower = marginal_pkg$conf.low
ci_upper = marginal_pkg$conf.high

# step_times[["pkg_marginal"]] <- difftime(Sys.time(), step_start, units = "secs")
# cat(paste0("Step 6 - Marginal Effects Calculation: ", round(step_times[["pkg_marginal"]], 2), " seconds\n"))

# STEP 7: Save Results
#step_start <- Sys.time()

n_cases = length(unique(df_complete[EVENT == 1, DOCTOR_ID]))
n_controls = length(unique(df_complete[EVENT == 0, DOCTOR_ID]))

cat(paste(event_code, outcome_code, effect_size, p_value, ci_lower, ci_upper, n_cases, n_controls, sep = ","), "\n", file = results_file, append = TRUE)

# step_times[["save_results"]] <- difftime(Sys.time(), step_start, units = "secs")
# cat(paste0("Step 7 - Save Results: ", round(step_times[["save_results"]], 2), " seconds\n"))

# # FINAL TIMING SUMMARY
# total_time <- difftime(Sys.time(), start_time, units = "secs")
# cat("\n=== TIMING SUMMARY ===\n")
# cat(paste0("Total execution time: ", round(total_time, 2), " seconds\n"))
# cat("Step-by-step breakdown:\n")
# for (step_name in names(step_times)) {
#     pct = round(100 * as.numeric(step_times[[step_name]]) / as.numeric(total_time), 1)
#     cat(paste0("  ", step_name, ": ", round(step_times[[step_name]], 2), "s (", pct, "%)\n"))
# }
# cat("=====================\n")