#### Info:
# This script takes as input a list of doctor ids (cases + controls) and two datasets (parquet files):
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
    library(did)
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
renamed_ATC_file = "/media/volume/Projects/ATC_renamed_codes.csv"

#### Extra checks

#### Main
N_THREADS = 10
setDTthreads(N_THREADS) 
# not using all threads to easily run in background

# Initialize timing

# STEP 1: Data Loading 
# Read parquet and CSV files, keep all as data.table throughout analysis
covariates = fread(covariate_file)
doctor_ids = fread(doctor_list, header = FALSE)$V1
renamed_ATC = fread(renamed_ATC_file)

# Load events
events = as.data.table(read_parquet(events_file))
events[, CODE := as.character(CODE)]

event_code_parts = strsplit(event_code, "_")[[1]]
event_source = event_code_parts[1]
event_actual_code = event_code_parts[2]

# Filter events based on the event code
# If the code is an old code that have been modified, exit analysis
if (event_actual_code %in% renamed_ATC$ATC_OLD) {
    cat(paste0("Event code ", event_actual_code, " is an old code. Exiting analysis.\n"))
    quit(status = 0)
}
# If input code is a new code, keep as is and rename other codes to the new one
if (event_actual_code %in% renamed_ATC$ATC_NEW) {
    old_codes = renamed_ATC[ATC_NEW == event_actual_code, ATC_OLD]
    events[CODE %in% old_codes, CODE := event_actual_code]
    cat(paste0("Event code ", event_actual_code, " is a new code. Renaming other codes ", paste(old_codes, collapse = ", "), " to the new one.\n"))
}
events <- events[startsWith(CODE, event_actual_code)]
event_ids <- unique(events$PATIENT_ID)

# Load outcomes with specific columns only
# check if outcome code is a new code that has been renamed, if so load also old codes, rename columns and merge them
if (outcome_code %in% renamed_ATC$ATC_NEW) {
    outcome_cols1 = c("DOCTOR_ID", "YEAR", paste0("N_", outcome_code), paste0("Y_", outcome_code), paste0("first_year_", outcome_code), paste0("last_year_", outcome_code))
    outcomes = as.data.table(read_parquet(outcomes_file, col_select = outcome_cols1))

    old_codes = unique(renamed_ATC[ATC_NEW == outcome_code, ATC_OLD])
    # Loop through each old code and stack them
    for(old_code in old_codes) {
        outcome_cols2 = c("DOCTOR_ID", "YEAR", paste0("N_", old_code), paste0("Y_", old_code), paste0("first_year_", old_code), paste0("last_year_", old_code))
        outcomes2 = as.data.table(read_parquet(outcomes_file, col_select = outcome_cols2))     
        setnames(outcomes2, 
            old = c(paste0("N_", old_code), paste0("Y_", old_code), paste0("first_year_", old_code), paste0("last_year_", old_code)),
            new = c(paste0("N_", outcome_code), paste0("Y_", outcome_code), paste0("first_year_", outcome_code), paste0("last_year_", outcome_code)))     
        outcomes = rbind(outcomes, outcomes2)
    }
} else {
    outcomes_cols = c("DOCTOR_ID", "YEAR", paste0("N_", outcome_code), paste0("Y_", outcome_code), paste0("first_year_", outcome_code), paste0("last_year_", outcome_code))
    outcomes = as.data.table(read_parquet(outcomes_file, col_select = outcomes_cols))
}

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
df_merged <- df_merged[
    YEAR >= buffered_min_year & YEAR <= buffered_max_year
]
# 3. Exclude events which happened before the first prescription of the outcome / or after the last one (using buffered range)
df_merged <- df_merged[
    is.na(EVENT_YEAR) | (EVENT_YEAR >= buffered_min_year & EVENT_YEAR <= buffered_max_year)
]

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

# Filter out events after 60 and prescriptions after 60
events_after60 = df_complete[AGE_AT_EVENT > 60 & !is.na(AGE_AT_EVENT), unique(DOCTOR_ID)]
df_complete = df_complete[!(DOCTOR_ID %in% events_after60) & AGE <= 60]

# STEP 3: Model Data Preparation
df_model <- as.data.table(df_complete)[
    , `:=`(
        SPECIALTY = factor(SPECIALTY, levels = c("", setdiff(unique(df_complete$SPECIALTY), ""))),
        SEX = factor(SEX, levels = c(1, 2), labels = c("Male", "Female")),
        Y = get(paste0("Y_", outcome_code)),
        Ni = get(paste0("N_", outcome_code))
    )
]

# Replace missing Y values with 0s 
# (instead of removing doctors with gaps in their trajectory)
df_model[is.na(Y), Y := 0]

# Step 1: prepare the model data
df_model$ID <- as.integer(factor(df_model$DOCTOR_ID))                       # create a numeric ID variable
df_model$G <- ifelse(is.na(df_model$EVENT_YEAR), 0, df_model$EVENT_YEAR)  # G = group of first treatment year, 0 for never-treated
df_model$T <- df_model$YEAR    

# Calculate number of cases (events) and controls
n_cases <- length(unique(df_model[df_model$EVENT == 1, DOCTOR_ID]))
n_controls <- length(unique(df_model[df_model$EVENT == 0, DOCTOR_ID]))

cat(paste0("Cases: ", n_cases, "\n"))
cat(paste0("Controls: ", n_controls, "\n"))

# For cases, count number of unique DOCTOR_IDs per EVENT_YEAR (i.e., number of events per year)
events_per_year <- df_model[df_model$EVENT == 1, .(N = uniqueN(DOCTOR_ID)), by = EVENT_YEAR][order(EVENT_YEAR)]
events_year_str <- paste0(events_per_year$EVENT_YEAR, ":", events_per_year$N, collapse = ", ")
cat("Events per year: [", events_year_str, "]\n")

# STEP 4: Model Estimation

# Staggered DiD Analysis: 
# using the did package developed by Callaway and Sant'Anna
# ATT adjusted for age, sex and specialty (calendar time and age at event will be automatically handled by the model)

att_gt_res <- att_gt(
    yname = "Y",
    tname = "T",
    idname = "ID",
    gname = "G",
    xformla = ~ BIRTH_YEAR + SEX + SPECIALTY,
    data = df_model,
    est_method = "dr",                  # doubly robust (for covariate adj.)
    control_group = "notyettreated",     # use not-yet-treated as control group
    clustervars = "ID",
    pl = TRUE,                           # parallel processing
    cores = N_THREADS
)

# STEP 5: Save Results
agg_dynamic <- aggte(att_gt_res, type = "dynamic", na.rm = TRUE)
results <- data.frame(
    time = agg_dynamic$egt,
    att = agg_dynamic$att.egt,
    se = agg_dynamic$se.egt
)

# Average effect and SE before event (-3,-2,-1)
before_idx <- results$time %in% c(-3, -2, -1)
avg_effect_before <- mean(results$att[before_idx], na.rm = TRUE)
avg_se_before <- sqrt(mean(results$se[before_idx]^2, na.rm = TRUE))

# Average effect and SE at and after event (0,1,2)
after_idx <- results$time %in% c(0, 1, 2)
avg_effect_after <- mean(results$att[after_idx], na.rm = TRUE)
avg_se_after <- sqrt(mean(results$se[after_idx]^2, na.rm = TRUE))

n_cases <- length(unique(df_complete[EVENT == 1, DOCTOR_ID]))
n_controls <- length(unique(df_complete[EVENT == 0, DOCTOR_ID]))

# Prepare output line
output <- c(
    event_code,
    outcome_code,
    avg_effect_before,
    avg_se_before,
    avg_effect_after,
    avg_se_after,
    n_cases,
    n_controls
)

cat(paste(output, collapse = ","), "\n", file = results_file, append = TRUE)
