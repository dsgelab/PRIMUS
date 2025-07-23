#### Info:
# This script takes as input a list of doctor ids (cases + controls) and two datasets Events.csv and Outcomes.csv
# It then performs a difference-in-differences analysis based on the input data

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
setDTthreads(0) # use all available threads

# Load data
covariates = fread(covariate_file)
doctor_ids = fread(doctor_list, header = FALSE)$V1

# Read parquet files and convert to data.table for consistent processing
events = read_parquet(events_file)
events = as.data.table(events)
event_code_parts = strsplit(event_code, "_")[[1]]
event_source = event_code_parts[1]
event_code_prefix = event_code_parts[2]
event_ids = unique(events[SOURCE == event_source & startsWith(as.character(CODE), event_code_prefix), PATIENT_ID])

# CHECK 1: if N of events is less than 500, stop the analysis
cat(paste0("Cases: ", length(event_ids), "\n"))
cat(paste0("Controls: ", length(setdiff(doctor_ids, event_ids)), "\n"))
if (length(event_ids) < 500) {
    cat("Number of events (CHECK 1) is less than 500, SKIP ANALYSIS.\n")
    stop("Number of events (CHECK 1) is less than 500, SKIP ANALYSIS.")
}

outcomes_cols = c("DOCTOR_ID", "MONTH", "YEAR", paste0("N_", outcome_code), paste0("Y_", outcome_code))
outcomes = read_parquet(outcomes_file, col_select = outcomes_cols)
outcomes = as.data.table(outcomes)

# CHECK 2: if doctor has less than 20 prescriptions for the outcome of interest, remove doctor from analysis
prescriptions_per_doctor = outcomes[, .(total_prescriptions = sum(get(paste0("N_", outcome_code)), na.rm = TRUE)), by = DOCTOR_ID]
doctors_to_keep = prescriptions_per_doctor[total_prescriptions >= 20, DOCTOR_ID]

# Apply both conditions: must have event AND ≥20 prescriptions AND be in doctor list
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

# Prepare outcomes for DiD analysis
# No step required 

# prepare events for DiD analysis + merge with outcomes
events = events[, .(PATIENT_ID, CODE, DATE)]
setnames(events, "PATIENT_ID", "DOCTOR_ID")

# Convert to tibble for dplyr operations
df_merged = as_tibble(outcomes) %>%
    left_join(as_tibble(events), by = "DOCTOR_ID") %>%
    filter(DOCTOR_ID %in% doctor_ids) %>%
    mutate(
        DATE = as.Date(DATE),
        EVENT = if_else(!is.na(DATE), 1, 0),
        EVENT_YEAR = if_else(!is.na(DATE), as.numeric(format(DATE, "%Y")), NA_real_),
        EVENT_MONTH = if_else(!is.na(DATE), (as.numeric(format(DATE, "%Y")) - 1998) * 12 + as.numeric(format(DATE, "%m")), NA_real_),
    ) %>%
    select(-DATE)

# Prepare covariates and specialty + merge them in the main dataframe
covariates_new = as_tibble(covariates) %>%
    select(DOCTOR_ID, BIRTH_DATE, SEX, INTERPRETATION) %>%
    mutate(SPECIALTY = as.character(INTERPRETATION)) %>% #currently using interpretation of longest specialty
    mutate(BIRTH_YEAR = as.numeric(substr(BIRTH_DATE, 1, 4))) %>% # date format is YYYY-MM-DD
    select(-BIRTH_DATE, -INTERPRETATION)

df_complete = df_merged %>%
    left_join(covariates_new, by = "DOCTOR_ID") %>%
    mutate(
        AGE = YEAR - BIRTH_YEAR,
        AGE_IN_2023 = 2023 - BIRTH_YEAR,
        AGE_AT_EVENT = if_else(is.na(EVENT_YEAR), NA_real_, EVENT_YEAR - BIRTH_YEAR)
    )

events_after65 = df_complete %>% 
    filter(AGE_AT_EVENT > 65) %>% 
    pull(DOCTOR_ID) %>% 
    unique()

df_complete = df_complete %>% 
    filter(!(DOCTOR_ID %in% events_after65)) %>% # remove people which experiment the event after pension (age 65)
    filter(AGE <= 65) # remove all prescriptions done after pension (age 65)

# DiD analysis model
# - adjusting for age in 2023, sex, specialty + age and year of event
df_model = df_complete %>%
    mutate(
        PERIOD = case_when(
            !is.na(EVENT_MONTH) & MONTH < EVENT_MONTH ~ "BEFORE",
            !is.na(EVENT_MONTH) & MONTH > EVENT_MONTH ~ "AFTER",
            is.na(EVENT_MONTH) ~ NA_character_),
        time_from_event = MONTH - EVENT_MONTH
    ) %>%
    filter(!is.na(PERIOD), time_from_event >= -36, time_from_event <= 36) %>%
    mutate(
        PERIOD = factor(PERIOD, levels = c("BEFORE", "AFTER")), # set BEFORE as reference
        SPECIALTY = factor(SPECIALTY, levels = c("", setdiff(unique(df_complete$SPECIALTY), ""))), # set no specialty as reference
        SEX = factor(SEX, levels = c(1, 2), labels = c("Male", "Female")) # set male as reference
    )

# Create the outcome variable name dynamically
outcome_var = paste0("Y_", outcome_code)
model_formula = as.formula(paste0(outcome_var, " ~ PERIOD + MONTH + AGE_IN_2023 + AGE_AT_EVENT + SEX + SPECIALTY + AGE_IN_2023:PERIOD + AGE_AT_EVENT:PERIOD + SEX:PERIOD + SPECIALTY:PERIOD"))
model = fixest::feols(model_formula, data = df_model, vcov = ~DOCTOR_ID)

options(marginaleffects_parallel = TRUE)
marginal = avg_slopes(model, variables = "PERIOD")

# Save results
effect_size = marginal$estimate
p_value = marginal$p.value
ci_lower = marginal$conf.low
ci_upper = marginal$conf.high
n_cases = length(unique(df_complete$DOCTOR_ID[df_complete$EVENT == 1]))
n_controls = length(unique(df_complete$DOCTOR_ID[df_complete$EVENT == 0]))

cat(paste(event_code, outcome_code, effect_size, p_value, ci_lower, ci_upper, n_cases, n_controls, sep = ","), "\n", file = results_file, append = TRUE)