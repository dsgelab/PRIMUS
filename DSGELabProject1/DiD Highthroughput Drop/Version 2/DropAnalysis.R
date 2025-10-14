#### Info:
# This script takes as input a list of doctor ids (cases + controls) and two datasets Events.csv and Outcomes.csv
# It then performs a difference-in-differences analysis based on the input data

.libPaths("/shared-directory/sd-tools/apps/R/lib/")

#### Libraries:
suppressPackageStartupMessages({
    library(data.table)
    library(dplyr)
    library(tidyr)
    library(lubridate)
    library(arrow)
    library(did)
})

##### Arguments
args = commandArgs(trailingOnly = TRUE)
doctor_list = args[1]
events_file = args[2]
event_code = args[3]
outcomes_file = args[4]
covariates_file = args[5]
outfile = args[6]

# Global Variables
N_THREADS = 10
COLOR_MALE = "blue" 
COLOR_FEMALE = "orange" 

#### Main
setDTthreads(N_THREADS)

# Load data
doctor_ids = fread(doctor_list, header = FALSE)$V1

events = as.data.table(read_parquet(events_file))
event_code_parts = strsplit(event_code, "_")[[1]]
event_source = event_code_parts[1]
event_actual_code = event_code_parts[2]

# Filter events based on the event code
events = events[SOURCE == event_source & startsWith(as.character(CODE), event_actual_code), ]
event_ids = intersect(unique(events$PATIENT_ID), doctor_ids)
control_ids <- setdiff(doctor_ids, event_ids)

outcomes = as.data.table(read_parquet(outcomes_file))
covariates = fread(covariates_file)

# prepare outcomes for DiD analysis
# now done with ProcessOutcomes.py
outcomes = outcomes[DOCTOR_ID %in% doctor_ids,] # QC : only selected doctors 

# prepare events for DiD analysis + merge with outcomes
events = events[, .(PATIENT_ID, CODE, DATE)]
events = events[PATIENT_ID %in% doctor_ids,] # QC : only selected doctors
events$DATE <- as.Date(events$DATE)
events = events[events[, .I[which.min(DATE)], by = .(PATIENT_ID, CODE)]$V1] # only use first event
events = events[, c("PATIENT_ID", "DATE")] %>% rename("DOCTOR_ID" = "PATIENT_ID")
df_merged = left_join(outcomes, events, by = "DOCTOR_ID")
df_merged = df_merged %>%
    mutate(
        EVENT = if_else(!is.na(DATE), 1, 0),
        EVENT_YEAR = if_else(!is.na(DATE), as.numeric(format(DATE, "%Y")), NA_real_),
    ) %>%
    select(-DATE)

# Prepare  covariates and specialty + merge them in the main dataframe
covariates_new = covariates %>%
    select(DOCTOR_ID, BIRTH_DATE, SEX, INTERPRETATION) %>%
    mutate(SPECIALTY = as.character(INTERPRETATION)) %>% #currently using interpretation of longest specialty
    mutate(BIRTH_YEAR = as.numeric(substr(BIRTH_DATE, 1, 4))) %>% # date format is YYYY-MM-DD
    select(-BIRTH_DATE, -INTERPRETATION)
df_complete = merge(df_merged, covariates_new, by = "DOCTOR_ID", how = "left")
df_complete = df_complete %>% 
    mutate(
        AGE = YEAR - BIRTH_YEAR,
        AGE_IN_2023 = 2023 - BIRTH_YEAR,
        AGE_AT_EVENT = if_else(is.na(EVENT_YEAR), NA_real_, EVENT_YEAR - BIRTH_YEAR)
    )
events_after60 = df_complete %>% filter(AGE_AT_EVENT > 60) %>% pull(DOCTOR_ID) %>% unique()
df_complete = df_complete %>% 
    filter(!(DOCTOR_ID %in% events_after60)) %>% # remove people which experiment the event after pension (age 60)
    filter(AGE <= 60) # remove all prescriptions done after pension (age 60)

# ============================================================================
# MAIN
# ============================================================================
# This script analyzes time series data to quantify dips around event time 
# Extracting results using the DiD framework to compare cases and controls & adjust for covariates
# ============================================================================

df_model = df_complete %>%
    mutate(
        PERIOD = case_when(
            !is.na(EVENT_YEAR) & YEAR < EVENT_YEAR ~ "BEFORE",
            !is.na(EVENT_YEAR) & YEAR > EVENT_YEAR ~ "AFTER",
            is.na(EVENT_YEAR) ~ NA_character_),
        time = YEAR - EVENT_YEAR
    ) %>%
    mutate(
        PERIOD = factor(PERIOD, levels = c("BEFORE", "AFTER")), # set BEFORE as reference
        SPECIALTY = factor(SPECIALTY, levels = c("", setdiff(unique(df_complete$SPECIALTY), ""))), # set no specialty as reference
        SEX = factor(SEX, levels = c(1, 2), labels = c("Male", "Female")) # set male as reference
    )

# Analysis requires only individuals with non-missing N for all years in the required window
years_required <- (min(df_model$YEAR, na.rm = TRUE)):(max(df_model$YEAR, na.rm = TRUE))
ids_with_all_years <- df_model %>%
    filter(YEAR %in% years_required & !is.na(N)) %>%
    group_by(DOCTOR_ID) %>%
    summarise(n_years = n_distinct(YEAR)) %>%
    filter(n_years == length(years_required)) %>%
    pull(DOCTOR_ID)

# Step 1: prepare the model data
df_model <- df_model %>% filter(DOCTOR_ID %in% ids_with_all_years, YEAR %in% years_required)
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

att_gt_res <- att_gt(
    yname = "N",
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

agg_dynamic <- aggte(att_gt_res, type = "dynamic", na.rm = TRUE)
results <- data.frame(
    time = agg_dynamic$egt,
    att = agg_dynamic$att.egt,
    se = agg_dynamic$se.egt
)

# Drop ATT and SE at event
effect_at_event <- results$att[results$time == 0]
se_at_event <- results$se[results$time == 0]

# ============================================================================
# 3. EXPORT RESULTS TO CSV
# ============================================================================

# Append summary row to outfile
summary_row <- data.frame(
        event_code = event_code,
        drop = effect_at_event,
        se = se_at_event,
        n_cases = n_cases,
        n_controls = n_controls
)

write.table(
        summary_row,
        file = outfile,
        sep = ",",
        row.names = FALSE,
        col.names = FALSE,
        append = TRUE
)
