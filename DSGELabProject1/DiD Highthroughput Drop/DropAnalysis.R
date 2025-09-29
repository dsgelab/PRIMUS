#### Info:
# This script takes as input a list of doctor ids (cases + controls) and two datasets Events.csv and Outcomes.csv
# It then performs a difference-in-differences analysis based on the input data

#### Libraries:
suppressPackageStartupMessages({
    library(data.table)
    library(dplyr)
    library(tidyr)
    library(lubridate)
    library(fixest)
    library(ggplot2)
    library(patchwork)
    library(arrow)
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

# CHECK 1 : if N of events is less than 500, stop the analysis
cat(paste0("Cases : ", length(event_ids), "\n"))
cat(paste0("Controls : ", length(control_ids), "\n"))
if (length(event_ids) < 500) {
    stop("Number of events (CHECK 1) is less than 500, SKIP ANALYSIS.")
}

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
        EVENT_MONTH = if_else(!is.na(DATE), (as.numeric(format(DATE, "%Y")) - 1998) * 12 + as.numeric(format(DATE, "%m")), NA_real_),
    ) %>%
    select(-DATE)

# Prepare  covariates and specialty + merge them in the main dataframe
covariates_new = covariates %>%
    select(DOCTOR_ID, BIRTH_DATE, SEX, INTERPRETATION) %>%
    mutate(SPECIALTY = as.character(INTERPRETATION)) %>% #currently using interpretation of longest specialty
    mutate(BIRTH_YEAR = as.numeric(substr(BIRTH_DATE, 1, 4))) %>% # date format is YYYY-MM-DD
    select(-BIRTH_DATE, -INTERPRETATION)
df_complete = merge(df_merged, covariates_new, by = "DOCTOR_ID", how = "left") %>% as_tibble()
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
# This script analyzes time series data to quantify dips around event time (0)
# Extracting 2 quantities depth of the dip and width of the dip
# ============================================================================

# ============================================================================
# 1. TIME TO RECOVER (TTR) CALCULATION FUNCTION
# This function calculates the time required to return to baseline levels after a dip
# ============================================================================

calculate_ttr <- function(mean_N, baseline) {
  ttr <- which(mean_N >= baseline)[1] # first cell in (post event) ordered vector which equals or exceeds baseline
  if (is.na(ttr)) {
    return(-1)
  } else {
    return(ttr) 
  }
}

# ============================================================================
# 2. ANALYZE DATA
# ============================================================================

df_model = df_complete %>%
    mutate(
        PERIOD = case_when(
            !is.na(EVENT_MONTH) & MONTH < EVENT_MONTH ~ "BEFORE",
            !is.na(EVENT_MONTH) & MONTH > EVENT_MONTH ~ "AFTER",
            is.na(EVENT_MONTH) ~ NA_character_),
        time = MONTH - EVENT_MONTH
    ) %>%
    mutate(
        PERIOD = factor(PERIOD, levels = c("BEFORE", "AFTER")), # set BEFORE as reference
        SPECIALTY = factor(SPECIALTY, levels = c("", setdiff(unique(df_complete$SPECIALTY), ""))), # set no specialty as reference
        SEX = factor(SEX, levels = c(1, 2), labels = c("Male", "Female")) # set male as reference
    )

# Set buffer (in months) for baseline calculation
buffer <- 3

# Calculate baseline
# Consider the period BEFORE the event, only focus on 3 years - buffer
baseline_data <- df_model %>% filter(time >= -36, time < -buffer)
baseline <- mean(baseline_data$N, na.rm = TRUE)

# Calculate height drop (baseline - minimum)
# Consider the period around the event (using buffer)
event_period_data <- df_model %>% filter(time >= -buffer, time <= buffer)
avg_N_by_time <- event_period_data %>%
  group_by(time) %>%
  summarise(mean_N = mean(N, na.rm = TRUE)) %>%
  ungroup()
minimum_value <- min(avg_N_by_time$mean_N, na.rm = TRUE)
height <- baseline - minimum_value

# Calculate width of drop using Time to Recovery (TTR) formula
# Consider the period AFTER the event (not using buffer)
after_event_data <- avg_N_by_time %>% filter(time > 0)
ttr <- calculate_ttr(after_event_data$mean_N, baseline)

# ============================================================================
# Additional metrics by age at event (quartiles)
# ============================================================================

age_event <- df_model %>% filter(!is.na(AGE_AT_EVENT)) %>% pull(AGE_AT_EVENT)
quartiles <- quantile(age_event, probs = c(0, 0.25, 0.5, 0.75, 1), na.rm = TRUE)

ttr_age_q <- c()
for (i in 1:4) {
    age_min <- quartiles[i]
    age_max <- quartiles[i+1]
    age_data <- df_model %>% filter(AGE_AT_EVENT >= age_min, AGE_AT_EVENT < age_max)
    
    # Calculate baseline for age group
    baseline_age_data <- age_data %>% filter(time >= -36, time < -buffer)
    baseline_age <- mean(baseline_age_data$N, na.rm = TRUE)
    
    # Calculate TTR for age group
    after_event_age <- age_data %>% filter(time > 0) %>%
        group_by(time) %>%
        summarise(mean_N = mean(N, na.rm = TRUE)) %>%
        ungroup()
    ttr_age <- calculate_ttr(after_event_age$mean_N, baseline_age)
    ttr_age_q[i] <- ttr_age
}

# ============================================================================
# 3. EXPORT RESULTS TO CSV
# ============================================================================

# Append summary row to outfile
summary_row <- data.frame(
        event_code = event_code,
        baseline = baseline,
        drop = height,
        ttr = ttr,
        ttr_age_q1 = ttr_age_q[1],
        ttr_age_q2 = ttr_age_q[2],
        ttr_age_q3 = ttr_age_q[3],
        ttr_age_q4 = ttr_age_q[4],
        n_cases = length(event_ids),
        n_controls = length(control_ids)
)

write.table(
        summary_row,
        file = outfile,
        sep = ",",
        row.names = FALSE,
        col.names = FALSE,
        append = TRUE
)
