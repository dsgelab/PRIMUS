#### Info:
# This script takes as input a list of doctor ids (cases + controls) and two datasets Events.csv and Outcomes.csv
# It then performs a difference-in-differences analysis based on the input data

#### Libraries:
library(data.table)
library(dplyr)
library(tidyr)
library(lubridate)
library(fixest)
library(ggplot2)
library(patchwork)

##### Arguments
args = commandArgs(trailingOnly=TRUE)
doctor_list = args[1]
events_file = args[2]
event_code = args[3]
outcomes_file = args[4]
outcome_code = args[5]
covariates_file = args[6]
outdir = args[7]

# Global Variables
N_THREADS = 10
COLOR_MALE = "blue" 
COLOR_FEMALE = "orange" 

# Functions
enrichment_func_outcome <- function(s, df) {
    mean_Y_s = df$mean_Y[df$SPECIALTY == s]
    mean_Y_others = mean(df$mean_Y[df$SPECIALTY != s], na.rm = TRUE)
    ifelse(mean_Y_others == 0, NA, mean_Y_s / mean_Y_others)
}

#### Main
setDTthreads(N_THREADS)

# Load data
doctor_ids = fread(doctor_list, header = FALSE)$V1
events = fread(events_file)
events = events[grepl(paste0("^", event_code), CODE)]
event_ids = intersect(unique(events$PATIENT_ID), doctor_ids)

# CHECK 1 : if N of events is less than 500, stop the analysis
cat(paste0("Cases : ", length(event_ids), "\n"))
cat(paste0("Controls : ", length(doctor_ids)-length(event_ids), "\n"))
if (length(event_ids) < 500) {
    stop("Number of events (CHECK 1) is less than 500, SKIP ANALYSIS.")
}

outcomes = fread(outcomes_file)
covariates = fread(covariates_file)

# CHECK 2 : if doctor has less than 20 prescriptions for the outcome of interest, remove doctor from analysis
prescriptions_per_doctor <- outcomes[grepl(paste0("^", outcome_code), CODE), .N, by = DOCTOR_ID]
write.csv(prescriptions_per_doctor, file = file.path(outdir, "Outcomes.csv"), row.names = FALSE)
doctors_to_keep <- prescriptions_per_doctor[N >= 20, DOCTOR_ID]
event_ids <- intersect(intersect(unique(events$PATIENT_ID), doctors_to_keep), doctor_ids)
control_ids <- setdiff(intersect(doctor_ids, doctors_to_keep), event_ids)

# CHECK 1 (again) : if N of events is less than 500, stop the analysis
cat(paste0("Cases, with at least 20 prescriptions of outcome: ", length(event_ids), "\n"))
cat(paste0("Controls, with at least 20 prescriptions of outcome: ", length(control_ids), "\n"))
if (length(event_ids) < 500) {
    stop("Number of events (post CHECK 2) is less than 500, SKIP ANALYSIS.")
}
doctor_ids = c(event_ids, control_ids) 

# prepare outcomes for DiD analysis
outcomes = outcomes[outcomes$DOCTOR_ID != outcomes$PATIENT_ID, ] # remove self-prescriptions
outcomes = outcomes[DOCTOR_ID %in% doctor_ids,] # QC : only selected doctors 
outcomes = outcomes[!is.na(CODE) & !is.na(DATE)]
outcomes = outcomes[DATE >= as.Date("1998-01-01")] # QC: remove events before 1998
outcomes[, MONTH := (as.numeric(format(DATE, "%Y")) - 1998) * 12 + as.numeric(format(DATE, "%m"))]
outcomes = outcomes[, .(
    Ni = sum(grepl(paste0("^", outcome_code), CODE)),
    N = .N
), by = .(DOCTOR_ID, MONTH)]
outcomes[, Y := fifelse(N == 0, NA_real_, Ni / N)]
outcomes[, YEAR := 1998 + (MONTH - 1) %/% 12]

# prepare events for DiD analysis + merge with outcomes
events = events[, .(PATIENT_ID, CODE, DATE)]
events = events[PATIENT_ID %in% doctor_ids,] # QC : only selected doctors
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

# exclude events which happened before the first prescription of the outcome / or the last one
n_before = length(unique(df_merged$DOCTOR_ID))
df_merged = as.data.table(df_merged)
df_merged[, `:=`(
    first_Y_month = min(MONTH[!is.na(Y)], na.rm = TRUE),
    last_Y_month = max(MONTH[!is.na(Y)], na.rm = TRUE)
), by = DOCTOR_ID]
df_merged = df_merged[
    is.na(EVENT_MONTH) | (EVENT_MONTH >= first_Y_month & EVENT_MONTH <= last_Y_month)]
removed_ids = unique(df_merged[!(is.na(EVENT_MONTH) | (EVENT_MONTH >= first_Y_month & EVENT_MONTH <= last_Y_month)), DOCTOR_ID])
df_merged = df_merged[!(DOCTOR_ID %in% removed_ids)]
n_after = length(unique(df_merged$DOCTOR_ID))
cat(sprintf("Removed %d doctors with event outside prescription bounds\n", n_before - n_after))

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
events_after65 = df_complete %>% filter(AGE_AT_EVENT > 65) %>% pull(DOCTOR_ID) %>% unique()
df_complete = df_complete %>% 
    filter(!(DOCTOR_ID %in% events_after65)) %>% # remove people which experiment the event after pension (age 65)
    filter(AGE <= 65) # remove all prescriptions done after pension (age 65)

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
  ttr <- which(mean_N >= baseline)[1]
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
    filter(!is.na(PERIOD), time >= -36, time <= 36) %>%
    mutate(
        PERIOD = factor(PERIOD, levels = c("BEFORE", "AFTER")), # set BEFORE as reference
        SPECIALTY = factor(SPECIALTY, levels = c("", setdiff(unique(df_complete$SPECIALTY), ""))), # set no specialty as reference
        SEX = factor(SEX, levels = c(1, 2), labels = c("Male", "Female")) # set male as reference
    )

# Set buffer (in months) for baseline calculation
buffer <- 12 # 1 year

# Calculate baseline
baseline_data <- df_model %>% filter(time < -buffer)
baseline <- mean(baseline_data$N, na.rm = TRUE)

# Calculate height (baseline - minimum)
event_period_data <- df_model %>% filter(time >= -buffer, time <= buffer)
avg_N_by_time <- event_period_data %>%
  group_by(time) %>%
  summarise(mean_N = mean(N, na.rm = TRUE)) %>%
  ungroup()

# Extract the minimum value
minimum_value <- min(avg_N_by_time$mean_N, na.rm = TRUE)
height <- baseline - minimum_value

# Calculate width using recovery width (FWB) formula
ttr <- calculate_ttr(avg_N_by_time, baseline)

# ============================================================================
# 3. EXPORT RESULTS TO CSV
# ============================================================================

results_df <- data.frame(
  metric = c("baseline", "minimum", "height", "ttr"),
  value = c(baseline, minimum_value, height, ttr)
)
write.csv(results_df, file = file.path(outdir, "dip_analysis_results.csv"), row.names = FALSE)

# ============================================================================
# 4. VISUALIZATION WITH GGPLOT2
# ============================================================================

# Main time series plot (average N over time)
avg_N_by_time <- df_model %>%
  group_by(time) %>%
  summarise(mean_N = mean(N, na.rm = TRUE)) %>%
  ungroup()

p <- ggplot(avg_N_by_time, aes(x = time, y = mean_N)) +
  geom_line(color = "blue", size = 0.8) +
  geom_vline(xintercept = 0, color = "black", size = 0.8) +
  geom_hline(yintercept = baseline, linetype = "dashed", color = "darkgray", size = 1) +
  geom_segment(
    aes(
      x = 0,
      xend = ttr,
      y = minimum_value,
      yend = minimum_value
    ),
    color = "orange", size = 1.2
  ) +
  labs(
    title = "Analysis of Overall Drop in Total Prescriptions",
    x = "Time (months from event)",
    y = "Mean N",
    subtitle = sprintf("Dip height: %d, Time to Recover (TTR): %d (months)", as.integer(height), as.integer(ttr))
  ) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5))

# Save plot
ggsave(filename = file.path(outdir, "overall_drop_plot.png"), plot = p, width = 8, height = 5, dpi = 300)