
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
outdir = args[6]

# Global Variables
N_THREADS = 10
COLOR_MALE = "blue" 
COLOR_FEMALE = "orange" 

#### Main
setDTthreads(N_THREADS)

# Load data
doctor_ids = fread(doctor_list, header = FALSE)$V1
events = fread(events_file)
events = events[grepl(paste0("^", event_code), CODE)]

event_ids = intersect(unique(events$PATIENT_ID), doctor_ids)
control_ids <- setdiff(doctor_ids, event_ids)

# CHECK 1 : if N of events is less than 500, stop the analysis
cat(paste0("Cases : ", length(event_ids), "\n"))
cat(paste0("Controls : ", length(control_ids), "\n"))
if (length(event_ids) < 500) {
    stop("Number of events (CHECK 1) is less than 500, SKIP ANALYSIS.")
}

outcomes = fread(outcomes_file)
covariates = fread(covariates_file)

# prepare outcomes for DiD analysis
outcomes = outcomes[outcomes$DOCTOR_ID != outcomes$PATIENT_ID, ] # remove self-prescriptions
outcomes = outcomes[DOCTOR_ID %in% doctor_ids,] # QC : only selected doctors 
outcomes = outcomes[!is.na(CODE) & !is.na(DATE)]
outcomes = outcomes[DATE >= as.Date("1998-01-01")] # QC: remove events before 1998
outcomes[, MONTH := (as.numeric(format(DATE, "%Y")) - 1998) * 12 + as.numeric(format(DATE, "%m"))]
outcomes = outcomes[, .(N = .N), by = .(DOCTOR_ID, MONTH)]
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
    filter(!is.na(PERIOD), time >= -36, time <= 36) %>%
    mutate(
        PERIOD = factor(PERIOD, levels = c("BEFORE", "AFTER")), # set BEFORE as reference
        SPECIALTY = factor(SPECIALTY, levels = c("", setdiff(unique(df_complete$SPECIALTY), ""))), # set no specialty as reference
        SEX = factor(SEX, levels = c(1, 2), labels = c("Male", "Female")) # set male as reference
    )

# Set buffer (in months) for baseline calculation
buffer <- 3

# Calculate baseline
# Consider the period BEFORE the event (using buffer)
baseline_data <- df_model %>% filter(time < -buffer)
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
    subtitle = sprintf("Dip height: %.1f, Time to Recover (TTR): %.1f (months)", height, ttr)
  ) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5))

# Save plot
ggsave(filename = file.path(outdir, "overall_drop_plot.png"), plot = p, width = 8, height = 5, dpi = 300)