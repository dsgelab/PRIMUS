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
    library(metafor)
    library(readr)
})

##### Arguments
DATE = "20260129"
dataset_file <- paste0('/media/volume/Projects/DSGELabProject1/DiD_Experiments/DiD_Medications_', DATE, '/Results_', DATE, '/Results_ATC_', DATE, '.csv')
events_file = paste0("/media/volume/Projects/DSGELabProject1/DiD_Experiments/DiD_Medications_", DATE, "/ProcessedEvents_", DATE, "/processed_events.parquet")
outcomes_file = paste0("/media/volume/Projects/DSGELabProject1/DiD_Experiments/DiD_Medications_", DATE, "/ProcessedOutcomes_", DATE, "/processed_outcomes.parquet")
doctor_list = "/media/volume/Projects/DSGELabProject1/doctors_20250424.csv"
covariate_file = "/media/volume/Projects/DSGELabProject1/doctor_characteristics_20250520.csv"
renamed_ATC_file = "/media/volume/Projects/ATC_renamed_codes.csv"
outdir = "/media/volume/Projects/DSGELabProject1/Plots/Supplements/"

dataset <- read_csv(dataset_file, show_col_types = FALSE)
# Filter only codes with at least 300 cases available
dataset <- dataset[dataset$N_CASES >= 300, ]

# STEP 1:
# Apply FDR multiple testing correction
dataset$PVAL_ADJ_FDR <- p.adjust(dataset$PVAL_ABS_CHANGE, method = "bonferroni")
dataset$SIGNIFICANT_CHANGE <- dataset$PVAL_ADJ_FDR < 0.05

# STEP 2:
# Select only robust results, i.e those point / events with:
# A. an average prescription rate before event significantly non-different from controls
# B. an average prescription rate after event significantly different from controls
# Also apply FDR multiple testing correction here
dataset$PVAL_PRE_ADJ_FDR <- p.adjust(dataset$PVAL_PRE, method = "bonferroni")
dataset$PVAL_POST_ADJ_FDR <- p.adjust(dataset$PVAL_POST, method = "bonferroni")    
dataset$SIGNIFICANT_ROBUST <- (dataset$PVAL_PRE_ADJ_FDR >= 0.05) & (dataset$PVAL_POST_ADJ_FDR < 0.05)

# STEP 3:
# Create a combined significance variable with two levels
dataset$SIG_TYPE <- case_when(
  dataset$SIGNIFICANT_CHANGE & dataset$SIGNIFICANT_ROBUST ~ "Significant",
  TRUE ~ "Not Significant"
)

# prepare vectors for validation plots
code_list = dataset %>%
    filter(SIG_TYPE == "Significant") %>%
    pull(OUTCOME_CODE) %>%
    unique()

# Plotting
# Will filter only significant medications
# Then will calculate the baseline prescription rate in controls (average across years, will check if the variability is high or not with an extra plot)
# Finally will calculate the relative change as the absolute change divided by the baseline prescription rate

# Part 1: Calculate baseline rates

# Load data
doctor_ids = fread(doctor_list, header = FALSE)$V1
events = as.data.table(read_parquet(events_file))
events[, CODE := as.character(CODE)]

results_1 <- list()
for (code in code_list) {
  # Filter events based on the event code
  events_new <- events[CODE == code & SOURCE == "Purch"]
  event_ids <- unique(events_new$PATIENT_ID)
  control_ids <- setdiff(doctor_ids, event_ids)

  # Load outcomes
  outcome_cols1 = c("DOCTOR_ID", "YEAR", paste0("Y_", code), paste0("first_year_", code), paste0("last_year_", code))
  outcomes = as.data.table(read_parquet(outcomes_file, col_select = outcome_cols1))
  # 1. Calculate original min and max year across all doctors in the cohort
  original_min_year <- min(outcomes[[paste0("first_year_", code)]], na.rm = TRUE)
  original_max_year <- max(outcomes[[paste0("last_year_", code)]], na.rm = TRUE)
  # 2. Add buffer to min and max year to avoid bias
  BUFFER_YEARS = 1
  buffered_min_year <- original_min_year + BUFFER_YEARS
  buffered_max_year <- original_max_year - BUFFER_YEARS
  # 3. Remove all information outside of buffered range
  outcomes <- outcomes[YEAR >= buffered_min_year & YEAR <= buffered_max_year]

  # Calculate baseline rates in controls (for each year)
  baseline_rates <- outcomes %>%
    filter(DOCTOR_ID %in% control_ids) %>%
    group_by(YEAR) %>%
    summarise(BASELINE_RATE = mean(get(paste0("Y_", code)), na.rm = TRUE), .groups = 'drop')

  # Calculate average baseline rate across years
  avg_baseline_rate <- mean(baseline_rates$BASELINE_RATE, na.rm = TRUE)
  results_1[[code]] <- data.frame(
    OUTCOME_CODE = code,
    BASELINE_RATE = avg_baseline_rate
  )
}
# Combine results into a single data frame
baseline_rates_df <- do.call(rbind, results_1)

# Extra Plot / QC: Distribution of baseline prescription rates over the years

# Load data
doctor_ids = fread(doctor_list, header = FALSE)$V1
events = as.data.table(read_parquet(events_file))
events[, CODE := as.character(CODE)]

results_2 <- list()
for (code in code_list) {
  # Filter events based on the event code
  events_new <- events[CODE == code & SOURCE == "Purch"]
  event_ids <- unique(events_new$PATIENT_ID)
  control_ids <- setdiff(doctor_ids, event_ids)

  # Load outcomes
  outcome_cols1 = c("DOCTOR_ID", "YEAR", paste0("Y_", code), paste0("first_year_", code), paste0("last_year_", code))
  outcomes = as.data.table(read_parquet(outcomes_file, col_select = outcome_cols1))
  # 1. Calculate original min and max year across all doctors in the cohort
  original_min_year <- min(outcomes[[paste0("first_year_", code)]], na.rm = TRUE)
  original_max_year <- max(outcomes[[paste0("last_year_", code)]], na.rm = TRUE)
  # 2. Add buffer to min and max year to avoid bias
  BUFFER_YEARS = 1
  buffered_min_year <- original_min_year + BUFFER_YEARS
  buffered_max_year <- original_max_year - BUFFER_YEARS
  # 3. Remove all information outside of buffered range
  outcomes <- outcomes[YEAR >= buffered_min_year & YEAR <= buffered_max_year]

  # Calculate baseline rates in controls (for each year)
  baseline_rates <- outcomes %>%
    filter(DOCTOR_ID %in% control_ids) %>%
    group_by(YEAR) %>%
    summarise(BASELINE_RATE = mean(get(paste0("Y_", code)), na.rm = TRUE), .groups = 'drop')

  results_2[[code]] <- data.frame(
    OUTCOME_CODE = code,
    YEAR = baseline_rates$YEAR,
    BASELINE_RATE = baseline_rates$BASELINE_RATE
  )
}
# for each code, plot the distribution of baseline rates over the years
baseline_rates_by_year <- do.call(rbind, results_2)

p_baseline_evolution <- ggplot(baseline_rates_by_year, aes(x = YEAR, y = BASELINE_RATE, color = OUTCOME_CODE, group = OUTCOME_CODE)) +
  geom_line() +
  geom_point() +
  labs(title = "Baseline Prescription Rates Evolution Over Years",
       x = "Year",
       y = "Baseline Rate",
       color = "ATC Code") +
  theme_minimal() +
  theme(legend.position = "right")

# save plot
ggsave(filename = paste0(outdir, "Supplementary_BaselineRatesEvolution_", DATE, ".png"), plot = p_baseline_evolution, width = 10, height = 6)

# Part 2: Calculate relative changes 
dataset_with_baseline <- dataset %>%
  filter(OUTCOME_CODE %in% code_list) %>%
  left_join(baseline_rates_df, by = "OUTCOME_CODE")

# Calculate relative change
dataset_with_baseline <- dataset_with_baseline %>%
  mutate(RELATIVE_CHANGE = (ABS_CHANGE / BASELINE_RATE)) %>%
  arrange(RELATIVE_CHANGE)

# save resutls to file
dataset_with_baseline <- dataset_with_baseline %>%
  select(OUTCOME_CODE, BASELINE_RATE, ABS_CHANGE, RELATIVE_CHANGE, ABS_CHANGE_SE, PVAL_ABS_CHANGE, PVAL_PRE, PVAL_POST)
write_csv(dataset_with_baseline, paste0(outdir, "Supplementary_RelativeChangeEstimates_", DATE, ".csv"))  

# Plot: Relative change estimates
p <- ggplot(dataset_with_baseline, aes(y = reorder(OUTCOME_CODE, RELATIVE_CHANGE))) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey", linewidth = 0.8) +
  geom_segment(aes(x = BASELINE_RATE, xend = BASELINE_RATE + ABS_CHANGE, yend = reorder(OUTCOME_CODE, RELATIVE_CHANGE)), color = "red", linewidth = 0.8) +
  geom_point(aes(x = BASELINE_RATE, shape = "Baseline"), color = "red", size = 3, stroke = 1.5) +
  geom_point(aes(x = BASELINE_RATE + ABS_CHANGE, shape = "After Event"), color = "red", size = 4) +
  scale_shape_manual(values = c("Baseline" = 1, "After Event" = 16), name = "") +
  labs(title = "Estimated Change in Average Prescription Rates After Event (Relative to Baseline)",
       x = "Prescription Rate",
       y = "ATC Code") +
  theme_minimal() +
  theme(axis.text.y = element_text(size = 8),legend.position = "bottom")

# Combine all plots into a single figure
ggsave(filename = paste0(outdir, "Supplementary_RelativeChangeEstimates_", DATE, ".png"), plot = p, width = 12, height = 8)
