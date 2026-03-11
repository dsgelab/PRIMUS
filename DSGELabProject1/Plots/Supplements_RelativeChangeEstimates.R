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
    summarise(BASELINE_MEAN = mean(get(paste0("Y_", code)), na.rm = TRUE), .groups = 'drop')

  # Calculate baseline rate for 2021 specifically
  baseline_2021 <- outcomes %>%
    filter(DOCTOR_ID %in% control_ids & YEAR == 2021) %>%
    summarise(BASELINE_2021 = mean(get(paste0("Y_", code)), na.rm = TRUE)) 

  results_1[[code]] <- data.frame(
    OUTCOME_CODE = code,
    BASELINE_MEAN = baseline_rates$BASELINE_MEAN,
    BASELINE_2021 = baseline_2021$BASELINE_2021
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
    summarise(BASELINE_MEAN = mean(get(paste0("Y_", code)), na.rm = TRUE), .groups = 'drop')

  # Calculate baseline rate for 2021 specifically
  baseline_2021 <- outcomes %>%
    filter(DOCTOR_ID %in% control_ids & YEAR == 2021) %>%
    summarise(BASELINE_2021 = mean(get(paste0("Y_", code)), na.rm = TRUE)) 

  results_2[[code]] <- data.frame(
    OUTCOME_CODE = code,
    YEAR = baseline_rates$YEAR,
    BASELINE_MEAN = baseline_rates$BASELINE_MEAN,
    BASELINE_2021 = baseline_2021$BASELINE_2021
  )
}
# for each code, plot the distribution of baseline rates over the years
baseline_rates_by_year <- do.call(rbind, results_2)

p_baseline_evolution <- ggplot(baseline_rates_by_year, aes(x = YEAR, y = BASELINE_MEAN, color = OUTCOME_CODE, group = OUTCOME_CODE)) +
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
  mutate(
    RELATIVE_CHANGE_MEAN = (ABS_CHANGE + BASELINE_MEAN) / BASELINE_MEAN,
    RELATIVE_CHANGE_2021 = (ABS_CHANGE + BASELINE_2021) / BASELINE_2021
  ) %>%
  arrange(RELATIVE_CHANGE_MEAN)

# save resutls to file
dataset_with_baseline <- dataset_with_baseline %>%
  select(OUTCOME_CODE, BASELINE_MEAN, BASELINE_2021, ABS_CHANGE, RELATIVE_CHANGE_MEAN, RELATIVE_CHANGE_2021, ABS_CHANGE_SE, PVAL_ABS_CHANGE, PVAL_PRE, PVAL_POST)
write_csv(dataset_with_baseline, paste0(outdir, "Supplementary_RelativeChangeEstimates_", DATE, ".csv"))  

# Plot V1 
p1 <- ggplot(dataset_with_baseline, aes(y = reorder(OUTCOME_CODE, RELATIVE_CHANGE_MEAN))) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey", linewidth = 0.8) +
  geom_segment(aes(x = BASELINE_MEAN, xend = BASELINE_MEAN + ABS_CHANGE, yend = reorder(OUTCOME_CODE, RELATIVE_CHANGE_MEAN)), color = "red", linewidth = 0.8) +
  geom_point(aes(x = BASELINE_MEAN, shape = "Baseline"), color = "red", size = 3, stroke = 1.5) +
  geom_point(aes(x = BASELINE_MEAN + ABS_CHANGE, shape = "After Event"), color = "red", size = 4) +
  scale_shape_manual(values = c("Baseline" = 1, "After Event" = 16), name = "") +
  labs(title = "Estimated Change in Average Prescription Rates After Event (Relative to Baseline)",
       x = "Prescription Rate",
       y = "ATC Code") +
  theme_minimal() +
  theme(axis.text.y = element_text(size = 8),legend.position = "bottom")

# Combine all plots into a single figure
ggsave(filename = paste0(outdir, "Supplementary_RelativeChangeEstimates_V1_", DATE, ".png"), plot = p1, width = 12, height = 8)

# Plot V2
# Calculate 95% CI for relative change
dataset_with_baseline <- dataset_with_baseline %>%
  mutate(
    RELATIVE_CHANGE_SE = abs(ABS_CHANGE_SE / BASELINE_MEAN),
    RELATIVE_CHANGE_CI_LOW = RELATIVE_CHANGE_MEAN - 1.96 * RELATIVE_CHANGE_SE,
    RELATIVE_CHANGE_CI_UP = RELATIVE_CHANGE_MEAN + 1.96 * RELATIVE_CHANGE_SE
  )
p2 <- ggplot(dataset_with_baseline, aes(y = reorder(OUTCOME_CODE, RELATIVE_CHANGE_MEAN))) +
  geom_vline(xintercept = 1, linetype = "dashed", color = "grey", linewidth = 0.8) +
  geom_point(aes(x = RELATIVE_CHANGE_MEAN), color = "red", size = 3, fill = "red") +
  geom_errorbarh(aes(xmin = RELATIVE_CHANGE_CI_LOW, xmax = RELATIVE_CHANGE_CI_UP), height = 0.2, color = "red", linewidth = 0.8) +
  geom_text(aes(x = RELATIVE_CHANGE_MEAN, label = paste0(round(RELATIVE_CHANGE_MEAN, 3), "\n[", round(RELATIVE_CHANGE_CI_LOW, 3), ", ", round(RELATIVE_CHANGE_CI_UP, 3), "]")), hjust = -0.1, vjust = 0.5, size = 2.5) +
  labs(title = "Estimated Relative Change in Prescription Rates After Event (95% CI)",
       x = "Relative Change",
       y = "ATC Code") +
  theme_minimal() +
  theme(axis.text.y = element_text(size = 8), legend.position = "bottom")


# Combine all plots into a single figure
ggsave(filename = paste0(outdir, "Supplementary_RelativeChangeEstimates_V2_", DATE, ".png"), plot = p2, width = 12, height = 8)

# Plot V3:
# compare relative changes based on different baselines (mean vs 2021)
# Calculate 95% CI for 2021 relative change
dataset_with_baseline <- dataset_with_baseline %>%
  mutate(
    RELATIVE_CHANGE_2021_SE = abs(ABS_CHANGE_SE / BASELINE_2021),
    RELATIVE_CHANGE_2021_CI_LOW = RELATIVE_CHANGE_2021 - 1.96 * RELATIVE_CHANGE_2021_SE,
    RELATIVE_CHANGE_2021_CI_UP = RELATIVE_CHANGE_2021 + 1.96 * RELATIVE_CHANGE_2021_SE
  )
p3 <- ggplot(dataset_with_baseline, aes(x = RELATIVE_CHANGE_MEAN, y = RELATIVE_CHANGE_2021)) +
  geom_point(color = "darkred", size = 3) +
  geom_errorbarh(aes(xmin = RELATIVE_CHANGE_CI_LOW, xmax = RELATIVE_CHANGE_CI_UP), height = 0.05, color = "darkred", linewidth = 0.6, alpha = 0.7) +
  geom_errorbar(aes(ymin = RELATIVE_CHANGE_2021_CI_LOW, ymax = RELATIVE_CHANGE_2021_CI_UP), width = 0.05, color = "darkred", linewidth = 0.6, alpha = 0.7) +
  geom_text(aes(label = OUTCOME_CODE), size = 2, hjust = -0.2, vjust = -0.2) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "grey", linewidth = 0.8) +
  labs(title = "Relative Change: Mean Baseline vs. 2021 Baseline (95% CI)",
        x = "Relative Change (Mean Baseline)",
        y = "Relative Change (2021 Baseline)") +
  theme_minimal() +
  theme(legend.position = "bottom")

ggsave(filename = paste0(outdir, "Supplementary_RelativeChangeEstimates_V3_", DATE, ".png"), plot = p3, width = 12, height = 8)
