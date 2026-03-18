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
DATE = "20260316"
dataset_file <- paste0('/media/volume/Projects/DSGELabProject1/DiD_Experiments/DiD_Medications_', DATE, '/Results_', DATE, '/Results_ATC_', DATE, '.csv')
events_file = paste0("/media/volume/Projects/DSGELabProject1/DiD_Experiments/DiD_Medications_", DATE, "/ProcessedEvents_", DATE, "/processed_events.parquet")
outcomes_file = paste0("/media/volume/Projects/DSGELabProject1/DiD_Experiments/DiD_Medications_", DATE, "/ProcessedOutcomes_", DATE, "/processed_outcomes.parquet")
doctor_list = "/media/volume/Projects/DSGELabProject1/doctors_20250424.csv"
covariate_file = "/media/volume/Projects/DSGELabProject1/doctor_characteristics_20250520.csv"
renamed_ATC_file = "/media/volume/Projects/ATC_renamed_codes.csv"
outdir = "/media/volume/Projects/DSGELabProject1/Plots/Results_20260316/"
if (!dir.exists(outdir)) dir.create(outdir, recursive = TRUE)

##### Main
dataset <- read_csv(dataset_file, show_col_types = FALSE)

# Filter only codes with at least 300 cases available
dataset <- dataset[dataset$N_CASES >= 300, ]

# Apply multiple test correction
dataset$PVAL_ADJ <- p.adjust(dataset$PVAL_ABS_CHANGE, method = "bonferroni")
dataset$SIGNIFICANT_CHANGE <- dataset$PVAL_ADJ < 0.05

# Apply correction also to the pre and post event p-values
dataset$PVAL_PRE_ADJ <- p.adjust(dataset$PVAL_PRE, method = "bonferroni")
dataset$PVAL_POST_ADJ <- p.adjust(dataset$PVAL_POST, method = "bonferroni")    

# Create a significance variable with two levels
dataset$SIG_TYPE <- case_when(
  dataset$SIGNIFICANT_CHANGE ~ "Significant",
  TRUE ~ "Not Significant"
)

# Extract list of significant medications for plots
code_list = dataset %>%
    filter(SIG_TYPE == "Significant") %>%
    pull(OUTCOME_CODE) %>%
    unique()

#-----------------------------------------------
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

  results_1[[code]] <- data.frame(
    OUTCOME_CODE = code,
    BASELINE_MEAN = baseline_rates$BASELINE_MEAN
  )
}

# Combine results into a single data frame
baseline_rates_df <- do.call(rbind, results_1)

# -----------------------------------------------
# Extra Plot / QC: Evolution of baseline prescription rates over the years
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

  results_2[[code]] <- data.frame(
    OUTCOME_CODE = code,
    YEAR = baseline_rates$YEAR,
    BASELINE_MEAN = baseline_rates$BASELINE_MEAN
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
ggsave(filename = paste0(outdir, "Supplements_BaselinePrescription_", DATE, ".png"), plot = p_baseline_evolution, width = 10, height = 6)

#-----------------------------------------------
# Part 2: Calculate relative changes 
dataset_with_baseline <- dataset %>%
  filter(OUTCOME_CODE %in% code_list) %>%
  left_join(baseline_rates_df, by = "OUTCOME_CODE")

# Calculate relative change
dataset_with_baseline <- dataset_with_baseline %>%
  mutate(REL_CHANGE = (ABS_CHANGE + BASELINE_MEAN) / BASELINE_MEAN) %>%
  arrange(REL_CHANGE)

# Plot V1 
p1 <- ggplot(dataset_with_baseline, aes(y = reorder(OUTCOME_CODE, REL_CHANGE))) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey", linewidth = 0.8) +
  geom_segment(aes(x = BASELINE_MEAN, xend = BASELINE_MEAN + ABS_CHANGE, yend = reorder(OUTCOME_CODE, REL_CHANGE)), color = "red", linewidth = 0.8) +
  geom_point(aes(x = BASELINE_MEAN, shape = "Baseline"), color = "red", size = 3, stroke = 1.5) +
  geom_point(aes(x = BASELINE_MEAN + ABS_CHANGE, shape = "After Event"), color = "red", size = 4) +
  scale_shape_manual(values = c("Baseline" = 1, "After Event" = 16), name = "") +
  labs(title = "Estimated Change in Average Prescription Rates After Event (Relative to Baseline)",
       x = "Prescription Rate",
       y = "ATC Code") +
  theme_minimal() +
  theme(axis.text.y = element_text(size = 8),legend.position = "bottom")

# Combine all plots into a single figure
ggsave(filename = paste0(outdir, "Supplements_RelativeChange_Plot1_", DATE, ".png"), plot = p1, width = 12, height = 8)

# Plot V2
# Calculate 95% CI for relative change
dataset_with_baseline <- dataset_with_baseline %>%
  mutate(
    REL_CHANGE_SE = abs(ABS_CHANGE_SE / BASELINE_MEAN),
    REL_CHANGE_CI_LOW = REL_CHANGE - 1.96 * REL_CHANGE_SE,
    REL_CHANGE_CI_UP = REL_CHANGE + 1.96 * REL_CHANGE_SE,
    PVAL_REL_CHANGE = 2 * (1 - pnorm(abs(REL_CHANGE / REL_CHANGE_SE)))
  )
p2 <- ggplot(dataset_with_baseline, aes(y = reorder(OUTCOME_CODE, REL_CHANGE))) +
  geom_vline(xintercept = 1, linetype = "dashed", color = "grey", linewidth = 0.8) +
  geom_point(aes(x = REL_CHANGE), color = "red", size = 3, fill = "red") +
  geom_errorbarh(aes(xmin = REL_CHANGE_CI_LOW, xmax = REL_CHANGE_CI_UP), height = 0.2, color = "red", linewidth = 0.8) +
  geom_text(aes(x = REL_CHANGE, label = paste0(round(REL_CHANGE, 3), "\n[", round(REL_CHANGE_CI_LOW, 3), ", ", round(REL_CHANGE_CI_UP, 3), "]")), hjust = -0.1, vjust = 0.5, size = 4) +
  labs(title = "Estimated Relative Change in Prescription Rates After Event (95% CI)",
       x = "Relative Change",
       y = "ATC Code") +
  theme_minimal() +
  theme(axis.text.y = element_text(size = 8), legend.position = "bottom")

# Combine all plots into a single figure
ggsave(filename = paste0(outdir, "Supplements_RelativeChange_Plot2_", DATE, ".png"), plot = p2, width = 12, height = 8)

# save results to file
dataset_with_baseline <- dataset_with_baseline %>%
  mutate(
        ABS_CHANGE_CI_LOW = ABS_CHANGE - 1.96 * ABS_CHANGE_SE,
        ABS_CHANGE_CI_UP  = ABS_CHANGE + 1.96 * ABS_CHANGE_SE
  ) %>%
  select(
      OUTCOME_CODE, BASELINE_MEAN, 
      ABS_CHANGE, REL_CHANGE, 
      ABS_CHANGE_SE, REL_CHANGE_SE,
      ABS_CHANGE_CI_LOW, ABS_CHANGE_CI_UP,
      REL_CHANGE_CI_LOW, REL_CHANGE_CI_UP,
      PVAL_ABS_CHANGE, PVAL_REL_CHANGE
      )
write_csv(dataset_with_baseline, paste0(outdir, "Supplements_RelativeChange_Estimates_", DATE, ".csv"))  
