# Prescription Event Analysis - Visualization Script
# Data: EVENT_CODE, BASELINE, DROP, TTR, N_CASES, N_CONTROLS

# Load required libraries
library(ggplot2)
library(dplyr)
library(tidyr)
library(scales)
library(gridExtra)
library(patchwork)
library(viridis)

# Functions
ttr_to_ym <- function(m) {
  yrs <- floor(m / 12)
  mos <- round(m %% 12)
  paste0(yrs, "y ", mos, "m")
}

# Load data
DATE = "20250922"
data <- read.csv(paste0("/media/volume/Projects/DSGELabProject1/DiD_Experiments/Version1_Highthroughput_drop/Results/DropAnalysisResults_", DATE, ".csv"))
OutDir <- paste0("/media/volume/Projects/DSGELabProject1/DiD_Experiments/Version1_Highthroughput_drop/Results/PlotResults_", DATE, "/")
if (!dir.exists(OutDir)) {dir.create(OutDir, recursive = TRUE)}

# ============================================================================
# SECTION 1: BASELINE ANALYSIS
# Explore the pre-event prescription numbers across different events
# ============================================================================

# 1.A Baseline prescription numbers by event
avg_baseline <- mean(data$BASELINE, na.rm = TRUE)
p1 <- ggplot(data, aes(x = reorder(EVENT_CODE, BASELINE), y = BASELINE)) +
  geom_col(fill = "grey", alpha = 0.7) +
  geom_hline(yintercept = avg_baseline, color = "red", linetype = "dashed", size = 1) +
  labs(
    title = "Baseline Prescription Numbers by Event Type",
    subtitle = paste("Average baseline across events:", round(avg_baseline, 2)),
    x = "Event Code", 
    y = "Average Baseline Prescriptions Number"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_blank())
ggsave(filename = paste0(OutDir, "BaselinePlot_A_", DATE, ".png"), plot = p1, width = 8, height = 5)

# 1.B Distribution of baseline number 
avg_baseline <- mean(data$BASELINE, na.rm = TRUE)
p2 <- ggplot(data, aes(x = BASELINE)) +
  geom_histogram(bins = 20, fill = "gray", color = "black", alpha = 0.7) +
  geom_vline(xintercept = avg_baseline, color = "red", linetype = "dashed", size = 1) +
  labs(
    title = "Distribution of Baseline Prescription Numbers, Across different Events",
    subtitle = paste("Average baseline:", round(avg_baseline, 2)),
    x = "Baseline Prescriptions",
    y = "Frequency"
  ) +
  theme_minimal()
ggsave(filename = paste0(OutDir, "BaselinePlot_B_", DATE, ".png"), plot = p2, width = 8, height = 5)

# ============================================================================
# SECTION 2: DROP ANALYSIS
# Analyze the immediate impact (prescription drops) after events
# ============================================================================

# 2.A Relative drop (as percentage of baseline)
data$REL_DROP <- (data$DROP / data$BASELINE) * 100
avg_rel_drop <- mean(data$REL_DROP, na.rm = TRUE)
p3 <- ggplot(data, aes(x = reorder(EVENT_CODE, -REL_DROP), y = REL_DROP)) +
  geom_col(fill = "grey", alpha = 0.7) +
  geom_hline(yintercept = avg_rel_drop, color = "red", linetype = "dashed", size = 1) +
  labs(
    title = "Relative Prescription Drop by Event",
    subtitle = paste("Average relative drop across events:", round(avg_rel_drop, 2), "%"),
    x = "Event Code",
    y = "Drop as % of Baseline"
  ) +
  scale_y_continuous(labels = percent_format(scale = 1)) +
  theme_minimal() +
  theme(axis.text.x = element_blank())
ggsave(filename = paste0(OutDir, "DropPlot_A_", DATE, ".png"), plot = p3, width = 8, height = 5)

# 2.B Relative drop, Boxplot overall distribution with labels for top and bottom 10 drops
top10 <- data %>% arrange(desc(REL_DROP)) %>% slice_head(n = 10)
bottom10 <- data %>% arrange(REL_DROP) %>% slice_head(n = 10)
label_data <- bind_rows(top10, bottom10) %>%
  arrange(REL_DROP) %>%
  mutate(
    label_y = REL_DROP,
    label_x = 1.2,
    rank = row_number(),
    spread_y = seq(min(REL_DROP) - 5, max(REL_DROP) + 5, length.out = 20)
  )
p4 <- ggplot(data, aes(x = 1, y = REL_DROP)) +
  geom_boxplot(fill = "grey", alpha = 0.7, width = 0.3) +
  geom_point(data = label_data, aes(x = 1, y = REL_DROP), color = "black", size = 2) +
  geom_text(data = label_data, aes(x = label_x, y = spread_y, label = EVENT_CODE), color = "black", size = 3, hjust = 0) +
  geom_segment(data = label_data, aes(x = 1, xend = label_x - 0.05, y = REL_DROP, yend = spread_y), color = "black", linetype = "dotted") +
  geom_hline(yintercept = 0, color = "red", linetype = "solid", size = 0.5) +
  labs(title = "Distribution of Relative Prescription Drop (All Events)", x = "", y = "Drop as % of Baseline") +
  scale_y_continuous(labels = percent_format(scale = 1)) +
  scale_x_continuous(limits = c(0.8, 1.3)) +
  theme_minimal() +
  theme(
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank()
  )
ggsave(filename = paste0(OutDir, "DropPlot_B_", DATE, ".png"), plot = p4, width = 8, height = 5)

# 2.C Relative drop, Boxplot across diagnosis and medication chapters
data$GROUP <- substr(sub(".*_", "", data$EVENT_CODE), 1, 1)
data_med <- data %>% filter(grepl("^Purch", EVENT_CODE))
data_diag <- data %>% filter(grepl("^Diag", EVENT_CODE))
# Medications boxplot
p_med <- ggplot(data_med, aes(x = GROUP, y = REL_DROP, fill = GROUP)) +
  geom_boxplot(alpha = 0.7) +
  labs(title = "Relative Drop by Medication Chapter",
       x = "Medication Chapter",
       y = "Drop as % of Baseline") +
  scale_y_continuous(labels = percent_format(scale = 1), limits = c(0, 100)) +
  theme_minimal() +
  theme(legend.position = "none")
# Diagnosis boxplot
p_diag <- ggplot(data_diag, aes(x = GROUP, y = REL_DROP, fill = GROUP)) +
  geom_boxplot(alpha = 0.7) +
  labs(title = "Relative Drop by Diagnosis Chapter",
       x = "Diagnosis Chapter",
       y = "Drop as % of Baseline") +
  scale_y_continuous(labels = percent_format(scale = 1), limits = c(0, 100)) +
  theme_minimal() +
  theme(legend.position = "none")
# Arrange plots side by side, and save
p5 <- p_med / p_diag
ggsave(filename = paste0(OutDir, "DropPlot_C_", DATE, ".png"), plot = p5, width = 12, height = 5, device = "png")

# ============================================================================
# SECTION 3: TIME-TO-RECOVERY (TTR) ANALYSIS
# Examine recovery patterns and identify events with no recovery
# ============================================================================

# 3.A Time to recovery by event (excluding never-recovered cases) - Boxplot
data_recovered <- data %>% filter(TTR != -1)
avg_ttr <- mean(data_recovered$TTR, na.rm = TRUE)
n_total <- nrow(data)
n_recovered <- nrow(data_recovered)
p6 <- ggplot(data_recovered, aes(x = 1, y = TTR)) +
  geom_boxplot(fill = "gray", alpha = 0.7, width = 0.3) +
  geom_jitter(width = 0.1, height = 0, alpha = 0.5, color = "black", size = 2) +
  geom_hline(yintercept = avg_ttr, color = "red", linetype = "dashed", size = 1) +
  labs(
    title = "Time to Recovery Distribution (Recovered Cases Only)",
    subtitle = paste(
      "Average recovery time:", round(avg_ttr, 2), "months |",
      "Recovered events:", n_recovered, "of", n_total
    ),
    x = "",
    y = "Time to Recovery (months)"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank()) +
  scale_y_continuous(limits = c(0, NA))
ggsave(filename = paste0(OutDir, "TTRPlot_A_", DATE, ".png"), plot = p6, width = 8, height = 5)

# 3.B Time to recovery by event, stratified by medication and diagnosis chapters (boxplots)
data_recovered <- data %>% filter(TTR != -1)
data_med <- data_recovered %>% filter(grepl("^Purch", EVENT_CODE))
data_diag <- data_recovered %>% filter(grepl("^Diag", EVENT_CODE))
all_med_groups <- data %>% filter(grepl("^Purch", EVENT_CODE)) %>% pull(GROUP) %>% unique() %>% sort()
all_diag_groups <- data %>% filter(grepl("^Diag", EVENT_CODE)) %>% pull(GROUP) %>% unique() %>% sort()
# Medication chapter boxplot
p7_med <- ggplot(data_med, aes(x = GROUP, y = TTR, fill = GROUP)) +
  geom_boxplot(alpha = 0.7) +
  scale_x_discrete(limits = all_med_groups) +
  scale_y_continuous(limits = c(0, NA)) +
  labs(
    title = "Time to Recovery by Medication Chapter (Recovered Cases Only)",
    x = "Medication Chapter",
    y = "Time to Recovery (months)"
  ) +
  theme_minimal() +
  theme(legend.position = "none")
# Diagnosis chapter boxplot
p7_diag <- ggplot(data_diag, aes(x = GROUP, y = TTR, fill = GROUP)) +
  geom_boxplot(alpha = 0.7) +
  scale_x_discrete(limits = all_diag_groups) +
  scale_y_continuous(limits = c(0, NA)) +
  labs(
    title = "Time to Recovery by Diagnosis Chapter (Recovered Cases Only)",
    x = "Diagnosis Chapter",
    y = "Time to Recovery (months)"
  ) +
  theme_minimal() +
  theme(legend.position = "none")
# Arrange plots side by side and save
p7 <- p7_med / p7_diag
ggsave(filename = paste0(OutDir, "TTRPlot_B_", DATE, ".png"), plot = p7, width = 12, height = 5)


# 3.C Recovery status, stratified by medications and diagnosis
data$RECOVERY_STATUS <- ifelse(data$TTR == -1, "Never Recovered", "Recovered")
data_med <- data %>% filter(grepl("^Purch", EVENT_CODE))
data_diag <- data %>% filter(grepl("^Diag", EVENT_CODE))
# Count recovery status for subtitle
med_recovered <- sum(data_med$RECOVERY_STATUS == "Recovered", na.rm = TRUE)
med_never <- sum(data_med$RECOVERY_STATUS == "Never Recovered", na.rm = TRUE)
diag_recovered <- sum(data_diag$RECOVERY_STATUS == "Recovered", na.rm = TRUE)
diag_never <- sum(data_diag$RECOVERY_STATUS == "Never Recovered", na.rm = TRUE)
# Medications recovery status
med_counts <- data_med %>%
  group_by(GROUP, RECOVERY_STATUS) %>%
  summarise(Count = n())
p8_med <- ggplot(med_counts, aes(x = GROUP, y = Count, fill = RECOVERY_STATUS)) +
  geom_col(position = "dodge", alpha = 0.8) +
  labs(
    title = "Recovery Status by Medication Chapter",
    subtitle = paste0("Recovered: ", med_recovered, " | Never Recovered: ", med_never),
    x = "Medication Chapter",
    y = "Number of Events"
  ) +
  scale_fill_manual(values = c("Never Recovered" = "orange", "Recovered" = "blue")) +
  theme_minimal()
# Diagnosis recovery status
diag_counts <- data_diag %>%
  group_by(GROUP, RECOVERY_STATUS) %>%
  summarise(Count = n())
p8_diag <- ggplot(diag_counts, aes(x = GROUP, y = Count, fill = RECOVERY_STATUS)) +
  geom_col(position = "dodge", alpha = 0.8) +
  labs(
    title = "Recovery Status by Diagnosis Chapter",
    subtitle = paste0("Recovered: ", diag_recovered, " | Never Recovered: ", diag_never),
    x = "Diagnosis Chapter",
    y = "Number of Events"
  ) +
  scale_fill_manual(values = c("Never Recovered" = "orange", "Recovered" = "blue")) +
  theme_minimal()
# Stack plots vertically, and save
p8 <- p8_med / p8_diag
ggsave(filename = paste0(OutDir, "TTRPlot_C_", DATE, ".png"), plot = p8, width = 10, height = 10)


# 3.D TTR stratified by age at event
ttr_age_long <- data %>%
  select(EVENT_CODE, starts_with("TTR_AGE_Q")) %>%
  pivot_longer(
    cols = starts_with("TTR_AGE_Q"),
    names_to = "AGE_QUARTILE",
    values_to = "TTR"
  ) %>%
  mutate(
    AGE_QUARTILE = factor(
      AGE_QUARTILE,
      levels = c("TTR_AGE_Q1", "TTR_AGE_Q2", "TTR_AGE_Q3", "TTR_AGE_Q4"),
      labels = c("Q1 (Youngest)", "Q2", "Q3", "Q4 (Oldest)")
    )
  )
# Boxplot of TTR by age quartile, keep outliers printed
data_plot <- ttr_age_long[!is.na(ttr_age_long$TTR) & ttr_age_long$TTR != -1, ]
p_ttr_age_boxplot <- ggplot(data_plot, aes(x = AGE_QUARTILE, y = TTR, fill = AGE_QUARTILE)) +
  geom_boxplot(alpha = 0.7) +
  coord_cartesian(ylim = quantile(data_plot$TTR, c(0.05, 0.95), na.rm = TRUE)) +
  labs(
    title = "Time to Recovery by Age Quartile at Event",
    x = "Age Quartile",
    y = "Time to Recovery (months)"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

recov_summary <- ttr_age_long %>%
  mutate(RECOVERY_STATUS = ifelse(TTR == -1, "Never Recovered", "Recovered")) %>%
  group_by(AGE_QUARTILE) %>%
  summarise(
    N = n(),
    Recovered = sum(RECOVERY_STATUS == "Recovered"),
    Perc_Recovered = Recovered / N * 100
  )
# Barplot: percentage of recovered events by age quartile, with N on top
p_ttr_age_recov <- ggplot(recov_summary, aes(x = AGE_QUARTILE, y = Perc_Recovered, fill = AGE_QUARTILE)) +
  geom_col(alpha = 0.8, width = 0.7) +
  geom_text(aes(label = paste0("N=", N)), vjust = -0.5, size = 3.5) +
  scale_y_continuous(labels = scales::percent_format(scale = 1), limits = c(0, 100)) +
  labs(
    title = "Percentage of Recovered Events by Age Quartile",
    x = "Age Quartile",
    y = "% Recovered"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

# Combine boxplot and barplot vertically
p_ttr_age <- p_ttr_age_boxplot / p_ttr_age_recov + plot_layout(heights = c(2, 1))
ggsave(filename = paste0(OutDir, "TTRPlot_D_AgeQuartile_", DATE, ".png"), plot = p_ttr_age, width = 8, height = 5)

# ============================================================================
# SECTION 4: EXTRA ANALYSIS
# Explore correlations between different variables
# ============================================================================

# 4.A TTR vs. Relative Drop relationship (for recovered cases only)
p9 <- ggplot(data_recovered, aes(x = REL_DROP, y = TTR)) +
  geom_point(alpha = 0.7, size = 3, color = "darkgreen") +
  geom_smooth(method = "lm", se = TRUE, color = "red") +
  labs(title = "Time to Recovery vs. Relative Prescription Drop",
       x = "Drop as % of Baseline",
       y = "Time to Recovery (months)") +
  scale_x_continuous(labels = scales::percent_format(scale = 1)) +
  theme_minimal()
ggsave(filename = paste0(OutDir, "ExtraPlot_A_", DATE, ".png"), plot = p9, width = 8, height = 5)

# 4.B TTR vs. Relative Drop relationship (including never-recovered cases)
data$recovery_status <- ifelse(data$TTR == -1, "Never Recovered", "Recovered")
data$TTR_display <- ifelse(data$TTR == -1, NA, data$TTR)
p10 <- ggplot(data, aes(x = REL_DROP)) +
  # Points for recovered cases
  geom_point(data = subset(data, recovery_status == "Recovered"),
             aes(y = TTR_display, color = "Recovered"), 
             alpha = 0.7, size = 3) +
  # Jittered points at bottom for never-recovered cases
  geom_jitter(data = subset(data, recovery_status == "Never Recovered"),
              aes(y = -2, color = "Never Recovered"), 
              alpha = 0.7, size = 3, height = 0.5, width = 0) +
  # Smooth line only for recovered cases
  geom_smooth(data = subset(data, recovery_status == "Recovered"), aes(y = TTR_display), method = "lm", se = TRUE, color = "red", linetype = "dashed") +
  # Horizontal line to separate recovered from non-recovered
  geom_hline(yintercept = -1, linetype = "dotted", color = "gray50", alpha = 0.7) +
  scale_color_manual(values = c("Recovered" = "darkgreen", "Never Recovered" = "red")) +
  scale_x_continuous(labels = scales::percent_format(scale = 1)) +
  scale_y_continuous(breaks = c(-2, 0, 5, 10, 15, 20, 25), labels = c("Never\nRecovered", "0", "5", "10", "15", "20", "25")) +
  labs(title = "Time to Recovery vs. Relative Prescription Drop",
       subtitle = "Including both recovered and never-recovered events",
       x = "Drop as % of Baseline",
       y = "Time to Recovery (months)",
       color = "Recovery Status") +
  theme_minimal() +
  theme(legend.position = "bottom")
ggsave(filename = paste0(OutDir, "ExtraPlot_B_", DATE, ".png"), plot = p10, width = 8, height = 5)