# Prescription Event Analysis - Visualization Script
# Data: EVENT_CODE, BASELINE, DROP, TTR, N_CASES, N_CONTROLS

# Load required libraries
library(ggplot2)
library(dplyr)
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
DATE = "20250919"
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

# 3.A Time to recovery by event (excluding never-recovered cases)
data_recovered <- data %>% filter(TTR != -1)
avg_ttr <- mean(data_recovered$TTR, na.rm = TRUE)
p6 <- ggplot(data_recovered, aes(x = reorder(EVENT_CODE, TTR), y = TTR)) +
  geom_col(fill = "gray", alpha = 0.7) +
  geom_hline(yintercept = avg_ttr, color = "red", linetype = "dashed", size = 1) +
  geom_hline(yintercept = 12, color = "black", linetype = "solid", size = 0.8) +
  geom_hline(yintercept = 24, color = "black", linetype = "solid", size = 0.8) +
  annotate("text", x = 1, y = 12, label = "1 year", vjust = -1, hjust = 0, color = "black", size = 4) +
  annotate("text", x = 1, y = 24, label = "2 years", vjust = -1, hjust = 0, color = "black", size = 4) +
  labs(
    title = "Time to Recovery by Event Type (Recovered Cases Only)",
    subtitle = paste(
      "Average recovery time:",
      round(avg_ttr, 2), "months (", ttr_to_ym(avg_ttr), ")"
    ),
    x = "Event Code",
    y = "Time to Recovery (months)"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_blank())
ggsave(filename = paste0(OutDir, "TTRPlot_A_", DATE, ".png"), plot = p6, width = 8, height = 5)

# 3.B Recovery status stratified by medications and diagnosis (patchwork, stacked vertically)
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
p7_med <- ggplot(med_counts, aes(x = GROUP, y = Count, fill = RECOVERY_STATUS)) +
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
p7_diag <- ggplot(diag_counts, aes(x = GROUP, y = Count, fill = RECOVERY_STATUS)) +
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
p7 <- p7_med / p7_diag
ggsave(filename = paste0(OutDir, "TTRPlot_B_", DATE, ".png"), plot = p7, width = 10, height = 10)

# ============================================================================
# SECTION 4: EXTRA ANALYSIS
# Explore correlations between different variables
# ============================================================================

# 4.A TTR vs. Relative Drop relationship (for recovered cases only)
p8 <- ggplot(data_recovered, aes(x = REL_DROP, y = TTR)) +
  geom_point(alpha = 0.7, size = 3, color = "darkgreen") +
  geom_smooth(method = "lm", se = TRUE, color = "red") +
  labs(title = "Time to Recovery vs. Relative Prescription Drop",
       x = "Drop as % of Baseline",
       y = "Time to Recovery (months)") +
  scale_x_continuous(labels = scales::percent_format(scale = 1)) +
  theme_minimal()
ggsave(filename = paste0(OutDir, "ExtraPlot_A_", DATE, ".png"), plot = p8, width = 8, height = 5)

# 4.B TTR vs. Relative Drop relationship (including never-recovered cases)
data$recovery_status <- ifelse(data$TTR == -1, "Never Recovered", "Recovered")
data$TTR_display <- ifelse(data$TTR == -1, NA, data$TTR)
p9 <- ggplot(data, aes(x = REL_DROP)) +
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
ggsave(filename = paste0(OutDir, "ExtraPlot_B_", DATE, ".png"), plot = p9, width = 8, height = 5)