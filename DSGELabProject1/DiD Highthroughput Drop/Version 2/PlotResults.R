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

# Load data
DATE = "20251014"
data <- read.csv(paste0("/media/volume/Projects/DSGELabProject1/DiD_Experiments/Version2_Highthroughput_drop/Results/DropAnalysisResults_", DATE, ".csv"))
OutDir <- paste0("/media/volume/Projects/DSGELabProject1/DiD_Experiments/Version2_Highthroughput_drop/Results/PlotResults_", DATE, "/")
if (!dir.exists(OutDir)) {dir.create(OutDir, recursive = TRUE)}

# ============================================================================
# DROP ANALYSIS
# Analyze the immediate impact (prescription drops) after events
# ============================================================================

# Relative drop (as ATT from DiD framework)
# Boxplot of event across different diagnosis chapters
data$GROUP <- substr(sub(".*_", "", data$EVENT_CODE), 1, 1)
data$SIGNIFICANT <- abs(data$ATT_DROP) > 1.96 * data$SE_DROP
data_diag <- data %>% filter(grepl("^Diag", EVENT_CODE))

# Sort GROUP alphabetically for x axis
data_diag$GROUP <- factor(data_diag$GROUP, levels = sort(unique(data_diag$GROUP)))

p_diag <- ggplot(data_diag, aes(x = GROUP, y = ATT_DROP, fill = GROUP)) +
  geom_boxplot(alpha = 0.7, outlier.shape = NA, show.legend = FALSE) +
  geom_jitter(aes(color = SIGNIFICANT), width = 0.2, size = 2, alpha = 0.5, show.legend = TRUE) +
  scale_color_manual(
    name = "Significant",
    values = c("TRUE" = "black", "FALSE" = "grey"),
    labels = c("TRUE" = "Significant", "FALSE" = "Not Significant")
  ) +
  labs(
    title = "Drop in Total Prescriptions due to Health Event, by Diagnosis Chapter",
    x = "Diagnosis Chapter",
    y = "Dif-in-Dif ATT coefficient"
  ) +
  theme_minimal() +
  theme(
    legend.position = "right",
    legend.title = element_text(),
    legend.text = element_text()
  ) +
  guides(fill = "none") + 
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") + # Add red dashed horizontal line at y=0
  # Add label for significant events
  geom_text(
    data = subset(data_diag, SIGNIFICANT),
    aes(label = EVENT_CODE),
    vjust = -1, size = 3, color = "black"
  )

# Arrange plots side by side, and save
ggsave(filename = paste0(OutDir, "DropPlot_", DATE, ".png"), plot = p_diag, width = 12, height = 5, device = "png")
