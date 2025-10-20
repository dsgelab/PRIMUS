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
data_diag <- data %>% filter(grepl("^Diag", EVENT_CODE))
data_diag$GROUP <- substr(sub(".*_", "", data_diag$EVENT_CODE), 1, 1)
data_diag$PVAL <- 2 * (1 - pnorm(abs(data_diag$ATT_DROP / data_diag$SE_DROP)))

# Apply Bonferroni correction
data_diag$PVAL_ADJ_BONFERRONI <- p.adjust(data_diag$PVAL, method = "bonferroni")
data_diag$PVAL_ADJ_FDR <- p.adjust(data_diag$PVAL, method = "fdr")

data_diag$SIGNIFICANT_BONFERRONI <- data_diag$PVAL_ADJ_BONFERRONI < 0.05
data_diag$SIGNIFICANT_FDR <- data_diag$PVAL_ADJ_FDR < 0.05

# Sort GROUP alphabetically for x axis
data_diag$GROUP <- factor(data_diag$GROUP, levels = sort(unique(data_diag$GROUP)))

# Create a combined significance variable
data_diag$SIG_TYPE <- case_when(
  data_diag$SIGNIFICANT_BONFERRONI & data_diag$SIGNIFICANT_FDR ~ "Both",
  data_diag$SIGNIFICANT_BONFERRONI ~ "Bonferroni",
  data_diag$SIGNIFICANT_FDR ~ "FDR",
  TRUE ~ "Not Significant"
)
data_diag$SIG_TYPE <- factor(data_diag$SIG_TYPE, levels = c("Both", "Bonferroni", "FDR", "Not Significant"))

p_diag <- ggplot(data_diag, aes(x = GROUP, y = ATT_DROP, fill = GROUP)) +
  geom_boxplot(alpha = 0.7, outlier.shape = NA, show.legend = FALSE) +
  geom_jitter(aes(color = SIG_TYPE), width = 0.2, size = 2, alpha = 0.5, show.legend = TRUE) +
  geom_text(data = filter(data_diag, SIGNIFICANT_BONFERRONI | SIGNIFICANT_FDR), aes(label = EVENT_CODE), vjust = 0, hjust = 1.2, size = 3) + 
  scale_color_manual(
    name = "Significance",
    values = c("Both" = "darkgreen", "Bonferroni" = "blue", "FDR" = "red", "Not Significant" = "grey"),
    labels = c("Both" = "Both (Bonferroni & FDR)", "Bonferroni" = "Bonferroni only", "FDR" = "FDR only", "Not Significant" = "Not Significant")
  ) +
  labs(
    title = "Drop in Total Prescriptions due to Health Event, by Diagnosis Chapter",
    subtitle = sprintf("Number of diagnosis tested: %d", nrow(data_diag)),
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
  geom_hline(yintercept = 0, linetype = "dashed", color = "red")

# Arrange plots side by side, and save
ggsave(filename = paste0(OutDir, "DropPlot_", DATE, ".png"), plot = p_diag, width = 12, height = 5, device = "png")
