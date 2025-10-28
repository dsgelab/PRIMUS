# Load required libraries
library(ggplot2)
library(dplyr)
library(tidyr)
library(scales)
library(gridExtra)
library(patchwork)
library(viridis)
library(ggrepel)

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
data_diag$SIGNIFICANT_FDR <- data_diag$PVAL_ADJ_FDR < 0.05
data_diag$SIG_TYPE <- ifelse(data_diag$SIGNIFICANT_FDR, "Significant", "Not Significant")

# Sort GROUP alphabetically for x axis
data_diag$GROUP <- factor(data_diag$GROUP, levels = sort(unique(data_diag$GROUP)))

# ICD-10 Chapter names mapping
icd10_chapter_map <- c(
  "A" = "Infectious diseases",
  "C" = "Neoplasms",
  "D" = "Blood disorders",
  "E" = "Endocrine/metabolic",
  "F" = "Mental disorders",
  "G" = "Nervous system",
  "H" = "Eye/ear diseases",
  "I" = "Circulatory system",
  "J" = "Respiratory system",
  "K" = "Digestive system",
  "L" = "Skin diseases",
  "M" = "Musculoskeletal",
  "N" = "Genitourinary",
  "O" = "Pregnancy/childbirth",
  "P" = "Perinatal conditions",
  "Q" = "Congenital anomalies",
  "R" = "Symptoms/signs",
  "S" = "Injuries",
  "T" = "Poisoning/injuries",
  "U" = "Codes for special purposes",
  "V" = "External causes",
  "Z" = "Health status factors"
)

# Map chapter letters to full names
data_diag$CHAPTER_NAME <- factor(icd10_chapter_map[as.character(data_diag$GROUP)], levels = icd10_chapter_map[sort(unique(as.character(data_diag$GROUP)))])

# Calculate chapter-level averages with proper weighting by SE
chapter_summary <- data_diag %>%
  group_by(CHAPTER_NAME) %>%
  summarise(
    weight_sum = sum(1 / (SE_DROP^2)),
    ATT_MEAN = sum(ATT_DROP / (SE_DROP^2)) / weight_sum,
    SE_MEAN = sqrt(1 / weight_sum),
    .groups = "drop"
  )

# Color-blind friendly palette
cb_palette <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", 
                "#D55E00", "#CC79A7", "#999999", "#000000", "#E6AB02",
                "#7570B3", "#66A61E", "#E7298A", "#A6761D", "#666666",
                "#1B9E77", "#D95F02", "#7570B3", "#E7298A", "#66A61E", "#E6AB02")

# Left plot: Individual points
p_left <- ggplot(data_diag, aes(x = CHAPTER_NAME, y = ATT_DROP, color = CHAPTER_NAME)) +
  geom_jitter(aes(alpha = SIG_TYPE), width = 0.2, size = 3) +
  scale_color_manual(values = cb_palette, name = "Chapter", guide = "none") +
  scale_alpha_manual(
    name = "Significance\n(FDR < 0.05)",
    values = c("Significant" = 1, "Not Significant" = 0.2)
  ) +
  labs(
    title = "Individual Diagnoses",
    x = "ICD-10 Diagnosis Chapter",
    y = "Change in Total Prescriptions\n(DiD Estimate)"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "right" 
  ) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red")

# Right plot: Chapter averages
p_right <- ggplot(chapter_summary, aes(x = CHAPTER_NAME, y = ATT_MEAN, color = CHAPTER_NAME)) +
  geom_point(shape = 15, size = 3) +
  geom_errorbar(aes(ymin = ATT_MEAN - 1.96*SE_MEAN, ymax = ATT_MEAN + 1.96*SE_MEAN), width = 0.2) +
  scale_color_manual(values = cb_palette, name = "Chapter", guide = "none") +
  labs(
    title = "Chapter Averages",
    x = "ICD-10 Diagnosis Chapter",
    y = "Mean Change in Total Prescriptions\n(DiD Estimate)"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "none"
  ) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red")

# Combine plots
p_combined <- p_left + p_right + 
  plot_annotation(
    title = "Drop in Total Prescriptions due to Health Event, by Diagnosis Chapter",
    subtitle = sprintf("Number of diagnosis tested: %d", nrow(data_diag))
  )

# Save
ggsave(filename = paste0(OutDir, "DropPlot_", DATE, ".png"), 
       plot = p_combined, width = 16, height = 6, device = "png")