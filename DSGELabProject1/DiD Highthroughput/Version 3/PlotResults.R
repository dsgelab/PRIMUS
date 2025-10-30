# Libraries
library(ggplot2)
library(dplyr)
library(readr)
library(tidyr)
library(scales)
library(patchwork)

# Global Variables
DATE = "20251029"
dataset_file <- sprintf('/media/volume/Projects/DSGELabProject1/DiD_Experiments/Version3_Highthroughput/Results/Results_ATC_%s.csv', DATE)
output_file <- sprintf('/media/volume/Projects/DSGELabProject1/DiD_Experiments/Version3_Highthroughput/Results/Plot_%s.png', DATE)

# Main 
dataset <- read_csv(dataset_file, show_col_types = FALSE)

# Calculate p-values from z-scores
dataset <- dataset %>%
    mutate(
        EFFECT_DIFF = AVG_EFFECT_AFTER - AVG_EFFECT_BEFORE,
        EFFECT_DIFF_SE = sqrt(AVG_SE_AFTER^2 + AVG_SE_BEFORE^2),
        Z_SCORE = EFFECT_DIFF / EFFECT_DIFF_SE,
        PVAL = 2 * (1 - pnorm(abs(Z_SCORE)))
    )

# Apply multiple testing corrections
dataset$PVAL_ADJ_BONFERRONI <- p.adjust(dataset$PVAL, method = "bonferroni")
dataset$PVAL_ADJ_FDR <- p.adjust(dataset$PVAL, method = "fdr")
dataset$SIGNIFICANT_BONFERRONI <- dataset$PVAL_ADJ_BONFERRONI < 0.05
dataset$SIGNIFICANT_FDR <- dataset$PVAL_ADJ_FDR < 0.05

# Create a combined significance variable for FDR
dataset$SIG_TYPE <- ifelse(dataset$SIGNIFICANT_FDR, "Significant", "Not Significant")

# Extract medication chapter from OUTCOME_CODE
dataset <- dataset %>%
    mutate(MED_CHAPTER = substr(OUTCOME_CODE, 1, 1))

# Sort MED_CHAPTER alphabetically for x axis
dataset$MED_CHAPTER <- factor(dataset$MED_CHAPTER, levels = sort(unique(dataset$MED_CHAPTER)))

# ATC Chapter names mapping
# Based on WHO ATC classification system
atc_chapter_map <- c(
  "A" = "Alimentary Tract and Metabolism",
  "B" = "Blood and Blood Forming Organs",
  "C" = "Cardiovascular System",
  "D" = "Dermatologicals",
  "G" = "Genito Urinary System and Sex Hormones",
  "H" = "Systemic Hormonal Preparations, Excl. Sex Hormones and Insulins",
  "J" = "Antiinfectives for Systemic Use",
  "L" = "Antineoplastic and Immunomodulating Agents",
  "M" = "Musculo-Skeletal System",
  "N" = "Nervous System",
  "P" = "Antiparasitic Products, Insecticides and Repellents",
  "R" = "Respiratory System",
  "S" = "Sensory Organs",
  "V" = "Various"
)

# Map chapter letters to full names
dataset$CHAPTER_NAME <- factor(atc_chapter_map[as.character(dataset$MED_CHAPTER)], 
                               levels = atc_chapter_map[sort(unique(as.character(dataset$MED_CHAPTER)))])

# Calculate chapter-level averages with proper weighting by SE
chapter_summary <- dataset %>%
  group_by(CHAPTER_NAME) %>%
  summarise(
    weight_sum = sum(1 / (EFFECT_DIFF_SE^2)),
    EFFECT_DIFF_MEAN = sum(EFFECT_DIFF / (EFFECT_DIFF_SE^2)) / weight_sum,
    SE_MEAN = sqrt(1 / weight_sum),
    .groups = "drop"
  )

# Color-blind friendly palette
cb_palette <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", 
                "#D55E00", "#CC79A7", "#999999", "#000000", "#E6AB02",
                "#7570B3", "#66A61E", "#E7298A", "#A6761D", "#666666",
                "#1B9E77", "#D95F02", "#7570B3", "#E7298A", "#66A61E", "#E6AB02")

# Left plot: Individual points
p_left <- ggplot(dataset, aes(x = CHAPTER_NAME, y = EFFECT_DIFF, color = CHAPTER_NAME)) +
  geom_jitter(aes(alpha = SIG_TYPE), width = 0.2, size = 3) +
  scale_color_manual(values = cb_palette, name = "Chapter", guide = "none") +
  scale_alpha_manual(
    name = "Significance\n(FDR < 0.05)",
    values = c("Significant" = 1, "Not Significant" = 0.2)
  ) +
  labs(
    title = "Individual Medications",
    x = "ATC Medication Chapter",
    y = "Change in Prescription Behavior\n(Difference in Average ATT)"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "right" 
  ) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red")

# Right plot: Chapter averages
p_right <- ggplot(chapter_summary, aes(x = CHAPTER_NAME, y = EFFECT_DIFF_MEAN, color = CHAPTER_NAME)) +
  geom_point(shape = 15, size = 3) +
  geom_errorbar(aes(ymin = EFFECT_DIFF_MEAN - 1.96*SE_MEAN, ymax = EFFECT_DIFF_MEAN + 1.96*SE_MEAN), width = 0.2) +
  scale_color_manual(values = cb_palette, name = "Chapter", guide = "none") +
  labs(
    title = "Chapter Averages",
    x = "ATC Medication Chapter",
    y = "Mean Change in Prescription Behavior\n(Difference in Average ATT)"
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
    title = "Difference in Average Prescription Behavior 3 Years Before vs After Event, by Medication Chapter",
    subtitle = sprintf("Number of medications tested: %d", nrow(dataset))
  )

# Save
ggsave(output_file, plot = p_combined, width = 16, height = 6, device = "png")