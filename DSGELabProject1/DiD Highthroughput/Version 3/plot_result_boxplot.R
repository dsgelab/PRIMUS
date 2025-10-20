# Libraries
library(ggplot2)
library(dplyr)
library(readr)
library(tidyr)
library(scales)

# Global Variables
DATE = "20251010"
dataset_file <- sprintf('/media/volume/Projects/DSGELabProject1/DiD_Experiments/Version3_Highthroughput/Results/Results_ATC_%s.csv', DATE)
output_file <- sprintf('/media/volume/Projects/DSGELabProject1/DiD_Experiments/Version3_Highthroughput/Results/Boxplot_%s.png', DATE)

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

# Create a combined significance variable
dataset$SIG_TYPE <- case_when(
  dataset$SIGNIFICANT_BONFERRONI & dataset$SIGNIFICANT_FDR ~ "Both",
  dataset$SIGNIFICANT_BONFERRONI ~ "Bonferroni",
  dataset$SIGNIFICANT_FDR ~ "FDR",
  TRUE ~ "Not Significant"
)
dataset$SIG_TYPE <- factor(dataset$SIG_TYPE, levels = c("Both", "Bonferroni", "FDR", "Not Significant"))

# Extract medication chapter from OUTCOME_CODE
dataset <- dataset %>%
    mutate(MED_CHAPTER = substr(OUTCOME_CODE, 1, 1))

# Boxplot of EFFECT_DIFF stratified by medication chapter, colored by chapter
p <- ggplot(dataset, aes(x = MED_CHAPTER, y = EFFECT_DIFF, fill = MED_CHAPTER)) +
    geom_boxplot(alpha = 0.7, outlier.shape = NA, show.legend = FALSE) +
    geom_jitter(aes(color = SIG_TYPE),width = 0.2, size = 2, alpha = 0.5, show.legend = TRUE) +
    geom_text(data = filter(dataset, SIGNIFICANT_BONFERRONI | SIGNIFICANT_FDR), aes(label = EVENT_CODE), vjust = 0, hjust = 1.2, size = 3) +
    scale_color_manual(
        name = "Significance",
        values = c("Both" = "darkgreen", "Bonferroni" = "blue", "FDR" = "red", "Not Significant" = "grey"),
        labels = c("Both" = "Both (Bonferroni & FDR)", "Bonferroni" = "Bonferroni only", "FDR" = "FDR only", "Not Significant" = "Not Significant")
    ) +
    labs(
        title = "Difference in average prescription behaviour 3 years before vs after event",
        subtitle = sprintf("Number of medications tested: %d", nrow(dataset)),
        x = "Medication ATC Chapter",
        y = "average ATT difference"
    ) +
  theme_minimal() +
  theme(
    legend.position = "right",
    legend.title = element_text(),
    legend.text = element_text()
  ) +
  guides(fill = "none") + 
  geom_hline(yintercept = 0, linetype = "dashed", color = "red")

ggsave(output_file, plot = p, width = 10, height = 6)
