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

# Calculate difference between AVG_EFFECT_AFTER and AVG_EFFECT_BEFORE
dataset <- dataset %>%
    mutate(
        EFFECT_DIFF = AVG_EFFECT_AFTER - AVG_EFFECT_BEFORE,
        EFFECT_DIFF_SE = sqrt(AVG_SE_AFTER^2 + AVG_SE_BEFORE^2), 
        SIGNIFICANT = abs(EFFECT_DIFF) > 1.96 * EFFECT_DIFF_SE
    )

# Extract medication chapter from OUTCOME_CODE
dataset <- dataset %>%
    mutate(MED_CHAPTER = substr(OUTCOME_CODE, 1, 1))

# Boxplot of EFFECT_DIFF stratified by medication chapter, colored by chapter
p <- ggplot(dataset, aes(x = MED_CHAPTER, y = EFFECT_DIFF, fill = MED_CHAPTER)) +
    geom_boxplot(outlier.shape = NA, show.legend = FALSE) +
    geom_jitter(
        aes(color = SIGNIFICANT, alpha = SIGNIFICANT),
        width = 0.2, size = 2
    ) +
    scale_color_manual(
        name = "Significance",
        values = c("TRUE" = "red", "FALSE" = "grey70"),
        labels = c("TRUE" = "Significant", "FALSE" = "Not Significant")
    ) +
    scale_alpha_manual(
        values = c("TRUE" = 1, "FALSE" = 0.3),
        guide = "none"
    ) +
    labs(
        title = "Difference in average prescription behaviour 3 years before vs after event",
        subtitle = sprintf("Number of medications tested: %d", nrow(dataset)),
        x = "Medication ATC Chapter",
        y = "average ATT difference"
    ) +
    theme_minimal() +
    theme(legend.position = "right") +
    geom_text(
        data = dataset %>% filter(SIGNIFICANT),
        aes(label = OUTCOME_CODE),
        hjust = -0.1, vjust = -0.5, size = 3, color = "black",
        show.legend = FALSE
    )

ggsave(output_file, plot = p, width = 10, height = 6)

