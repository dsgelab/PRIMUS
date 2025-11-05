# Libraries
library(ggplot2)
library(dplyr)
library(readr)
library(tidyr)
library(scales)
library(patchwork)
library(ggrepel)

# Global Variables
DATE = "20251030"
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

# Create a combined significance variable with three levels
dataset$SIG_TYPE <- case_when(
  dataset$SIGNIFICANT_FDR ~ "FDR Significant",
  dataset$PVAL < 0.05 ~ "P-value Significant",
  TRUE ~ "Not Significant"
)

# Convert to factor with explicit levels to avoid alphabetical sorting
dataset$SIG_TYPE <- factor(dataset$SIG_TYPE, levels = c("FDR Significant", "P-value Significant", "Not Significant"))

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

# Map chapter letters to full names, remove unknown levels
dataset$CHAPTER_NAME <- factor(atc_chapter_map[as.character(dataset$MED_CHAPTER)], levels = atc_chapter_map[sort(unique(as.character(dataset$MED_CHAPTER)))])
dataset <- dataset %>% filter(!is.na(CHAPTER_NAME))

# Calculate chapter-level averages (inverse-variance weighted mean of the ATT estimates)
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
# Identify top 10 extreme values by absolute EFFECT_DIFF OR significant ones
top_extreme <- dataset %>%
  filter(abs(EFFECT_DIFF) >= sort(abs(EFFECT_DIFF), decreasing = TRUE)[10] | PVAL < 0.05)

# Set seed for reproducible jittering
set.seed(42)
# Global Variables for Plotting
JITTER_RANGE <- 0.2
TOP_EXTREME_COUNT <- 10
POINT_SIZE_FDR <- 4
POINT_SIZE_PVAL <- 4
POINT_SIZE_NOT_SIG <- 4
ALPHA_FDR <- 1
ALPHA_PVAL <- 1
ALPHA_NOT_SIG <- 0.2
TEXT_SIZE_TITLE <- 16
TEXT_SIZE_AXIS_TITLE <- 14
TEXT_SIZE_AXIS_TEXT <- 12
TEXT_SIZE_LEGEND <- 12

# Add jittered positions to both datasets
dataset$x_jittered <- as.numeric(dataset$CHAPTER_NAME) + runif(nrow(dataset), -JITTER_RANGE, JITTER_RANGE)
top_extreme$x_jittered <- dataset$x_jittered[match(interaction(top_extreme$CHAPTER_NAME, top_extreme$OUTCOME_CODE), interaction(dataset$CHAPTER_NAME, dataset$OUTCOME_CODE))]
p_left <- ggplot(dataset, aes(x = x_jittered, y = EFFECT_DIFF, color = CHAPTER_NAME)) +
  geom_point(aes(shape = SIG_TYPE, size = SIG_TYPE, alpha = SIG_TYPE, fill = SIG_TYPE)) +
  geom_text_repel(data = top_extreme, aes(label = OUTCOME_CODE), 
            size = 4, 
            show.legend = FALSE,
            max.overlaps = Inf,
            min.segment.length = 0,
            box.padding = 0.5,
            point.padding = 0.3,
            force = 2,
            force_pull = 0.5) +
  scale_x_continuous(
    breaks = 1:length(levels(dataset$CHAPTER_NAME)),
    labels = levels(dataset$CHAPTER_NAME)
  ) +
  scale_color_manual(values = cb_palette, name = "Chapter", guide = "none") +
  scale_fill_manual(
    values = c("FDR Significant" = "black", "P-value Significant" = "black", "Not Significant" = "gray70"),
    guide = "none"
  ) +
  scale_shape_manual(
    name = "Significance",
    values = c("FDR Significant" = 17, "P-value Significant" = 16, "Not Significant" = 16)
  ) +
  scale_size_manual(
    name = "Significance",
    values = c("FDR Significant" = POINT_SIZE_FDR, "P-value Significant" = POINT_SIZE_PVAL, "Not Significant" = POINT_SIZE_NOT_SIG)
  ) +
  scale_alpha_manual(
    name = "Significance",
    values = c("FDR Significant" = ALPHA_FDR, "P-value Significant" = ALPHA_PVAL, "Not Significant" = ALPHA_NOT_SIG)
  ) +
  labs(
    title = "Individual Medications",
    x = "ATC Medication Chapter",
    y = "Change in Prescription Behavior\n(Difference in Average ATT)"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = TEXT_SIZE_AXIS_TEXT),
    axis.text.y = element_text(size = TEXT_SIZE_AXIS_TEXT),
    axis.title.x = element_text(size = TEXT_SIZE_AXIS_TITLE),
    axis.title.y = element_text(size = TEXT_SIZE_AXIS_TITLE),
    plot.title = element_text(size = TEXT_SIZE_TITLE),
    legend.text = element_text(size = TEXT_SIZE_LEGEND),
    legend.title = element_text(size = TEXT_SIZE_LEGEND),
    legend.position = "right" 
  ) +
  guides(shape = guide_legend(override.aes = list(size = c(POINT_SIZE_FDR, POINT_SIZE_PVAL, POINT_SIZE_NOT_SIG), alpha = c(ALPHA_FDR, ALPHA_PVAL, ALPHA_NOT_SIG), fill = c("black", "black", "gray70")))) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red")

# Right plot: Chapter averages
p_right <- ggplot(chapter_summary, aes(x = CHAPTER_NAME, y = EFFECT_DIFF_MEAN, color = CHAPTER_NAME)) +
  geom_point(shape = 15, size = 5) +
  geom_errorbar(aes(ymin = EFFECT_DIFF_MEAN - 1.96*SE_MEAN, ymax = EFFECT_DIFF_MEAN + 1.96*SE_MEAN), width = 0.2, linewidth = 1) +
  scale_color_manual(values = cb_palette, name = "Chapter", guide = "none") +
  labs(
    title = "Chapter Averages",
    x = "ATC Medication Chapter",
    y = "Mean Change in Prescription Behavior\n(Difference in Average ATT)"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = TEXT_SIZE_AXIS_TEXT),
    axis.text.y = element_text(size = TEXT_SIZE_AXIS_TEXT),
    axis.title.x = element_text(size = TEXT_SIZE_AXIS_TITLE),
    axis.title.y = element_text(size = TEXT_SIZE_AXIS_TITLE),
    plot.title = element_text(size = TEXT_SIZE_TITLE),
    legend.position = "none"
  ) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red")

# Combine plots
p_combined <- p_left + p_right + 
  plot_annotation(
    title = "Difference in Average Prescription Behavior 3 Years Before vs 3 Years After Event, by Medication Chapter",
    subtitle = sprintf("Number of medications tested: %d", nrow(dataset))
  )

# Save
ggsave(output_file, plot = p_combined, width = 16, height = 6, device = "png")