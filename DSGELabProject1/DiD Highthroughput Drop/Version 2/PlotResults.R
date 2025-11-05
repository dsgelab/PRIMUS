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
DATE = "20251028"
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

# Create a combined significance variable with three levels
data_diag$SIG_TYPE <- case_when(
  data_diag$SIGNIFICANT_FDR ~ "FDR Significant",
  data_diag$PVAL < 0.05 ~ "P-value Significant",
  TRUE ~ "Not Significant"
)

# Convert to factor with explicit levels to avoid alphabetical sorting
data_diag$SIG_TYPE <- factor(data_diag$SIG_TYPE, levels = c("FDR Significant", "P-value Significant", "Not Significant"))

# Sort GROUP alphabetically for x axis
data_diag$GROUP <- factor(data_diag$GROUP, levels = sort(unique(data_diag$GROUP)))

# ICD-10 Chapter names mapping
# based on this: https://icd.who.int/browse10/2019/en
icd10_chapter_map <- c(
  "A" = "Certain infectious and parasitic diseases",
  "B" = "Certain infectious and parasitic diseases",
  "C" = "Malignant neoplasms",
  "D" = "Benign or uncertain neoplasms",
  "E" = "Endocrine, nutritional and metabolic diseases",
  "F" = "Mental and behavioural disorders",
  "G" = "Diseases of the nervous system",
  "H" = "Diseases of the eye and ear",
  "I" = "Diseases of the circulatory system",
  "J" = "Diseases of the respiratory system",
  "K" = "Diseases of the digestive system",
  "L" = "Diseases of the skin and subcutaneous tissue",
  "M" = "Diseases of the musculoskeletal system and connective tissue",
  "N" = "Diseases of the genitourinary system",
  "O" = "Pregnancy, childbirth and the puerperium",
  "P" = "Certain conditions originating in the perinatal period",
  "Q" = "Congenital malformations, deformations and chromosomal abnormalities",
  "R" = "Symptoms, signs and abnormal clinical and laboratory findings, not elsewhere classified",
  "S" = "Injuries",
  "T" = "Poisoning and certain other consequences of external causes",
  "U" = "Codes for special purposes",
  "V" = "Transport accidents",
  "W" = "Other external causes of accidental injury",
  "X" = "Other external causes of accidental injury",
  "Z" = "Factors influencing health status and contact with health services"
)

# Map chapter letters to full names
data_diag$CHAPTER_NAME <- factor(icd10_chapter_map[as.character(data_diag$GROUP)], levels = icd10_chapter_map[sort(unique(as.character(data_diag$GROUP)))])

# Calculate chapter-level averages (inverse-variance weighted mean of the ATT estimates)
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
# Identify top 10 extreme values by absolute ATT_DROP OR significant ones
top_extreme <- data_diag %>%
  filter(abs(ATT_DROP) >= sort(abs(ATT_DROP), decreasing = TRUE)[10] | PVAL < 0.05)

# Set seed for reproducible jittering
set.seed(42)

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
TEXT_SIZE_AXIS_TEXT <- 10
TEXT_SIZE_LEGEND <- 12

# Add jittered positions to both datasets
data_diag$x_jittered <- as.numeric(data_diag$CHAPTER_NAME) + runif(nrow(data_diag), -JITTER_RANGE, JITTER_RANGE)
top_extreme$x_jittered <- data_diag$x_jittered[match(interaction(top_extreme$CHAPTER_NAME, top_extreme$EVENT_CODE), interaction(data_diag$CHAPTER_NAME, data_diag$EVENT_CODE))]

p_left <- ggplot(data_diag, aes(x = x_jittered, y = ATT_DROP, color = CHAPTER_NAME)) +
  geom_point(aes(shape = SIG_TYPE, size = SIG_TYPE, alpha = SIG_TYPE, fill = SIG_TYPE)) +
  geom_text_repel(data = top_extreme, aes(label = gsub("Diag_", "", EVENT_CODE)), 
            size = 4, 
            show.legend = FALSE,
            max.overlaps = Inf,
            min.segment.length = 0,
            box.padding = 0.5,
            point.padding = 0.3,
            force = 2,
            force_pull = 0.5) +
  scale_x_continuous(
    breaks = 1:length(levels(data_diag$CHAPTER_NAME)),
    labels = levels(data_diag$CHAPTER_NAME)
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
    title = "Individual Diagnoses",
    x = "ICD-10 Diagnosis Chapter",
    y = "Change in Total Prescriptions\n(DiD Estimate)"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 60, hjust = 1, size = TEXT_SIZE_AXIS_TEXT),
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
p_right <- ggplot(chapter_summary, aes(x = CHAPTER_NAME, y = ATT_MEAN, color = CHAPTER_NAME)) +
  geom_point(shape = 15, size = POINT_SIZE_FDR) +
  geom_errorbar(aes(ymin = ATT_MEAN - 1.96*SE_MEAN, ymax = ATT_MEAN + 1.96*SE_MEAN), width = 0.2) +
  scale_color_manual(values = cb_palette, name = "Chapter", guide = "none") +
  labs(
    title = "Chapter Averages",
    x = "ICD-10 Diagnosis Chapter",
    y = "Mean Change in Total Prescriptions\n(DiD Estimate)"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 60, hjust = 1, size = TEXT_SIZE_AXIS_TEXT),
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
    title = "Drop in Total Prescriptions due to Health Event, by Diagnosis Chapter",
    subtitle = sprintf("Number of diagnosis tested: %d", nrow(data_diag))
  )

# Save
ggsave(filename = paste0(OutDir, "DropPlot_", DATE, ".png"), plot = p_combined, width = 19, height = 12, dpi = 300, device = "png")
