# Libraries
library(ggplot2)
library(dplyr)
library(readr)
library(tidyr)
library(scales)
library(patchwork)
library(ggrepel)

# Global Variables
DATE = "20260129"
dataset_file <- paste0('/media/volume/Projects/DSGELabProject1/DiD_Experiments/DiD_Medications_', DATE, '/Results_', DATE, '/Results_ATC_', DATE, '.csv')
OutDir <- paste0("/media/volume/Projects/DSGELabProject1/Plots/Figure5/")
if (!dir.exists(OutDir)) {dir.create(OutDir, recursive = TRUE)}

# ============================================================================
# Absolute change plot, with only significant points labeled
# ============================================================================

dataset <- read_csv(dataset_file, show_col_types = FALSE)
# Filter only codes with at least 300 cases available
dataset <- dataset[dataset$N_CASES >= 300, ]

# STEP 1:
# Apply FDR multiple testing correction to change p-values
dataset$PVAL_ADJ_FDR <- p.adjust(dataset$PVAL_ABS_CHANGE, method = "bonferroni")
dataset$SIGNIFICANT_CHANGE <- dataset$PVAL_ADJ_FDR < 0.05

# STEP 2:
# Select only robust results, i.e those point / events with:
# A. an average prescription rate before event significantly non-different from controls
# B. an average prescription rate after event significantly different from controls
# Also apply FDR multiple testing correction here
dataset$PVAL_PRE_ADJ_FDR <- p.adjust(dataset$PVAL_PRE, method = "bonferroni")
dataset$PVAL_POST_ADJ_FDR <- p.adjust(dataset$PVAL_POST, method = "bonferroni")    
dataset$SIGNIFICANT_ROBUST <- (dataset$PVAL_PRE_ADJ_FDR >= 0.05) & (dataset$PVAL_POST_ADJ_FDR < 0.05)

# STEP 3:
# Create a combined significance variable with two levels
dataset$SIG_TYPE <- case_when(
  dataset$SIGNIFICANT_CHANGE & dataset$SIGNIFICANT_ROBUST ~ "Significant",
  TRUE ~ "Not Significant"
)

# Convert to factor with explicit levels
dataset$SIG_TYPE <- factor(dataset$SIG_TYPE, levels = c("Significant", "Not Significant"))
# Extract medication chapter from OUTCOME_CODE
dataset <- dataset %>% mutate(MED_CHAPTER = substr(OUTCOME_CODE, 1, 1))
# Sort MED_CHAPTER alphabetically for x axis
dataset$MED_CHAPTER <- factor(dataset$MED_CHAPTER, levels = sort(unique(dataset$MED_CHAPTER)))
# ATC Chapter names mapping
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

# Calculate chapter-level averages, weighted by inverse variance (1 / SE^2)
chapter_summary <- dataset %>%
  group_by(CHAPTER_NAME) %>%
  summarise(
    weight_sum = sum(1 / (ABS_CHANGE_SE^2)),
    ABS_CHANGE_MEAN = sum(ABS_CHANGE / (ABS_CHANGE_SE^2)) / weight_sum,
    SE_MEAN = sqrt(1 / weight_sum),
    .groups = "drop"
  )

# Color-blind friendly palette
cb_palette <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", 
                "#D55E00", "#CC79A7", "#999999", "#000000", "#E6AB02",
                "#7570B3", "#66A61E", "#E7298A", "#A6761D", "#666666",
                "#1B9E77", "#D95F02", "#7570B3", "#E7298A", "#66A61E", "#E6AB02")

# Select medications to be labelled 
# print(dataset[dataset$SIG_TYPE == "Significant", ])
code_labels <- tibble(
    OUTCOME_CODE = c(
        "M01AH05",
        "N05CF02",
        "C10AA07",
        "N02CC07",
        "N02BE01",
        "N06AX26",
        "R01AD58"),
    LABEL = c(
        "etoricoxib",
        "zolpidem", 
        "rosuvastatin", 
        "frovatriptan",
        "paracetamol",     
        "vortioxetine",
        "fluticasone, combinations")
)
robust_result_labels <- dataset %>% inner_join(code_labels, by = "OUTCOME_CODE")

# Set seed for reproducible jittering
set.seed(1)
# Global Variables for Plotting
JITTER_RANGE <- 0.2
POINT_SIZE_SIG <- 4
POINT_SIZE_NOT_SIG <- 2
ALPHA_SIG <- 1
ALPHA_NOT_SIG <- 0.2
TEXT_SIZE_TITLE <- 16
TEXT_SIZE_AXIS_TITLE <- 14
TEXT_SIZE_AXIS_TEXT <- 10
TEXT_SIZE_LEGEND <- 12

# Add jittered positions to both datasets
dataset$x_jittered <- as.numeric(dataset$CHAPTER_NAME) + runif(nrow(dataset), -JITTER_RANGE, JITTER_RANGE)
robust_result_labels$x_jittered <- dataset$x_jittered[match(interaction(robust_result_labels$CHAPTER_NAME, robust_result_labels$OUTCOME_CODE), interaction(dataset$CHAPTER_NAME, dataset$OUTCOME_CODE))]

p_left <- ggplot(dataset, aes(x = x_jittered, y = ABS_CHANGE, color = CHAPTER_NAME)) +
  geom_point(aes(shape = SIG_TYPE, size = SIG_TYPE, alpha = SIG_TYPE)) +
  geom_text_repel(data = robust_result_labels, aes(label = paste0(OUTCOME_CODE, ": ", LABEL)), 
                    size = 4, 
                    show.legend = FALSE,
                    max.overlaps = Inf,
                    min.segment.length = 0,
                    box.padding = 0.8,
                    point.padding = 0.5,
                    force = 3,
                    force_pull = 1,
                    segment.size = 0.5,
                    segment.alpha = 0.6) +
  scale_x_continuous(
    breaks = 1:length(levels(dataset$CHAPTER_NAME)),
    labels = levels(dataset$CHAPTER_NAME)
  ) +
  scale_color_manual(values = cb_palette, name = "Chapter", guide = "none") +
  scale_shape_manual(
    name = "Significance",
    values = c("Significant" = 17, "Not Significant" = 16)
  ) +
  scale_size_manual(
    name = "Significance",
    values = c("Significant" = POINT_SIZE_SIG, "Not Significant" = POINT_SIZE_NOT_SIG)
  ) +
  scale_alpha_manual(
    name = "Significance",
    values = c("Significant" = ALPHA_SIG, "Not Significant" = ALPHA_NOT_SIG)
  ) +
  labs(
    title = "Individual Medications",
    x = "ATC Medication Chapter",
    y = "Absolute Change in Prescription Rate\n(Average 3-years After - Average 3 Years Before Event)"
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

# Right plot: Chapter averages
p_right <- ggplot(chapter_summary, aes(x = CHAPTER_NAME, y = ABS_CHANGE_MEAN, color = CHAPTER_NAME)) +
  geom_point(shape = 15, size = 5) +
  geom_errorbar(aes(ymin = ABS_CHANGE_MEAN - 1.96*SE_MEAN, ymax = ABS_CHANGE_MEAN + 1.96*SE_MEAN), width = 0.2, linewidth = 1) +
  scale_color_manual(values = cb_palette, name = "Chapter", guide = "none") +
  labs(
    title = "Chapter Averages",
    x = "ATC Medication Chapter",
    y = "Absolute Change in Prescription Rate\n(Average 3-years After - Average 3 Years Before Event)"
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
    title = "Change in Prescription Behavior due to Event, by Medication Chapter",
    subtitle = sprintf("Number of medications tested: %d", nrow(dataset))
  )

# Save
ggsave(paste0(OutDir, "Figure5A_", DATE, ".png"), plot = p_combined, width = 24, height = 14, dpi = 300, device = "png")