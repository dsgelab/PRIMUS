# Load required libraries
library(ggplot2)
library(dplyr)
library(tidyr)
library(scales)
library(gridExtra)
library(patchwork)
library(viridis)
library(ggrepel)
library(readr)

# Load data
DATE = "20260217"
dataset_file <- paste0('/media/volume/Projects/DSGELabProject1/DiD_Experiments/DiD_Diagnosis_', DATE, '/Results_', DATE, '/Results_ICD_', DATE, '.csv')
OutDir <- paste0("/media/volume/Projects/DSGELabProject1/Plots/Figure5/")
if (!dir.exists(OutDir)) {dir.create(OutDir, recursive = TRUE)}

# ============================================================================
# Absolute drop plot, with only significant points labeled
# ============================================================================

dataset <- read_csv(dataset_file, show_col_types = FALSE)
# Filter only codes with at least 300 cases available
dataset <- dataset[dataset$N_CASES >= 300, ]

# STEP 1:
# Apply multiple testing correction to p-values
dataset$PVAL <- 2 * (1 - pnorm(abs(dataset$ATT_DROP / dataset$SE_DROP)))
dataset$PVAL_ADJ <- p.adjust(dataset$PVAL, method = "bonferroni")
dataset$SIGNIFICANT <- dataset$PVAL_ADJ < 0.05

# STEP 2:
# Create a combined significance variable with two levels
dataset$SIG_TYPE <- case_when(
  dataset$SIGNIFICANT  ~ "Significant",
  TRUE ~ "Not Significant"
)

# Convert to factor with explicit levels
dataset$SIG_TYPE <- factor(dataset$SIG_TYPE, levels = c("Significant", "Not Significant"))
# Extract medication chapter from OUTCOME_CODE
dataset$EVENT_CODE <- substr(sub(".*_", "", dataset$EVENT_CODE), 1, 1)
dataset <- dataset %>% mutate(MED_CHAPTER = substr(EVENT_CODE, 1, 1))

# Sort MED_CHAPTER alphabetically for x axis
dataset$MED_CHAPTER <- factor(dataset$MED_CHAPTER, levels = sort(unique(dataset$MED_CHAPTER)))
# ICD-10 Chapter names mapping
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
dataset$CHAPTER_NAME <- factor(icd10_chapter_map[as.character(dataset$MED_CHAPTER)], levels = icd10_chapter_map[sort(unique(as.character(dataset$MED_CHAPTER)))])

# Calculate chapter-level averages
chapter_summary <- dataset %>%
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

# Select dieases to be labelled
code_labels <- tibble(
    EVENT_CODE = c(
            "Diag_C50", "Diag_F33", "Diag_F43", "Diag_I80", 
            "Diag_O80", "Diag_O82", "Diag_O42", "Diag_O02",
            "Diag_Z34", "Diag_Z36", "Diag_Z73"
    ),
    LABEL = c(
            "Malignant neoplasm of breast", "Recurrent depressive disorder", "Severe stress and adjustment disorders", "Phlebitis and thrombophlebitis", 
            "Single spontaneous delivery", "Single delivery by caesarean section", "Premature rupture of membranes", "Other abnormal products of conception",
            "Supervision of normal pregnancy", "Antenatal screening", "Problems related to life-management difficulty"
    )
)
robust_result_labels <- dataset %>% inner_join(code_labels, by = "EVENT_CODE")

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
robust_result_labels$x_jittered <- dataset$x_jittered[match(interaction(robust_result_labels$CHAPTER_NAME, robust_result_labels$EVENT_CODE), interaction(dataset$CHAPTER_NAME, dataset$EVENT_CODE))]

p_left <- ggplot(dataset, aes(x = x_jittered, y = ATT_DROP, color = CHAPTER_NAME)) +
  geom_point(aes(shape = SIG_TYPE, size = SIG_TYPE, alpha = SIG_TYPE)) +
  geom_text_repel(data = robust_result_labels, aes(label = paste0(EVENT_CODE, ": ", LABEL)), 
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
    title = "Individual Diseases",
    x = "ICD Disease Chapter",
    y = "Absolute Drop in Total Prescription\n(Within the event year) "
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
p_right <- ggplot(chapter_summary, aes(x = CHAPTER_NAME, y = ATT_MEAN, color = CHAPTER_NAME)) +
  geom_point(shape = 15, size = 5) +
  geom_errorbar(aes(ymin = ATT_MEAN - 1.96*SE_MEAN, ymax = ATT_MEAN + 1.96*SE_MEAN), width = 0.2, linewidth = 1) +
  scale_color_manual(values = cb_palette, name = "Chapter", guide = "none") +
  labs(
    title = "Chapter Averages",
    x = "ICD Disease Chapter",
    y = "Absolute Drop in Total Prescription\n(Within the event year) "
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
    title = "Drop in Total Prescriptions  due to Event, by Disease Chapter",
    subtitle = sprintf("Number of diseases tested: %d", nrow(dataset))
  )

# Save
ggsave(paste0(OutDir, "Figure3A_", DATE, ".png"), plot = p_combined, width = 24, height = 14, dpi = 300, device = "png")