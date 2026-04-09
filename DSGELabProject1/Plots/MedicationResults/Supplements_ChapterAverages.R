.libPaths("/shared-directory/sd-tools/apps/R/lib/")

# ============================================================================
# LIBRARIES
# ============================================================================

library(ggplot2)
library(dplyr)
library(readr)

# ============================================================================
# PATHS & SETTINGS
# ============================================================================

DATE         <- "20260316"
dataset_file <- paste0('/media/volume/Projects/DSGELabProject1/DiD_Experiments/',
                       'DiD_Medications_', DATE, '/Results_', DATE,
                       '/Results_ATC_', DATE, '.csv')
OutDir       <- paste0("/media/volume/Projects/DSGELabProject1/Plots/Results_", DATE, "/")
if (!dir.exists(OutDir)) dir.create(OutDir, recursive = TRUE)

JITTER_RANGE <- 0.2

# ============================================================================
# SHARED REFERENCE DATA
# ============================================================================

atc_chapter_map <- c(
    "A" = "Alimentary Tract and Metabolism",
    "B" = "Blood and Blood Forming Organs",
    "C" = "Cardiovascular System",
    "D" = "Dermatologicals",
    "G" = "Genito Urinary System and Sex Hormones",
    "H" = "Systemic Hormonal Preparations, \nExcl. Sex Hormones and Insulins",
    "J" = "Antiinfectives for Systemic Use",
    "L" = "Antineoplastic and Immunomodulating Agents",
    "M" = "Musculo-Skeletal System",
    "N" = "Nervous System",
    "P" = "Antiparasitic Products, \nInsecticides and Repellents",
    "R" = "Respiratory System",
    "S" = "Sensory Organs",
    "V" = "Various"
)

cb_palette <- c(
    "#E69F00",  # A - Alimentary Tract and Metabolism
    "#56B4E9",  # B - Blood and Blood Forming Organs
    "#009E73",  # C - Cardiovascular System
    "#D55E00",  # D - Dermatologicals
    "#CC79A7",  # G - Genito Urinary System and Sex Hormones
    "#0072B2",  # H - Systemic Hormonal Preparations
    "#F0E442",  # J - Antiinfectives for Systemic Use
    "#999999",  # L - Antineoplastic and Immunomodulating Agents
    "#E7298A",  # M - Musculo-Skeletal System
    "#7570B3",  # N - Nervous System
    "#66A61E",  # P - Antiparasitic Products
    "#A6761D",  # R - Respiratory System
    "#999999",  # S - Sensory Organs
    "#E6AB02"   # V - Various
)

# ============================================================================
# LOAD & PREPARE DATA
# ============================================================================

dataset <- read_csv(dataset_file, show_col_types = FALSE)

# Filter only codes with at least 300 cases
dataset <- dataset[dataset$N_CASES >= 300, ]

# Multiple test correction
dataset$PVAL_ADJ           <- p.adjust(dataset$PVAL_ABS_CHANGE, method = "bonferroni")
dataset$SIGNIFICANT_CHANGE <- dataset$PVAL_ADJ < 0.05

# Annotate ATC chapter — preserve original alphabetical-by-letter order
dataset <- dataset %>%
    mutate(
        MED_CHAPTER  = substr(OUTCOME_CODE, 1, 1),
        CHAPTER_NAME = atc_chapter_map[MED_CHAPTER]
    ) %>%
    filter(!is.na(CHAPTER_NAME))

dataset$CHAPTER_NAME <- factor(
    dataset$CHAPTER_NAME,
    levels = unique(atc_chapter_map[sort(unique(dataset$MED_CHAPTER))])
)

chapter_color_map <- setNames(
    cb_palette[seq_len(nlevels(dataset$CHAPTER_NAME))],
    levels(dataset$CHAPTER_NAME)
)

# ============================================================================
# JITTERED INDIVIDUAL DOTS
# ============================================================================

set.seed(1)
dataset$x_jittered <- as.numeric(dataset$CHAPTER_NAME) +
    runif(nrow(dataset), -JITTER_RANGE, JITTER_RANGE)

# ============================================================================
# PLOT (horizontal: chapters on x-axis, matching Figure 5 orientation)
# ============================================================================

p_supp <- ggplot(dataset, aes(x = CHAPTER_NAME, y = ABS_CHANGE, colour = CHAPTER_NAME, fill = CHAPTER_NAME)) +
    # Reference line at 0
    geom_hline(yintercept = 0, linetype = "dashed", colour = "grey50", linewidth = 0.5) +
    # Background: individual jittered transparent dots
    geom_point(
        aes(x = x_jittered),
        shape = 16,
        size  = 1.5,
        alpha = 0.2
    ) +
    # Boxplot on top (transparent fill so dots show through)
    geom_boxplot(
        aes(x = as.numeric(CHAPTER_NAME)),
        width    = 0.45,
        alpha    = 0.3,
        outlier.shape = NA,   # outliers already visible as jittered dots
        linewidth = 0.6
    ) +
    scale_x_continuous(
        breaks = seq_along(levels(dataset$CHAPTER_NAME)),
        labels = levels(dataset$CHAPTER_NAME)
    ) +
    scale_colour_manual(values = chapter_color_map, guide = "none") +
    scale_fill_manual(values = chapter_color_map, guide = "none") +
    labs(
        x     = NULL,
        y     = "Absolute Change in Prescription Rate",
        title = "Distribution of Absolute Change by ATC Chapter"
    ) +
    theme_minimal() +
    theme(
        axis.text.x        = element_text(size = 10, angle = 45, hjust = 1),
        axis.text.y        = element_text(size = 10),
        axis.title.y       = element_text(size = 12),
        plot.title         = element_text(size = 14, face = "bold"),
        panel.grid.major.y = element_line(colour = "grey93"),
        panel.grid.minor   = element_blank(),
        plot.margin        = margin(8, 12, 8, 8)
    )

# ============================================================================
# SAVE
# ============================================================================

ggsave(
    filename = paste0(OutDir, "Supplements_ChapterAverages.png"),
    plot     = p_supp,
    width    = 14,
    height   = 10,
    dpi      = 300,
    device   = "png"
)

# Calculate summary statistics by chapter
summary_stats <- dataset %>%
    group_by(CHAPTER_NAME) %>%
    summarise(
        N = n(),
        Mean = mean(ABS_CHANGE, na.rm = TRUE),
        Median = median(ABS_CHANGE, na.rm = TRUE),
        SD = sd(ABS_CHANGE, na.rm = TRUE),
        Q1 = quantile(ABS_CHANGE, 0.25, na.rm = TRUE),
        Q3 = quantile(ABS_CHANGE, 0.75, na.rm = TRUE),
        IQR = Q3 - Q1,
        Min = min(ABS_CHANGE, na.rm = TRUE),
        Max = max(ABS_CHANGE, na.rm = TRUE),
        .groups = "drop"
    )

write_csv(summary_stats, paste0(OutDir, "Supplements_ChapterAverages_Table.csv"))