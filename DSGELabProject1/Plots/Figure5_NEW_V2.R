.libPaths("/shared-directory/sd-tools/apps/R/lib/")

# ============================================================================
# LIBRARIES
# ============================================================================

library(ggplot2)
library(dplyr)
library(readr)
library(tidyr)
library(scales)
library(gridExtra)   
library(grid)        
library(ggrepel)
library(metafor)


# ============================================================================
# GLOBAL SETTINGS: Paths, dates, plot constants
# ============================================================================

DATE <- "20260129"
dataset_file <- paste0(
    '/media/volume/Projects/DSGELabProject1/DiD_Experiments/',
    'DiD_Medications_', DATE,
    '_FE_MetaAnalysis/Results_', DATE,
    '/Results_ATC_', DATE, '.csv'
)
relative_change_file <- paste0(
    "/media/volume/Projects/DSGELabProject1/Plots/Supplements/",
    "Supplementary_RelativeChangeEstimates_20260129.csv"
)
validation_data_file <- paste0(
    "/media/volume/Projects/DSGELabProject1/Plots/Supplements/",
    "TempData/temp_results_20260203.csv"
)
OutDir <- "/media/volume/Projects/DSGELabProject1/Plots/Figure5/"
if (!dir.exists(OutDir)) dir.create(OutDir, recursive = TRUE)

# Plot size constants
JITTER_RANGE        <- 0.2
POINT_SIZE_SIG      <- 4
POINT_SIZE_NOT_SIG  <- 2
ALPHA_SIG           <- 1
ALPHA_NOT_SIG       <- 0.2
TEXT_SIZE_TITLE     <- 16
TEXT_SIZE_AXIS_TITLE <- 14
TEXT_SIZE_AXIS_TEXT  <- 10
TEXT_SIZE_LEGEND     <- 12


# ============================================================================
# SHARED REFERENCE DATA: ATC chapter map, color palette, medication labels
# ============================================================================

# Full ATC chapter names keyed by single-letter code
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

# Color-blind friendly palette (one color per chapter)
cb_palette <- c(
    "#E69F00", "#56B4E9", "#000000", "#F0E442", "#0072B2",
    "#D55E00", "#E7298A", "#999999", "#CC79A7", "#E6AB02",
    "#7570B3", "#66A61E", "#009E73", "#A6761D", "#666666"
)

# Medications of interest: ATC code → readable label
code_labels <- tibble(
    OUTCOME_CODE = c(
        "M01AH05", "N05CF02", "C10AA07", "N02CC07",
        "N02BE01", "N06AX26", "R01AD58"
    ),
    LABEL = c(
        "etoricoxib", "zolpidem", "rosuvastatin", "frovatriptan",
        "paracetamol", "vortioxetine", "fluticasone, combinations"
    )
)


# ============================================================================
# SECTION 1: MAIN DATASET — absolute change analysis (ATC chapter level)
# ============================================================================

dataset <- read_csv(dataset_file, show_col_types = FALSE)

# Keep only codes with at least 300 cases
dataset <- dataset %>% filter(N_CASES >= 300)

# --- Significance flags ---

# Bonferroni-corrected p-value for the absolute change
dataset$PVAL_ADJ_FDR      <- p.adjust(dataset$PVAL_ABS_CHANGE, method = "bonferroni")
dataset$SIGNIFICANT_CHANGE <- dataset$PVAL_ADJ_FDR < 0.05

# Robustness check:
#   PRE  test: prescription rate before event NOT significantly different from controls (p >= 0.05)
#   POST test: prescription rate after  event IS significantly different from controls (p < 0.05)
dataset$PVAL_PRE_ADJ_FDR  <- p.adjust(dataset$PVAL_PRE,  method = "bonferroni")
dataset$PVAL_POST_ADJ_FDR <- p.adjust(dataset$PVAL_POST, method = "bonferroni")
dataset$SIGNIFICANT_ROBUST <- (dataset$PVAL_PRE_ADJ_FDR >= 0.05) & (dataset$PVAL_POST_ADJ_FDR < 0.05)

# Combined significance label (used for shape/size/alpha mapping)
dataset$SIG_TYPE <- factor(
    case_when(
        dataset$SIGNIFICANT_CHANGE & dataset$SIGNIFICANT_ROBUST ~ "Significant",
        TRUE ~ "Not Significant"
    ),
    levels = c("Significant", "Not Significant")
)

# --- Chapter annotation ---

dataset <- dataset %>%
    mutate(
        MED_CHAPTER  = substr(OUTCOME_CODE, 1, 1),
        CHAPTER_NAME = atc_chapter_map[MED_CHAPTER]
    ) %>%
    filter(!is.na(CHAPTER_NAME))

# Order chapter factor levels alphabetically by letter and assign colors
dataset$CHAPTER_NAME <- factor(
    dataset$CHAPTER_NAME,
    levels = unique(atc_chapter_map[sort(unique(dataset$MED_CHAPTER))])
)
chapter_color_map <- setNames(
    cb_palette[1:nlevels(dataset$CHAPTER_NAME)],
    levels(dataset$CHAPTER_NAME)
)

# Pull out the labeled medication rows for text annotations in p1
robust_result_labels <- dataset %>% inner_join(code_labels, by = "OUTCOME_CODE")


# ============================================================================
# PANEL 1 (left, full height): Jittered boxplot of absolute change by chapter
# ============================================================================

# Reproducible jitter positions stored on the dataset
set.seed(1)
dataset$x_jittered <- as.numeric(dataset$CHAPTER_NAME) +
    runif(nrow(dataset), -JITTER_RANGE, JITTER_RANGE)

# Match jittered x positions onto the labeled subset
robust_result_labels$x_jittered <- dataset$x_jittered[
    match(
        interaction(robust_result_labels$CHAPTER_NAME, robust_result_labels$OUTCOME_CODE),
        interaction(dataset$CHAPTER_NAME, dataset$OUTCOME_CODE)
    )
]

p1 <- ggplot(dataset, aes(x = x_jittered, y = ABS_CHANGE, color = CHAPTER_NAME)) +
  geom_point(aes(shape = SIG_TYPE, size = SIG_TYPE, alpha = SIG_TYPE)) +
  geom_text_repel(data = robust_result_labels, aes(label = LABEL),
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
    # title removed — panel label supplied via textGrob below
    x = NULL,
    y = "Absolute Change in Prescription Rate"
  ) +
  theme_minimal() +
  theme(
    axis.text.x  = element_text(size = TEXT_SIZE_AXIS_TEXT),
    axis.text.y  = element_text(size = TEXT_SIZE_AXIS_TEXT),
    axis.title.y = element_text(size = TEXT_SIZE_AXIS_TITLE),
    axis.title.x = element_text(size = TEXT_SIZE_AXIS_TITLE),
    legend.position = "none"
  ) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "darkgrey") +
  coord_flip()


# ============================================================================
# SECTION 2: RELATIVE CHANGE FILE — absolute and relative summaries per code
# ============================================================================

rel_data <- read_csv(relative_change_file, show_col_types = FALSE)

# Compute SE and 95% CI for both absolute and relative change
rel_data <- rel_data %>%
    mutate(
        RELATIVE_CHANGE_SE     = abs(ABS_CHANGE_SE / BASELINE_MEAN),
        RELATIVE_CHANGE_CI_LOW = RELATIVE_CHANGE_MEAN - 1.96 * RELATIVE_CHANGE_SE,
        RELATIVE_CHANGE_CI_UP  = RELATIVE_CHANGE_MEAN + 1.96 * RELATIVE_CHANGE_SE,
        ABS_CHANGE_CI_LOW      = ABS_CHANGE - 1.96 * ABS_CHANGE_SE,
        ABS_CHANGE_CI_UP       = ABS_CHANGE + 1.96 * ABS_CHANGE_SE
    )

# Annotate each code with its ATC chapter name and readable medication label
rel_data <- rel_data %>%
    mutate(
        MED_CHAPTER  = substr(OUTCOME_CODE, 1, 1),
        CHAPTER_NAME = factor(atc_chapter_map[MED_CHAPTER], levels = levels(dataset$CHAPTER_NAME))
    ) %>%
    left_join(code_labels, by = "OUTCOME_CODE") %>%
    mutate(LABEL = paste0(LABEL, "\n(ATC: ", OUTCOME_CODE, ")"))


# ============================================================================
# PANEL 2 (top right): Absolute change summary — dot + error bar, all codes
# ============================================================================

# Compute the ordering by ABS_CHANGE once and reuse it in both panels
# so that absolute and relative change plots share the same y-axis order.
# Use LABEL (readable name) as the display variable, ordered by ABS_CHANGE.
abs_order      <- order(rel_data$ABS_CHANGE)
ordered_labels <- rel_data$LABEL[abs_order]
rel_data$LABEL_ORDERED <- factor(rel_data$LABEL, levels = ordered_labels)

p_abs_summary <- ggplot(
    rel_data,
    aes(x = ABS_CHANGE, y = LABEL_ORDERED, color = CHAPTER_NAME)
) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "grey", linewidth = 0.8) +
    geom_point(size = 3) +
    geom_errorbarh(
        aes(xmin = ABS_CHANGE_CI_LOW, xmax = ABS_CHANGE_CI_UP),
        height = 0.2, linewidth = 0.8
    ) +
    scale_color_manual(values = chapter_color_map, name = "ATC Chapter") +
    xlim(0, 0.01) +
    labs(
        # title removed — panel label supplied via textGrob below
        x = "Absolute Change",
        y = ""
    ) +
    theme_minimal() +
    theme(
        axis.text.y  = element_text(size = TEXT_SIZE_AXIS_TEXT),
        axis.title.x = element_text(size = TEXT_SIZE_AXIS_TITLE),
        legend.position = "none"
    )


# ============================================================================
# PANEL 3 (center right): Relative change — dot + error bar, no text labels
# ============================================================================

p_ratio_combined <- ggplot(
    rel_data,
    aes(x = RELATIVE_CHANGE_MEAN, y = LABEL_ORDERED, color = CHAPTER_NAME)
) +
    geom_vline(xintercept = 1, linetype = "dashed", color = "grey", linewidth = 0.8) +
    geom_point(size = 3) +
    geom_errorbarh(
        aes(xmin = RELATIVE_CHANGE_CI_LOW, xmax = RELATIVE_CHANGE_CI_UP),
        height = 0.2, linewidth = 0.8
    ) +
    scale_color_manual(values = chapter_color_map, name = "ATC Chapter") +
    xlim(1, 8) +
    labs(
        # title removed — panel label supplied via textGrob below
        x = "Relative Change",
        y = ""
    ) +
    theme_minimal() +
    theme(
        axis.text.y  = element_text(size = TEXT_SIZE_AXIS_TEXT),
        axis.title.x = element_text(size = TEXT_SIZE_AXIS_TITLE),
        legend.position = "none"
    )


# ============================================================================
# SECTION 3: VALIDATION DATA — time-series (att) per selected medication
# ============================================================================

val_data <- read_csv(validation_data_file, show_col_types = FALSE)
val_data <- val_data %>%
    mutate(
        MED_CHAPTER  = substr(code, 1, 1),
        CHAPTER_NAME = atc_chapter_map[MED_CHAPTER]
    )

# Build the pooled time-series dataset for all selected codes.
# For each code: estimate fixed-effects meta-analytic average ATT
# over the pre-period (t = -3, -2, -1) and post-period (t = 1, 2, 3).
data_plot_all <- data.frame()

for (code in sort(unique(val_data$code), decreasing = TRUE)) {

    temp_results <- val_data %>% filter(code == !!code)

    # Index pre- and post-event time points
    before_idx <- temp_results$time %in% c(-3, -2, -1)
    after_idx  <- temp_results$time %in% c( 1,  2,  3)

    # Fixed-effects meta-analysis for pre-period
    pre_meta <- metafor::rma(
        yi  = temp_results$att[before_idx],
        sei = temp_results$se[before_idx],
        method = "FE"
    )
    avg_effect_before <- pre_meta$b[, 1]
    ci_pre            <- c(pre_meta$ci.lb, pre_meta$ci.ub)

    # Fixed-effects meta-analysis for post-period
    post_meta <- metafor::rma(
        yi  = temp_results$att[after_idx],
        sei = temp_results$se[after_idx],
        method = "FE"
    )
    avg_effect_after <- post_meta$b[, 1]
    ci_post          <- c(post_meta$ci.lb, post_meta$ci.ub)

    # Keep only t = -3 … +3, attach pooled estimates and labels
    data_plot <- temp_results %>%
        filter(time >= -3, time <= 3) %>%
        mutate(
            avg_effect_before = avg_effect_before,
            avg_effect_after  = avg_effect_after,
            ci_pre_lower      = ci_pre[1],
            ci_pre_upper      = ci_pre[2],
            ci_post_lower     = ci_post[1],
            ci_post_upper     = ci_post[2],
            facet_label       = code_labels$LABEL[code_labels$OUTCOME_CODE == code],
            CHAPTER_NAME      = val_data$CHAPTER_NAME[val_data$code == code][1]
        )

    data_plot_all <- rbind(data_plot_all, data_plot)
}


# ============================================================================
# PANEL 4 (bottom right): Rosuvastatin (C10AA07) time-series — exact replica
# ============================================================================

# Filter data to rosuvastatin only
data_plot_ros <- data_plot_all %>% filter(code == "C10AA07")
ros_chapter   <- data_plot_ros$CHAPTER_NAME[1]
ros_color     <- chapter_color_map[[ros_chapter]]

p_rosuvastatin <- ggplot(data_plot_ros, aes(x = time, y = att)) +
    # Main time-series line and points
    geom_line(color = ros_color) +
    geom_point(
        aes(alpha = ifelse(time == 0, 0.3, 1)),
        color = ros_color, size = 2
    ) +
    geom_errorbar(
        aes(
            ymin  = att - 1.96 * se,
            ymax  = att + 1.96 * se,
            alpha = ifelse(time == 0, 0.3, 1)
        ),
        color = ros_color, width = 0.2, linewidth = 1
    ) +
    # Reference lines
    geom_hline(yintercept = 0, linetype = "dashed", color = "grey") +
    geom_vline(xintercept = 0, linetype = "dashed", color = "grey") +
    # Pooled pre-period average and 95% CI (red horizontal segments)
    geom_segment(aes(x = -3.5, xend = -0.5, y = avg_effect_before, yend = avg_effect_before),
                 color = "red", alpha = 0.3, linewidth = 0.3) +
    geom_segment(aes(x = -3.5, xend = -0.5, y = ci_pre_lower, yend = ci_pre_lower),
                 color = "red", alpha = 0.3, linewidth = 0.3, linetype = "dashed") +
    geom_segment(aes(x = -3.5, xend = -0.5, y = ci_pre_upper, yend = ci_pre_upper),
                 color = "red", alpha = 0.3, linewidth = 0.3, linetype = "dashed") +
    # Pooled post-period average and 95% CI
    geom_segment(aes(x = 0.5, xend = 3.5, y = avg_effect_after, yend = avg_effect_after),
                 color = "red", alpha = 0.3, linewidth = 0.3) +
    geom_segment(aes(x = 0.5, xend = 3.5, y = ci_post_lower, yend = ci_post_lower),
                 color = "red", alpha = 0.3, linewidth = 0.3, linetype = "dashed") +
    geom_segment(aes(x = 0.5, xend = 3.5, y = ci_post_upper, yend = ci_post_upper),
                 color = "red", alpha = 0.3, linewidth = 0.3, linetype = "dashed") +
    scale_alpha_identity() +
    coord_cartesian(ylim = c(-0.001, 0.01)) +
    labs(
        # title removed — panel label supplied via textGrob below
        x = "Years from Event",
        y = "Prescription Rate Difference\n(compared to controls)"
    ) +
    theme_minimal()


# ============================================================================
# FINAL FIGURE: 4-panel composition using grid.arrange + textGrob
#
#   [ title_A              | title_B               ]
#   [ p1 (full height)     | p_abs_summary         ]
#   [                      | title_C               ]
#   [                      | p_ratio_combined      ]
#   [                      | title_D               ]
#   [                      | p_rosuvastatin        ]
# ============================================================================

# --- Panel title grobs (bold letter prefix + normal description text) ---
# textGrob supports R expressions for mixed bold/plain text.
# hjust = 0 + x = 0.01 gives a left-aligned title flush with the plot margin.

make_title_grob <- function(expr, fontsize = TEXT_SIZE_TITLE) {
    textGrob(
        label = expr,
        x     = 0.01,
        hjust = 0,
        gp    = gpar(fontsize = fontsize)
    )
}

title_A <- make_title_grob(expression(bold("A.") ~ "Individual Estimates"))
title_B <- make_title_grob(expression(bold("B.") ~ "Absolute Change, for Significant Medications"))
title_C <- make_title_grob(expression(bold("C.") ~ "Relative Change, for Significant Medications"))
title_D <- make_title_grob(expression(bold("D.") ~ "Zoom-in Rosuvastatin Estimate"))

# Title grobs are short; give them a fixed height relative to a plot unit.
TITLE_HEIGHT <- 0.06   # fraction of one panel height

# --- Wrap each right-column panel: title grob on top, plot below ---
panel_B <- arrangeGrob(
    title_B,
    p_abs_summary,
    nrow    = 2,
    heights = unit(c(TITLE_HEIGHT, 1 - TITLE_HEIGHT), "npc")
)

panel_C <- arrangeGrob(
    title_C,
    p_ratio_combined,
    nrow    = 2,
    heights = unit(c(TITLE_HEIGHT, 1 - TITLE_HEIGHT), "npc")
)

panel_D <- arrangeGrob(
    title_D,
    p_rosuvastatin,
    nrow    = 2,
    heights = unit(c(TITLE_HEIGHT, 1 - TITLE_HEIGHT), "npc")
)

# --- Stack the three right-column panels vertically ---
right_col <- arrangeGrob(
    panel_B,
    panel_C,
    panel_D,
    nrow = 3
)

# --- Wrap the left panel with its title ---
panel_A <- arrangeGrob(
    title_A,
    p1,
    nrow    = 2,
    heights = unit(c(TITLE_HEIGHT * (1/3), 1 - TITLE_HEIGHT * (1/3)), "npc")
    # p1 is full height so its title height fraction is proportionally smaller
)

# --- Combine left and right side by side, with a blank spacer column ---
# nullGrob() renders as empty space — mimics patchwork's plot_spacer()
p_final <- arrangeGrob(
    panel_A,
    nullGrob(),
    right_col,
    ncol   = 3,
    widths = unit(c(0.475, 0.05, 0.475), "npc")
)

# --- Save ---
ggsave(
    filename = paste0(OutDir, "Figure5_V2_20260309.png"),
    plot     = p_final,
    width    = 14,
    height   = 12,
    dpi      = 300,
    device   = "png"
)