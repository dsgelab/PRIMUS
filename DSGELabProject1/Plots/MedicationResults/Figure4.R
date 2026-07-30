
### ----------------------------------------------------------------------------
### 0. LIBRARIES
### ----------------------------------------------------------------------------

.libPaths("/shared-directory/sd-tools/apps/R/lib/")

library(ggplot2)
library(dplyr)
library(readr)
library(tidyr)
library(scales)
library(gridExtra)
library(grid)     
library(ggrepel)
library(metafor)


### ----------------------------------------------------------------------------
### 1. PARAMETERS / ARGUMENTS
### ----------------------------------------------------------------------------

# --- Significance / filtering thresholds (Panel A dataset) ---
MIN_CASES   <- 300
PADJ_METHOD <- "bonferroni"
SIG_ALPHA   <- 0.05

# --- Event-time windows used for the pre/post meta-analysis (Panel B) ---
PRE_WINDOW    <- c(-3, -2, -1)
POST_WINDOW   <- c(1, 2, 3)
CI_MULTIPLIER <- 1.96   

# --- Panel A: jitter / point styling ---
JITTER_RANGE       <- 0.2
POINT_SIZE_SIG     <- 4
POINT_SIZE_NOT_SIG <- 2
ALPHA_SIG          <- 1
ALPHA_NOT_SIG      <- 0.3

# --- Final figure export settings ---
FIG_WIDTH  <- 24
FIG_HEIGHT <- 12
FIG_DPI    <- 300

# --- Reference data: ATC chapter names, color palette, medication labels ---

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
    "#E6AB02"  # V - Various
)

# Medications of interest: ATC code -> readable label
code_labels <- tibble(
    OUTCOME_CODE = c(
        "A06AC01",
        "C10AA07",
        "M01AH05",
        "N02CC07",
        "N05CF02",
        "N06AX26",
        "R01AD12",
        "R01AD58",
        "R03AK10"
    ),
    LABEL = c(
        "psylla seeds",
        "rosuvastatin",
        "etoricoxib",
        "frovatriptan",
        "zolpidem",
        "vortioxetine",
        "fluticasone furoate",
        "fluticasone, combinations",
        "vilanterol and fluticasone furoate"
    ),
    LABEL_WITH_INDICATION = c(
        "psylla seeds \n(constipation)",
        "rosuvastatin \n(CVD, hypercholesterolemia)",
        "etoricoxib \n(acute pain, arthritis)",
        "frovatriptan \n(migraine)",
        "zolpidem \n(insomnia)",
        "vortioxetine \n(depression)",
        "fluticasone furoate \n(allergic rhinitis)",
        "fluticasone, combinations \n(allergic rhinitis)",
        "vilanterol and fluticasone furoate \n(COPD, asthma)"
    )
)


### ----------------------------------------------------------------------------
### 2. PATHS
### ----------------------------------------------------------------------------

DATE_DATA_1  <- "20260316"                    
DATE_DATA_2  <- "20260729"                    
TODAY        <- format(Sys.Date(), "%Y%m%d") 

# --- Inputs ---
dataset_file <- paste0("/media/volume/Projects/DSGELabProject1/DiD_Experiments/", "DiD_Medications_", DATE_DATA_1, "/Results_", DATE_DATA_1, "/Results_ATC_", DATE_DATA_1, ".csv")
relative_change_file <- paste0("/media/volume/Projects/DSGELabProject1/Plots/ManuscriptFinal/Supplements_RelativeChange_Estimates_", DATE_DATA_2, ".csv")
validation_data_file  <- paste0("/media/volume/Projects/DSGELabProject1/Plots/ManuscriptFinal/Supplements_DiD_LongitudinalEstimates_", DATE_DATA_2, ".csv")

# --- Output ---
OutDir <- "/media/volume/Projects/DSGELabProject1/Plots/ManuscriptFinal/"
if (!dir.exists(OutDir)) dir.create(OutDir, recursive = TRUE)

BASENAME_PLOT <- paste0("Figure4_", TODAY)

# -- Helper: save a ggplot as both PNG and PDF using the same base filename --
save_plot_png_pdf <- function(plot, dir, basename, width, height, dpi = PLOT_DPI) {
    ggsave(filename = file.path(dir, paste0(basename, ".png")), 
      plot = plot,
      width = width, 
      height = height, 
      dpi = dpi
    )
    ggsave(filename = file.path(dir, paste0(basename, ".pdf")), 
      plot = plot,
      width = width, 
      height = height
    )
}
### ----------------------------------------------------------------------------
### 3. HELPER FUNCTION
### Longitudinal DiD time-series plot for any medication code (used for Panel B).
### Identical visual style for every medication — color and title are derived
### automatically from the ATC chapter color map and the code_labels table.
### ----------------------------------------------------------------------------

make_longitudinal_plot <- function(med_code, data_all, color_map, label_tbl, show_y_title = TRUE) {

    plot_data <- data_all %>% filter(code == med_code)
    med_color <- color_map[[ plot_data$CHAPTER_NAME[1] ]]
    med_label <- label_tbl$LABEL[ label_tbl$OUTCOME_CODE == med_code ]

    abs_change_row <- dataset %>% filter(OUTCOME_CODE == med_code)
    diff_val <- abs_change_row$ABS_CHANGE[1]
    se_diff  <- abs_change_row$ABS_CHANGE_SE[1]
    diff_lo  <- diff_val - CI_MULTIPLIER * se_diff
    diff_hi  <- diff_val + CI_MULTIPLIER * se_diff

    pre_mean   <- plot_data$avg_effect_before[1]
    post_mean  <- plot_data$avg_effect_after[1]

    fmt <- function(x) formatC(x, format = "f", digits = 4)
    bracket_label <- paste0(
        "\u0394 = ", fmt(diff_val),   # \u0394 is the Greek letter delta
        "\n[", fmt(diff_lo), ", ", fmt(diff_hi), "]"
    )

    bx        <- 3.7
    tick_len  <- 0.15
    label_x   <- bx + 0.15
    label_y   <- (pre_mean + post_mean) / 2

    y_title <- if (show_y_title) "Prescription Rate Differences \n(compared to controls)" else NULL

    ggplot(plot_data, aes(x = time, y = att)) +
        geom_line(color = med_color) +
        geom_point(
            aes(alpha = ifelse(time == 0, 0.3, 1)),
            color = med_color, size = 2
        ) +
        geom_errorbar(
            aes(
                ymin  = att - CI_MULTIPLIER * se,
                ymax  = att + CI_MULTIPLIER * se,
                alpha = ifelse(time == 0, 0.3, 1)
            ),
            color = med_color, width = 0.2, linewidth = 1
        ) +
        geom_hline(yintercept = 0, linetype = "dashed", color = "grey") +
        geom_vline(xintercept = 0, linetype = "dashed", color = "grey") +

        geom_segment(aes(x = -3.5, xend = -0.5, y = avg_effect_before, yend = avg_effect_before),
                     color = "black", alpha = 0.3, linewidth = 0.3) +
        geom_segment(aes(x = -3.5, xend = -0.5, y = ci_pre_lower, yend = ci_pre_lower),
                     color = "black", alpha = 0.3, linewidth = 0.3, linetype = "dashed") +
        geom_segment(aes(x = -3.5, xend = -0.5, y = ci_pre_upper, yend = ci_pre_upper),
                     color = "black", alpha = 0.3, linewidth = 0.3, linetype = "dashed") +

        geom_segment(aes(x = 0.5, xend = 3.5, y = avg_effect_after, yend = avg_effect_after),
                     color = "black", alpha = 0.3, linewidth = 0.3) +
        geom_segment(aes(x = 0.5, xend = 3.5, y = ci_post_lower, yend = ci_post_lower),
                     color = "black", alpha = 0.3, linewidth = 0.3, linetype = "dashed") +
        geom_segment(aes(x = 0.5, xend = 3.5, y = ci_post_upper, yend = ci_post_upper),
                     color = "black", alpha = 0.3, linewidth = 0.3, linetype = "dashed") +

        annotate("segment",
                 x = bx, xend = bx, y = pre_mean, yend = post_mean,
                 color = "black", linewidth = 0.5) +
        annotate("segment",
                 x = bx - tick_len, xend = bx, y = pre_mean, yend = pre_mean,
                 color = "black", linewidth = 0.5) +
        annotate("segment",
                 x = bx - tick_len, xend = bx, y = post_mean, yend = post_mean,
                 color = "black", linewidth = 0.5) +
        annotate("text",
                 x = label_x, y = label_y,
                 label = bracket_label,
                 hjust = 0, vjust = 0.5, size = 2.5, lineheight = 0.9) +

        scale_alpha_identity() +
        coord_cartesian(ylim = c(-0.001, 0.01), xlim = c(-3.5, 3.5), clip = "off") +
        labs(
            title = med_label,
            x = "Years from Event",
            y = y_title
        ) +
        theme_minimal() +
        theme(
            plot.title   = element_text(size = 12, face = "bold", color = med_color),
            axis.title   = element_text(size = 9),
            axis.text    = element_text(size = 8),
            plot.margin  = margin(5, 55, 5, 5)
        )
}


### ----------------------------------------------------------------------------
### 4. PANEL A — DATA: absolute change analysis (ATC chapter level)
### ----------------------------------------------------------------------------

dataset <- read_csv(dataset_file, show_col_types = FALSE)
dataset <- dataset[dataset$N_CASES >= MIN_CASES, ]

# Apply multiple test correction
# Note: Using Bonferroni correction here, since it is a very conservative approach.
dataset$PVAL_ADJ <- p.adjust(dataset$PVAL_ABS_CHANGE, method = PADJ_METHOD)
dataset$SIGNIFICANT_CHANGE <- dataset$PVAL_ADJ < SIG_ALPHA

# Combined significance label (used for shape/size/alpha mapping)
dataset$SIG_TYPE <- factor(
    case_when(
        dataset$SIGNIFICANT_CHANGE ~ "Significant",
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


### ----------------------------------------------------------------------------
### 5. PANEL A — PLOT: jittered plot of absolute change by chapter
### ----------------------------------------------------------------------------

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
  geom_text_repel(data = robust_result_labels, aes(label = LABEL_WITH_INDICATION), 
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
    x = NULL,
    y = "Change in Prescription Rate \n(before vs after event, 3 year window)"
  ) +
  theme_minimal() +
  theme(
    axis.text.x  = element_text(size = 10, angle = 20, hjust = 1),
    axis.text.y  = element_text(size = 10),
    axis.title.y = element_text(size = 10),
    axis.title.x = element_text(size = 10),
    legend.position = "none"
  ) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "darkgrey")


### ----------------------------------------------------------------------------
### 6. PANEL C — relative change (with 95% CI)
### ----------------------------------------------------------------------------

rel_data <- read_csv(relative_change_file, show_col_types = FALSE)

# Annotate each code with its ATC chapter name and readable medication label
rel_data <- rel_data %>%
    mutate(
        MED_CHAPTER  = substr(OUTCOME_CODE, 1, 1),
        CHAPTER_NAME = factor(atc_chapter_map[MED_CHAPTER], levels = levels(dataset$CHAPTER_NAME))
    ) %>%
    left_join(code_labels, by = "OUTCOME_CODE")

# Sort results by their absolute change values (descending)
sorted_codes <- dataset %>%
    filter(OUTCOME_CODE %in% code_labels$OUTCOME_CODE) %>%
    arrange(desc(ABS_CHANGE)) %>%
    pull(OUTCOME_CODE)

rel_data <- rel_data %>%
    filter(OUTCOME_CODE %in% sorted_codes) %>%  
    mutate(LABEL_ORDERED = factor(LABEL, levels = rev(code_labels$LABEL))) %>%
    arrange(LABEL_ORDERED)

p_ratio_combined <- ggplot(
    rel_data,
    aes(x = REL_CHANGE, y = LABEL_ORDERED, color = CHAPTER_NAME)
) +
    geom_vline(xintercept = 1, linetype = "dashed", color = "grey", linewidth = 0.8) +
    geom_point(size = 3) +
    geom_errorbarh(
        aes(xmin = REL_CHANGE_CI_LOW, xmax = REL_CHANGE_CI_UP),
        height = 0.2, linewidth = 0.8
    ) +
    scale_color_manual(values = chapter_color_map, name = "ATC Chapter") +
    xlim(1, 8) +
    labs(x = "Relative Change", y = "") +
    scale_y_discrete(position = "right") +
    theme_minimal() +
    theme(
        axis.text.y  = element_text(size = 10),
        axis.text.x  = element_text(size = 10),
        axis.title.x = element_text(size = 10),
        axis.title.y = element_text(size = 10),
        plot.title   = element_text(size = 14),
        legend.position = "none"
    )


### ----------------------------------------------------------------------------
### 7. PANEL B — DATA: validation dataset, pre/post meta-analysis per medication
### ----------------------------------------------------------------------------

val_data <- read_csv(validation_data_file, show_col_types = FALSE)
val_data <- val_data %>%
    mutate(
        MED_CHAPTER  = substr(code, 1, 1),
        CHAPTER_NAME = atc_chapter_map[MED_CHAPTER]
    )

# Build the pooled time-series dataset for all selected codes.
# For each code: estimate fixed-effects meta-analytic average ATT
# over the pre-period (PRE_WINDOW) and post-period (POST_WINDOW).
data_plot_all <- data.frame()

for (code in sort(unique(val_data$code), decreasing = TRUE)) {

    temp_results <- val_data %>% filter(code == !!code)

    # Index pre- and post-event time points
    before_idx <- temp_results$time %in% PRE_WINDOW
    after_idx  <- temp_results$time %in% POST_WINDOW

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

    # Keep only t = min(PRE_WINDOW) ... max(POST_WINDOW), attach pooled estimates and labels
    data_plot <- temp_results %>%
        filter(time >= min(PRE_WINDOW), time <= max(POST_WINDOW)) %>%
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


### ----------------------------------------------------------------------------
### 8. PANEL B — PLOTS: longitudinal DiD plots, sorted by absolute change
### ----------------------------------------------------------------------------

# Only plots 1 and 6 get y axis title
lp1  <- make_longitudinal_plot(sorted_codes[1],  data_plot_all, chapter_color_map, code_labels, show_y_title = TRUE)
lp2  <- make_longitudinal_plot(sorted_codes[2],  data_plot_all, chapter_color_map, code_labels, show_y_title = FALSE)
lp3  <- make_longitudinal_plot(sorted_codes[3],  data_plot_all, chapter_color_map, code_labels, show_y_title = FALSE)
lp4  <- make_longitudinal_plot(sorted_codes[4],  data_plot_all, chapter_color_map, code_labels, show_y_title = FALSE)
lp5  <- make_longitudinal_plot(sorted_codes[5],  data_plot_all, chapter_color_map, code_labels, show_y_title = FALSE)
lp6  <- make_longitudinal_plot(sorted_codes[6],  data_plot_all, chapter_color_map, code_labels, show_y_title = TRUE)
lp7  <- make_longitudinal_plot(sorted_codes[7],  data_plot_all, chapter_color_map, code_labels, show_y_title = FALSE)
lp8  <- make_longitudinal_plot(sorted_codes[8],  data_plot_all, chapter_color_map, code_labels, show_y_title = FALSE)
lp9  <- make_longitudinal_plot(sorted_codes[9],  data_plot_all, chapter_color_map, code_labels, show_y_title = FALSE)


### ----------------------------------------------------------------------------
### 9. FINAL FIGURE — assemble panels A, B, C and save
### ----------------------------------------------------------------------------

# Panel title grobs
a_title_grob <- textGrob(
    "A.  Absolute Change Estimates, by ATC Chapter",
    gp   = gpar(fontsize = 14, fontface = "bold"),
    just = "left",
    x    = unit(0.01, "npc")
)

b_title_grob <- textGrob(
    "B.  Difference-in-Differences Estimates",
    gp   = gpar(fontsize = 14, fontface = "bold"),
    just = "left",
    x    = unit(0.01, "npc")
)

c_title_grob <- textGrob(
    "C.  Relative Change Estimates",
    gp   = gpar(fontsize = 14, fontface = "bold"),
    just = "left",
    x    = unit(0.01, "npc")
)

# --- Panel A: title + plot stacked, proportional heights ---
panel_A <- arrangeGrob(
    a_title_grob,
    p1,
    nrow    = 2,
    heights = unit(c(1, 9), "null")
)

# --- Panel C cell: title + plot stacked ---
panel_C <- arrangeGrob(
    c_title_grob,
    p_ratio_combined,
    nrow    = 2,
    heights = unit(c(1, 9), "null")
)

# --- Panel B: 2x5 grid of longitudinal plots (Panel C occupies the last cell) ---
panel_BC <- arrangeGrob(
    b_title_grob,
    lp1, lp2, lp3, lp4, lp5,
    lp6, lp7, lp8, lp9, panel_C,
    layout_matrix = rbind(
        c(1, 1, 1, 1, 1),
        c(2, 3, 4, 5, 6),
        c(7, 8, 9, 10, 11)
    ),
    heights = unit(c(1, 9, 9), "null")
)

# --- Final figure: Panel A on top (40%), Panel BC on bottom (60%) ---
p_final <- arrangeGrob(
    panel_A,
    panel_BC,
    nrow   = 2,
    heights = unit(c(4, 6), "null")
)

# --- Save final figure as PNG and PDF ---
save_plot_png_pdf(p_final, OutDir, BASENAME_PLOT, FIG_WIDTH, FIG_HEIGHT, FIG_DPI)