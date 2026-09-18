# ============================================================
# FIGURE 3 — Combined multi-panel figure
#
# Panels:
#   A : Absolute change estimates for individual diseases, by ICD-10 chapter
#   B : Depression and mental distress, base
#   C : Childbirth — female doctors vs male doctors 
#   D : Depression and mental distress, sick leave adjusted (LOCF)
#   E : Phlebitis and thrombophlebitis, by subcode
#
# Also produced:
#   Supplementary figure — chapter-level boxplot of the Panel A estimates
#
# Layout:
#   Row 1 : A (full width)
#   Row 2 : B | C
#   Row 3 : D | E
#
# ============================================================


# ============================================================
# 1. Libraries
# ============================================================

.libPaths("/shared-directory/sd-tools/apps/R/lib/")
suppressPackageStartupMessages({
    library(data.table)
    library(dplyr)
    library(tidyr)
    library(lubridate)
    library(ggplot2)
    library(patchwork)
    library(arrow)
    library(stringr)
    library(did)
    library(scales)
    library(ggrepel)
    library(readr)
    library(viridis)
})


# ============================================================
# 2. File paths and output directory
# ============================================================

# ---- Extraction / run dates of the input result files ----
DATE_3A  <- "20260915"   # Panel A  
DATE_3B  <- "20260709"   # Panel B  
DATE_3C  <- "20260716"   # Panel C 

# ---- Input files (one per panel) ----
results_3A_file  <- paste0('/media/volume/Projects/DSGELabProject1/DiD_Experiments/DiD_Diagnosis_', DATE_3A, '/Results_', DATE_3A, '/Results_ICD_', DATE_3A, '.csv')
results_3B_file  <- paste0('/media/volume/Projects/DSGELabProject1/Plots/Supplements/Supplements_Distress_', DATE_3B, '/Supplements_DepressionBurnout_PhenotypeComparison_V1_', DATE_3B, '.csv')
results_3C_file  <- paste0('/media/volume/Projects/mattferr/notes/note_20260715_depression_sick_leave_V2/Results_LOCF_MultiPhenotype_', DATE_3C, '/PhenotypeComparison_LOCF_', DATE_3C, '.csv')

# ---- Output directory ----
TODAY  <- format(Sys.Date(), "%Y%m%d")
outdir <- "/media/volume/Projects/DSGELabProject1/Plots/ManuscriptFinal/"

if (!dir.exists(outdir)) dir.create(outdir, recursive = TRUE)

# ============================================================
# 3. Parameters
# ============================================================

# ---- Compute ----
N_THREADS <- 10
setDTthreads(N_THREADS)
SEED <- 1                    # only used for the Panel A jitter

# ---- Analysis ----
WIN          <- 3            # years either side of the event shown in panels B, C
MIN_N_CASES  <- 300          # Panel A: minimum cases for a code to be plotted
FDR_ALPHA    <- 0.05         # Panel A: significance threshold after FDR correction
CI_MULT      <- 1.96         # 95% CI multiplier applied to all SEs
MIN_N_BOXPLOT <- 3           # Supplementary: min codes per chapter to draw a box

# ---- Panel A point styling ----
JITTER_RANGE       <- 0.2
POINT_SIZE_SIG     <- 4
POINT_SIZE_NOT_SIG <- 2
ALPHA_SIG          <- 1
ALPHA_NOT_SIG      <- 0.2

# ---- DiD panel (B-C) styling ----
LINEWIDTH_MAIN  <- 0.5
POINT_SIZE_MAIN <- 2
ERRORBAR_WIDTH  <- 0.2
DODGE_WIDTH     <- 0.3
HLINE_COLOR     <- "grey50"
HLINE_TYPE      <- "dashed"
VLINE_COLOR     <- "grey50"
VLINE_TYPE      <- "dashed"

# ---- Text sizes / legend ----
TEXT_SIZE_TITLE      <- 16
TEXT_SIZE_AXIS_TITLE <- 14
TEXT_SIZE_AXIS_TEXT  <- 12
TEXT_SIZE_LEGEND     <- 14
LEGEND_POSITION      <- "bottom"

# ---- Output sizes ----
FIG_MAIN_WIDTH  <- 24
FIG_MAIN_HEIGHT <- 18
FIG_SUPP_WIDTH  <- 14
FIG_SUPP_HEIGHT <- 8
FIG_DPI         <- 300

# ---- Shared axis label used by every DiD panel ----
Y_LAB_DID <- "Change in Total Number of Prescriptions\n(compared to controls)"
Y_LAB_A   <- "Change in Total Number of Prescriptions\n(within the event year)"


# ============================================================
# 4. Shared styling — palettes, colour helper, theme
# ============================================================

# Colourblind-friendly palette used for the ICD-10 chapters in Panel A
cb_palette <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2",
                "#D55E00", "#CC79A7", "#999999", "#000000", "#E6AB02",
                "#7570B3", "#66A61E", "#E7298A", "#A6761D", "#666666",
                "#1B9E77", "#D95F02", "#7570B3", "#E7298A", "#66A61E", "#E6AB02")

# palette for phenotype comparisons 
PALETTE_PHENOTYPE <- c(
    "#1f77b4", "#d62728", "#2ca02c", "#ff7f0e", "#9467bd", "#8c564b"
)
PHENOTYPE_COLORS_FIXED <- c(
    "Recurrent depressive disorder" = "#1f77b4",
    "Single depressive episode"     = "#d62728",
    "Distress"                      = "#2ca02c"
)

make_phenotype_colors <- function(ph_names) {
    ph_names <- as.character(unique(ph_names))
    cols <- unname(PHENOTYPE_COLORS_FIXED[ph_names])
    names(cols) <- ph_names
    gaps <- is.na(cols)
    if (any(gaps)) {
        spare <- setdiff(PALETTE_PHENOTYPE, cols[!gaps])
        if (length(spare) == 0) spare <- PALETTE_PHENOTYPE
        cols[gaps] <- rep_len(spare, sum(gaps))
    }
    cols
}

# ---- Panel C: legend order (phenotypes come from note5.R) ----
PANEL_C_PHENOTYPE_ORDER <- c("Recurrent depressive disorder",
                             "Single depressive episode",
                             "Distress")

# Common theme layer added on top of theme_minimal() for every panel
shared_theme <- theme_minimal() +
    theme(
        legend.position  = LEGEND_POSITION,
        legend.text      = element_text(size = TEXT_SIZE_LEGEND),
        legend.title     = element_text(size = TEXT_SIZE_LEGEND),
        axis.text.x      = element_text(size = TEXT_SIZE_AXIS_TEXT),
        axis.text.y      = element_text(size = TEXT_SIZE_AXIS_TEXT),
        axis.title.x     = element_text(size = TEXT_SIZE_AXIS_TITLE),
        axis.title.y     = element_text(size = TEXT_SIZE_AXIS_TITLE),
        plot.title       = element_text(size = TEXT_SIZE_TITLE)
    )


# ============================================================
# 5. Lookup tables (Panel A)
# ============================================================

icd10_chapter_map <- c(
    "A" = "Certain infectious \nand parasitic diseases",
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
    "M" = "Diseases of the musculoskeletal system \nand connective tissue",
    "N" = "Diseases of the genitourinary system",
    "O" = "Pregnancy, childbirth and the puerperium",
    "P" = "Certain conditions originating in the perinatal period",
    "Q" = "Congenital malformations, deformations \n and chromosomal abnormalities",
    "R" = "Symptoms, signs and abnormal clinical and \nlaboratory findings, not elsewhere classified",
    "S" = "Injuries",
    "T" = "Poisoning and certain other consequences \nof external causes",
    "U" = "Codes for special purposes",
    "V" = "Transport accidents",
    "W" = "Other external causes of accidental injury",
    "X" = "Other external causes of accidental injury",
    "Z" = "Factors influencing health status \n and contact with health services"
)

# Codes labelled directly on Panel A
code_labels <- tibble(
    EVENT_CODE = c(
        "C50",
        "F32",
        "F33",
        "F43",
        "G43",
        "I80",
        "O80",
        "O82",
        "Z34",
        "Z36"
    ),
    LABEL = c(
        "Malignant neoplasm of breast",
        "Single depressive episode",
        "Recurrent depressive disorder",
        "Severe stress and adjustment disorders",
        "Migraine",
        "Phlebitis and thrombophlebitis",
        "Single spontaneous delivery",
        "Single delivery by caesarean section",
        "Supervision of normal pregnancy",
        "Antenatal screening"    
    )
)


# ============================================================
# 6. PANEL A — Individual disease scatter (+ supplementary figure)
# ============================================================

# ---- Load and filter ----
results_A <- read_csv(results_3A_file, show_col_types = FALSE)
results_A <- results_A[results_A$N_CASES >= MIN_N_CASES, ]

# ---- Multiple testing correction ----
# Note: FDR correction, not as conservative as Bonferroni
results_A$SE               <- results_A$SE_DROP
results_A$PVAL             <- 2 * (1 - pnorm(abs(results_A$ATT_DROP / results_A$SE)))
results_A$PVAL_ADJ_FDR     <- p.adjust(results_A$PVAL, method = "fdr")
results_A$SIGNIFICANT_FDR  <- results_A$PVAL_ADJ_FDR < FDR_ALPHA
results_A$SIG_TYPE <- factor(
    ifelse(results_A$SIGNIFICANT_FDR, "Significant", "Not Significant"),
    levels = c("Significant", "Not Significant")
)

# ---- Map codes to ICD-10 chapters ----
results_A$EVENT_CODE  <- substr(sub(".*_", "", results_A$EVENT_CODE), 1, 3)
results_A             <- results_A %>% mutate(MED_CHAPTER = substr(EVENT_CODE, 1, 1))
results_A$MED_CHAPTER <- factor(results_A$MED_CHAPTER, levels = sort(unique(results_A$MED_CHAPTER)))
results_A$CHAPTER_NAME <- factor(
    icd10_chapter_map[as.character(results_A$MED_CHAPTER)],
    levels = icd10_chapter_map[sort(unique(as.character(results_A$MED_CHAPTER)))]
)

# ---- Jitter codes within their chapter (shared by Panel A and the supplement) ----
set.seed(SEED)
results_A$x_jittered <- as.numeric(results_A$CHAPTER_NAME) +
    runif(nrow(results_A), -JITTER_RANGE, JITTER_RANGE)

# ---- Highlighted codes, placed on the same jittered coordinates ----
robust_result_labels <- results_A %>% inner_join(code_labels, by = "EVENT_CODE")
robust_result_labels$x_jittered <- results_A$x_jittered[
    match(interaction(robust_result_labels$CHAPTER_NAME, robust_result_labels$EVENT_CODE),
          interaction(results_A$CHAPTER_NAME,            results_A$EVENT_CODE))
]

# ---- Panel A plot ----
p_A_main <- ggplot(results_A, aes(x = x_jittered, y = ATT_DROP, color = CHAPTER_NAME)) +
    geom_point(aes(shape = SIG_TYPE, size = SIG_TYPE, alpha = SIG_TYPE)) +
    geom_text_repel(data = robust_result_labels,
                    aes(label = LABEL),
                    size = 4.5, show.legend = FALSE,
                    max.overlaps = Inf, min.segment.length = 0,
                    box.padding = 0.8, point.padding = 0.5,
                    force = 3, force_pull = 1,
                    segment.size = 0.5, segment.alpha = 0.6) +
    scale_x_continuous(breaks = seq_along(levels(results_A$CHAPTER_NAME)),
                       labels = levels(results_A$CHAPTER_NAME)) +
    scale_color_manual(values = cb_palette, name = "Chapter", guide = "none") +
    scale_shape_manual(name = "Significance",
                       values = c("Significant"     = 17,       # filled triangle
                                  "Not Significant" = 16)) +    # filled circle (dimmed via alpha)
    scale_size_manual(name  = "Significance",
                      values = c("Significant"      = POINT_SIZE_SIG,
                                 "Not Significant"  = POINT_SIZE_NOT_SIG)) +
    scale_alpha_manual(name  = "Significance",
                       values = c("Significant"     = ALPHA_SIG,
                                  "Not Significant" = ALPHA_NOT_SIG)) +
    geom_hline(yintercept = 0, linetype = HLINE_TYPE, color = HLINE_COLOR) +
    labs(title = expression(bold("A. Absolute Change Estimates, by ICD-10 Chapter")),
         x = "",
         y = Y_LAB_A) +
    shared_theme +
    theme(
        axis.text.x     = element_text(angle = 30, hjust = 1, size = TEXT_SIZE_AXIS_TEXT),
        legend.position = "none"
    )

# ---- Supplementary figure: chapter-level distribution of the same estimates ----
# Boxes are only drawn for chapters with enough codes to be meaningful
chapter_counts <- results_A %>%
    group_by(CHAPTER_NAME) %>%
    summarise(n = n(), .groups = "drop") %>%
    mutate(has_boxplot = n >= MIN_N_BOXPLOT)

results_A <- results_A %>%
    left_join(chapter_counts %>% select(CHAPTER_NAME, has_boxplot), by = "CHAPTER_NAME")

p_supp <- ggplot(results_A, aes(x = CHAPTER_NAME, y = ATT_DROP, colour = CHAPTER_NAME, fill = CHAPTER_NAME)) +
    geom_hline(yintercept = 0, linetype = HLINE_TYPE, colour = HLINE_COLOR, linewidth = 0.5) +
    geom_point(
        aes(x = x_jittered),
        shape = 16,
        size  = 1.5,
        alpha = 0.2
    ) +
    {if (any(results_A$has_boxplot)) geom_boxplot(
        aes(x = as.numeric(CHAPTER_NAME)),
        data = results_A %>% filter(has_boxplot),
        width         = 0.45,
        alpha         = 0.3,
        outlier.shape = NA,   # outliers already visible as jittered dots
        linewidth     = 0.6
    )} +
    scale_x_continuous(
        breaks = seq_along(levels(results_A$CHAPTER_NAME)),
        labels = levels(results_A$CHAPTER_NAME)
    ) +
    scale_colour_manual(values = cb_palette, guide = "none") +
    scale_fill_manual(values = cb_palette, guide = "none") +
    labs(
        x = "",
        y = Y_LAB_A
    ) +
    shared_theme +
    theme(
        axis.text.x        = element_text(size = TEXT_SIZE_AXIS_TEXT, angle = 45, hjust = 1),
        legend.position    = "none",
        panel.grid.major.y = element_line(colour = "grey93"),
        panel.grid.minor   = element_blank(),
        plot.margin        = margin(8, 12, 8, 8)
    )


# ============================================================
# 7. PANEL B — Depression and mental distress (all doctors)
# ============================================================

results_B <- fread(results_3B_file)
results_B <- results_B[time >= -WIN & time <= WIN]

phenotype_colors_B <- make_phenotype_colors(results_B$phenotype)

p_B <- ggplot(results_B, aes(x = time, y = att, color = phenotype, group = phenotype)) +
    geom_line(linewidth = LINEWIDTH_MAIN, position = position_dodge(width = DODGE_WIDTH)) +
    geom_point(size = POINT_SIZE_MAIN, position = position_dodge(width = DODGE_WIDTH)) +
    geom_errorbar(
        aes(ymin = att - CI_MULT * se, ymax = att + CI_MULT * se),
        width = ERRORBAR_WIDTH, position = position_dodge(width = DODGE_WIDTH)
    ) +
    geom_hline(yintercept = 0, linetype = HLINE_TYPE, color = HLINE_COLOR) +
    geom_vline(xintercept = 0, linetype = VLINE_TYPE, color = VLINE_COLOR) +
    scale_color_manual(values = phenotype_colors_B) +
    scale_x_continuous(breaks = -WIN:WIN) +
    labs(
        title = expression(bold("B. Depression and Mental Distress")),
        x     = "Years from Event",
        y     = Y_LAB_DID,
        color = "Phenotype"
    ) +
    shared_theme


# ============================================================
# 8. PANEL C — Depression and mental distress, sick leave adjusted
# ============================================================

results_C <- fread(results_3C_file)
results_C <- results_C[time >= -WIN & time <= WIN]

levels_C <- c(intersect(PANEL_C_PHENOTYPE_ORDER, unique(results_C$phenotype)),
              setdiff(unique(results_C$phenotype), PANEL_C_PHENOTYPE_ORDER))
results_C[, phenotype := factor(phenotype, levels = levels_C)]

phenotype_colors_C <- make_phenotype_colors(levels(results_C$phenotype))

p_C <- ggplot(results_C, aes(x = time, y = att, color = phenotype, group = phenotype)) +
    geom_line(linewidth = LINEWIDTH_MAIN, position = position_dodge(width = DODGE_WIDTH)) +
    geom_point(size = POINT_SIZE_MAIN, position = position_dodge(width = DODGE_WIDTH)) +
    geom_errorbar(
        aes(ymin = att - CI_MULT * se, ymax = att + CI_MULT * se),
        width = ERRORBAR_WIDTH, position = position_dodge(width = DODGE_WIDTH)
    ) +
    geom_hline(yintercept = 0, linetype = HLINE_TYPE, color = HLINE_COLOR) +
    geom_vline(xintercept = 0, linetype = VLINE_TYPE, color = VLINE_COLOR) +
    scale_color_manual(values = phenotype_colors_C) +
    scale_x_continuous(breaks = -WIN:WIN) +
    labs(
        title = expression(bold("C. Depression and Mental Distress, Sick Leave Adjusted")),
        x     = "Years from Event",
        y     = Y_LAB_DID,
        color = "Phenotype"
    ) +
    shared_theme


# ============================================================
# 11. Assemble and save outputs
#   Row 1 : Panel A  (full width)
#   Row 2 : Panel B | Panel C
# ============================================================

p_combined_full <- (
    p_A_main /
    (p_B | p_C)
) +
plot_layout(heights = c(1, 1))

ggsave(
    filename = file.path(outdir, paste0("Figure3_ABC_", TODAY, ".png")),
    plot     = p_combined_full,
    width    = FIG_MAIN_WIDTH,
    height   = FIG_MAIN_HEIGHT,
    dpi      = FIG_DPI
)

ggsave(
    filename = file.path(outdir, paste0("Supplementary_Chapter_Distribution_", TODAY, ".png")),
    plot     = p_supp,
    width    = FIG_SUPP_WIDTH,
    height   = FIG_SUPP_HEIGHT,
    dpi      = FIG_DPI
)