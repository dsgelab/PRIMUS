# ============================================================
# Combined Figure Script
#   Panel A  : Individual disease scatter
#   Supp Fig : Chapter-level boxplot
#   Panel B  : Depression & Burnout, all doctors
#   Panel C  : Depression stratified (PLACEHOLDER)
#   Panel D  : Childbirth, female doctors only
#   Panel E  : Childbirth, male doctors with pregnant spouse
#
# Modifications vs originals:
#   - 3-year window instead of 5 (time >= -3 & time <= 3)
#   - Pre/post bracket annotation at t=0 on each DiD panel (B–E)
# ============================================================


# ============================================================
# 0. Libraries
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
# 1. File paths and globals
# ============================================================

DATE_3A     <- "20260219"
DATE_3BD    <- "20260625"
DATE_3CE    <- "20260625"

results_3A_file    <- paste0('/media/volume/Projects/DSGELabProject1/DiD_Experiments/DiD_Diagnosis_', DATE_3A, '/Results_', DATE_3A, '/Results_ICD_', DATE_3A, '.csv')
results_3B_file    <- paste0('/media/volume/Projects/DSGELabProject1/Plots/Supplements/Supplements_DepressionBurnout_Base_', DATE_3BD, '/Supplements_DepressionBurnout_BaseDiD_Long_', DATE_3BD, '.csv')
results_3C_file    <- paste0('/media/volume/Projects/DSGELabProject1/Plots/Supplements/Supplements_Pregnancy_Base_', DATE_3CE, '/Supplements_Pregnancy_Female_Long_', DATE_3CE, '.csv')
results_3D_file    <- paste0('...')
results_3E_file    <- paste0('/media/volume/Projects/DSGELabProject1/Plots/Supplements/Supplements_Pregnancy_Base_', DATE_3CE, '/Supplements_Pregnancy_Male_Long_', DATE_3CE, '.csv')

TODAY   <- format(Sys.Date(), "%Y%m%d")
outdir  <- paste0("/media/volume/Projects/DSGELabProject1/Plots/Figure3/Figure3_", TODAY, "/")
if (!dir.exists(outdir)) dir.create(outdir, recursive = TRUE)

N_THREADS <- 10
setDTthreads(N_THREADS)

# Window size (years either side of event)
WIN <- 3

# Event-code regex patterns
ChildBirth        <- "O80|O81|O82|O83|O84"
DepressionBurnOut <- "F32|F33(?!\\.4)|F43|Z73"   # F33.4 (remission) excluded

# Human-readable phenotype labels
depr_label  <- "ICD-10 {F32, F33 excl. F33.4, F43, Z73}"
birth_label <- "ICD-10 {O80, O81, O82, O83, O84}"


# ============================================================
# 2. Helper: bracket annotation for DiD plots
#    Highlights the t = 0 estimate with horizontal reference
#    lines (point estimate + 95 % CI) and a vertical bracket
#    that reports the exact ATT value at the event year.
# ============================================================

add_bracket_annotation <- function(plot, results, win = WIN) {

    # Extract the t = 0 row
    t0 <- results %>% filter(time == 0)
    if (nrow(t0) == 0) return(plot)   # safety: no t=0 in data

    est    <- t0$att[1]
    ci_lo  <- est - t0$se[1] * 1.96
    ci_hi  <- est + t0$se[1] * 1.96

    # Bracket geometry — placed just to the right of t = 0
    bx       <- 0.2
    tick_len <- 0.05
    label_x  <- bx + 0.05
    # Bracket runs from y = 0 to y = est (estimated drop at t=0)
    y_low    <- 0
    y_high   <- est
    # Position label midway along the bracket
    label_y  <- (y_low + y_high) / 2
    bracket_label <- sprintf("drop = %.0f \nIC95%% [%.0f, %.0f]", est, ci_lo, ci_hi)

    plot +
        # Vertical bracket spanning from 0 to estimate
        annotate("segment",
                 x = bx, xend = bx, y = y_low, yend = y_high,
                 color = "black", linewidth = 0.7) +
        annotate("segment",
                 x = bx - tick_len, xend = bx, y = y_low, yend = y_low,
                 color = "black", linewidth = 0.7) +
        annotate("segment",
                 x = bx - tick_len, xend = bx, y = y_high, yend = y_high,
                 color = "black", linewidth = 0.7) +
        annotate("text",
                 x = label_x, y = label_y,
                 label = bracket_label,
                 hjust = 0, vjust = 0.5, size = 3.5, lineheight = 0.9)
}


# ============================================================
# 3. PANEL A — Individual disease scatter 
# ============================================================

dataset <- read_csv(results_3A_file, show_col_types = FALSE)
dataset <- dataset[dataset$N_CASES >= 300, ]

# Multiple testing correction
dataset$SE       <- dataset$SE_DROP
dataset$PVAL     <- 2 * (1 - pnorm(abs(dataset$ATT_DROP / dataset$SE)))
dataset$PVAL_ADJ_BONF <- p.adjust(dataset$PVAL, method = "bonferroni")
dataset$PVAL_ADJ_FDR    <- p.adjust(dataset$PVAL, method = "fdr")
dataset$SIGNIFICANT_BONF <- dataset$PVAL_ADJ_BONF < 0.05
dataset$SIGNIFICANT_FDR <- dataset$PVAL_ADJ_FDR < 0.05

dataset$SIG_TYPE <- case_when(
    dataset$SIGNIFICANT_BONF == TRUE  ~ "Bonferroni Significant",
    dataset$SIGNIFICANT_FDR == TRUE   ~ "FDR Significant",
    TRUE ~ "Not Significant"
)
dataset$SIG_TYPE <- factor(dataset$SIG_TYPE, levels = c("Bonferroni Significant", "FDR Significant", "Not Significant"))

dataset$EVENT_CODE  <- substr(sub(".*_", "", dataset$EVENT_CODE), 1, 3)
dataset             <- dataset %>% mutate(MED_CHAPTER = substr(EVENT_CODE, 1, 1))
dataset$MED_CHAPTER <- factor(dataset$MED_CHAPTER, levels = sort(unique(dataset$MED_CHAPTER)))

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
    "M" = "Diseases of the musculoskeletal system \nand connective tissue",
    "N" = "Diseases of the genitourinary system",
    "O" = "Pregnancy, childbirth and the puerperium",
    "P" = "Certain conditions originating in the perinatal period",
    "Q" = "Congenital malformations, deformations \n and chromosomal abnormalities",
    "R" = "Symptoms, signs and abnormal clinical and laboratory \nfindings, not elsewhere classified",
    "S" = "Injuries",
    "T" = "Poisoning and certain other consequences \nof external causes",
    "U" = "Codes for special purposes",
    "V" = "Transport accidents",
    "W" = "Other external causes of accidental injury",
    "X" = "Other external causes of accidental injury",
    "Z" = "Factors influencing health status \n and contact with health services"
)

dataset$CHAPTER_NAME <- factor(
    icd10_chapter_map[as.character(dataset$MED_CHAPTER)],
    levels = icd10_chapter_map[sort(unique(as.character(dataset$MED_CHAPTER)))]
)

cb_palette <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2",
                "#D55E00", "#CC79A7", "#999999", "#000000", "#E6AB02",
                "#7570B3", "#66A61E", "#E7298A", "#A6761D", "#666666",
                "#1B9E77", "#D95F02", "#7570B3", "#E7298A", "#66A61E", "#E6AB02")

code_labels <- tibble(
    EVENT_CODE = c(
        "C50", 
        "F33", 
        "F43", 
        "I80",
        "O80", 
        "O82",
        "O02",
        "Z34", 
        "Z36", 
        "Z73"),
    LABEL = c(
        "Malignant neoplasm of breast", 
        "Recurrent depressive disorder",
        "Severe stress and adjustment disorders", 
        "Phlebitis and thrombophlebitis",
        "Single spontaneous delivery", 
        "Single delivery by caesarean section",
        "Other abnormal products of conception",
        "Supervision of normal pregnancy", 
        "Antenatal screening",
        "Problems related to life-management difficulty"
    )
)
robust_result_labels <- dataset %>% inner_join(code_labels, by = "EVENT_CODE")

set.seed(1)
JITTER_RANGE        <- 0.2
POINT_SIZE_SIG      <- 4
POINT_SIZE_NOT_SIG  <- 2
ALPHA_SIG           <- 1
ALPHA_NOT_SIG       <- 0.2
TEXT_SIZE_TITLE      <- 16
TEXT_SIZE_AXIS_TITLE <- 14
TEXT_SIZE_AXIS_TEXT  <- 10
TEXT_SIZE_LEGEND     <- 12

dataset$x_jittered <- as.numeric(dataset$CHAPTER_NAME) +
    runif(nrow(dataset), -JITTER_RANGE, JITTER_RANGE)
robust_result_labels$x_jittered <- dataset$x_jittered[
    match(interaction(robust_result_labels$CHAPTER_NAME, robust_result_labels$EVENT_CODE),
          interaction(dataset$CHAPTER_NAME,              dataset$EVENT_CODE))
]

# Panel A: scatter of individual diseases
p_A_main <- ggplot(dataset, aes(x = x_jittered, y = ATT_DROP, color = CHAPTER_NAME)) +
    geom_point(aes(shape = SIG_TYPE, size = SIG_TYPE, alpha = SIG_TYPE)) +
    geom_text_repel(data = robust_result_labels,
                    aes(label = LABEL),
                    size = 4, show.legend = FALSE,
                    max.overlaps = Inf, min.segment.length = 0,
                    box.padding = 0.8, point.padding = 0.5,
                    force = 3, force_pull = 1,
                    segment.size = 0.5, segment.alpha = 0.6) +
    scale_x_continuous(breaks = 1:length(levels(dataset$CHAPTER_NAME)),
                       labels = levels(dataset$CHAPTER_NAME)) +
    scale_color_manual(values = cb_palette, name = "Chapter", guide = "none") +
    scale_shape_manual(name = "Significance",
                       values = c("Bonferroni Significant" = 17,   # filled triangle
                                  "FDR Significant"        = 16,   # filled circle
                                  "Not Significant"        = 16)) + # filled circle (dimmed via alpha)
    scale_size_manual(name  = "Significance",
                      values = c("Bonferroni Significant" = POINT_SIZE_SIG,
                                 "FDR Significant"        = POINT_SIZE_SIG,
                                 "Not Significant"        = POINT_SIZE_NOT_SIG)) +
    scale_alpha_manual(name  = "Significance",
                       values = c("Bonferroni Significant" = ALPHA_SIG,
                                  "FDR Significant"        = ALPHA_SIG,
                                  "Not Significant"        = ALPHA_NOT_SIG)) +
    labs(title = expression(bold("A. Individual Diseases")),
         x = "",
         y = "Change in Total Number of Prescriptions\n(Within the event year)") +
    theme_minimal() +
    theme(
        axis.text.x  = element_text(angle = 40, hjust = 1, size = TEXT_SIZE_AXIS_TEXT),
        axis.text.y  = element_text(size = TEXT_SIZE_AXIS_TEXT),
        axis.title.x = element_text(size = TEXT_SIZE_AXIS_TITLE),
        axis.title.y = element_text(size = TEXT_SIZE_AXIS_TITLE),
        plot.title   = element_text(size = TEXT_SIZE_TITLE),
        legend.position = "none"
    ) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "red")

# Supplementary figure: boxplot of drop across chapters
set.seed(1)
JITTER_RANGE <- 0.2
dataset$x_jittered <- as.numeric(dataset$CHAPTER_NAME) + runif(nrow(dataset), -JITTER_RANGE, JITTER_RANGE)

p_supp <- ggplot(dataset, aes(x = CHAPTER_NAME, y = ATT_DROP, colour = CHAPTER_NAME, fill = CHAPTER_NAME)) +
    geom_hline(yintercept = 0, linetype = "dashed", colour = "grey50", linewidth = 0.5) +
    geom_point(
        aes(x = x_jittered),
        shape = 16,
        size = 1.5,
        alpha = 0.2
    ) +
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
    scale_colour_manual(values = cb_palette, guide = "none") +
    scale_fill_manual(values = cb_palette, guide = "none") +
    labs(
        x = "",
        y = "Change in Total Number of Prescriptions\n(Within the event year)"
    ) +
    theme_minimal() +
    theme(
        axis.text.x = element_text(size = 8, angle = 45, hjust = 1),
        axis.text.y = element_text(size = 10),
        axis.title.y = element_text(size = 12),
        plot.title = element_text(size = 14, face = "bold"),
        panel.grid.major.y = element_line(colour = "grey93"),
        panel.grid.minor = element_blank(),
        plot.margin = margin(8, 12, 8, 8)
    )

# Save supplementary figure separately
ggsave(file.path(outdir, paste0("Supplementary_Chapter_Distribution_", TODAY, ".png")),
       plot = p_supp, width = 14, height = 8, dpi = 300)


# ============================================================
# 4. ANALYSIS 1 — Depression & Burnout
# ============================================================

# ------------------------------------------------
# Panel B — Depression & Burnout, all doctors
# ------------------------------------------------

# load data
results_B <- fread(results_3B_file)
results_B <- results_B %>% filter(time >= -WIN & time <= WIN)

n_cases_B    <- 2518
n_controls_B <- 22480

# plot
p_B_base <- ggplot(results_B, aes(x = time, y = att)) +
    geom_line(color = "#d62728") +
    geom_point(color = "#d62728") +
    geom_errorbar(aes(ymin = att - 1.96 * se, ymax = att + 1.96 * se), width = 0.2, color = "#d62728") +
    geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
    geom_vline(xintercept = 0, linetype = "dotted", color = "grey50") +
    labs(
        title    = expression(bold("B. Depression & Burnout")),
        subtitle = paste0("Phenotype definition:  ", depr_label,
                          "\nCases:  all doctors with depression/burnout event | Controls: all other doctors",
                          "\nCases: ", n_cases_B, "  |  Controls: ", n_controls_B),
        x = "Years from event",
        y = "Change in Total Number of Prescriptions\n(compared to controls)"
    ) +
    scale_x_continuous(breaks = -WIN:WIN) +
    theme_minimal()

p_B <- add_bracket_annotation(p_B_base, results_B)

# ------------------------------------------------
# Panel D — Depression stratified (PLACEHOLDER)
# ------------------------------------------------

p_D <- ggplot() +
    annotate("text", x = 0.5, y = 0.5,
             label = "Panel D (placeholder; analysis to be filled in)",
             size = 5, color = "grey45", hjust = 0.5, vjust = 0.5) +
    xlim(0, 1) + ylim(0, 1) +
    labs(
        title    = expression(bold("D. Depression & Burnout, stratified (placeholder)"))
    ) +
    theme_void() +
    theme(
        plot.title      = element_text(face = "bold", size = 12, hjust = 0),
        plot.subtitle   = element_text(size = 10, color = "grey40", hjust = 0),
        plot.background = element_rect(fill = "grey97", color = "grey70", linetype = "dashed")
    )


# ============================================================
# 5. ANALYSIS 2 — Childbirth
# ============================================================

# ------------------------------------------------
# Panel C — Childbirth, female doctors only
# ------------------------------------------------

# Load data
results_C <- fread(results_3C_file)
results_C <- results_C %>% filter(time >= -WIN & time <= WIN)

n_cases_C    <- 6950
n_controls_C <- 7250

# Plot
p_C_base <- ggplot(results_C ,aes(x = time, y = att)) +
    geom_line(color = "#2ca02c") +
    geom_point(color = "#2ca02c") +
    geom_errorbar(aes(ymin = att - 1.96 * se, ymax = att + 1.96 * se), width = 0.2, color = "#2ca02c") +
    geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
    geom_vline(xintercept = 0, linetype = "dotted", color = "grey50") +
    labs(
        title    = expression(bold("C. Childbirth, female doctors")),
        subtitle = paste0("Phenotype definition: ", birth_label,
                          "\nCases: female doctors which had a childbirth event | Controls: other female doctors",
                          "\nCases: ", n_cases_C, "  |  Controls: ", n_controls_C),
        x = "Years from event",
        y = "Change in Total Number of Prescriptions\n(compared to controls)"
    ) +
    scale_x_continuous(breaks = -WIN:WIN) +
    theme_minimal()

p_C <- add_bracket_annotation(p_C_base, results_C)


# ------------------------------------------------
# Panel E — Childbirth, male doctors with pregnant spouse
# ------------------------------------------------

# Load data
results_E <- fread(results_3E_file)
results_E <- results_E %>% filter(time >= -WIN & time <= WIN)

n_cases_E    <- 4253
n_controls_E <- 6850

# Plot
p_E_base <- ggplot(results_E ,aes(x = time, y = att)) +
    geom_line(color = "#1f77b4") +
    geom_point(color = "#1f77b4") +
    geom_errorbar(aes(ymin = att - 1.96 * se, ymax = att + 1.96 * se), width = 0.2, color = "#1f77b4") +
    geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
    geom_vline(xintercept = 0, linetype = "dotted", color = "grey50") +
    labs(
        title    = expression(bold("E. Childbirth, male doctors")),
        subtitle = paste0("Phenotype definition: ", birth_label, " (spouse's event)",
                          "\nCases: male doctors with a spouse who had a childbirth event | Controls: other male doctors",
                          "\nCases: ", n_cases_E, "  |  Controls: ", n_controls_E),
        x = "Years from event",
        y = "Change in Total Number of Prescriptions\n(compared to controls)"
    ) +
    scale_x_continuous(breaks = -WIN:WIN) +
    theme_minimal()

p_E <- add_bracket_annotation(p_E_base, results_E)

# ============================================================
# 7. Assemble combined figure
#   Row 1 : Panel A  (full width)
#   Row 2 : Panel B | Panel C
#   Row 3 : Panel D | Panel E
# ============================================================

p_combined_full <- (
    p_A_main /
    (p_B | p_C) /
    (p_D | p_E)
) +
plot_layout(heights = c(1.4, 1, 1))

ggsave(
    filename = file.path(outdir, paste0("Figure3_ABCDE_", TODAY, ".png")),
    plot     = p_combined_full,
    width    = 24,
    height   = 18,
    dpi      = 300
)