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
DATE_3BD    <- "20260709"
DATE_3C     <- "20260625"
DATE_3E     <- "20260709"

results_3A_file    <- paste0('/media/volume/Projects/DSGELabProject1/DiD_Experiments/DiD_Diagnosis_', DATE_3A, '/Results_', DATE_3A, '/Results_ICD_', DATE_3A, '.csv')
results_3B_file    <- paste0('/media/volume/Projects/DSGELabProject1/Plots/Supplements/Supplements_Distress_', DATE_3BD, '/Supplements_DepressionBurnout_PhenotypeComparison_V1_', DATE_3BD, '.csv')
results_3C1_file    <- paste0('/media/volume/Projects/DSGELabProject1/Plots/Supplements/Supplements_Pregnancy_Base_', DATE_3C, '/Supplements_Pregnancy_Female_Long_', DATE_3C, '.csv')
results_3C2_file    <- paste0('/media/volume/Projects/DSGELabProject1/Plots/Supplements/Supplements_Pregnancy_Base_', DATE_3C, '/Supplements_Pregnancy_Male_Long_', DATE_3C, '.csv')
results_3D_file    <- paste0('...')
results_3E_file    <- paste0('/media/volume/Projects/DSGELabProject1/Plots/Supplements/Supplements_I80_subcodes_', DATE_3E, '/DiD_results_I80_subcodes_', DATE_3E, '.csv')

TODAY   <- format(Sys.Date(), "%Y%m%d")
outdir  <- paste0("/media/volume/Projects/DSGELabProject1/Plots/Figure3/Figure3_", TODAY, "/")
if (!dir.exists(outdir)) dir.create(outdir, recursive = TRUE)

N_THREADS <- 10
setDTthreads(N_THREADS)

# Window size (years either side of event)
WIN <- 3

# ============================================================
# 2. PANEL A — Individual disease scatter 
# ============================================================

dataset <- read_csv(results_3A_file, show_col_types = FALSE)
dataset <- dataset[dataset$N_CASES >= 300, ]

# Multiple testing correction
# Note: Using FDR correction, not as conservative as Bonferroni
dataset$SE       <- dataset$SE_DROP
dataset$PVAL     <- 2 * (1 - pnorm(abs(dataset$ATT_DROP / dataset$SE)))
dataset$PVAL_ADJ_FDR    <- p.adjust(dataset$PVAL, method = "fdr")
dataset$SIGNIFICANT_FDR <- dataset$PVAL_ADJ_FDR < 0.05

dataset$SIG_TYPE <- case_when(
    dataset$SIGNIFICANT_FDR == TRUE   ~ "Significant",
    TRUE ~ "Not Significant"
)
dataset$SIG_TYPE <- factor(dataset$SIG_TYPE, levels = c("Significant", "Not Significant"))

dataset$EVENT_CODE  <- substr(sub(".*_", "", dataset$EVENT_CODE), 1, 3)
dataset             <- dataset %>% mutate(MED_CHAPTER = substr(EVENT_CODE, 1, 1))
dataset$MED_CHAPTER <- factor(dataset$MED_CHAPTER, levels = sort(unique(dataset$MED_CHAPTER)))

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
JITTER_RANGE            <- 0.2
POINT_SIZE_SIG          <- 4
POINT_SIZE_NOT_SIG      <- 2
ALPHA_SIG               <- 1
ALPHA_NOT_SIG           <- 0.2
TEXT_SIZE_TITLE         <- 16
TEXT_SIZE_AXIS_TITLE    <- 14
TEXT_SIZE_AXIS_TEXT     <- 12
TEXT_SIZE_LEGEND        <- 14
CI_MULT                 <- 1.96  # 95% CI multiplier applied to all SEs
LINEWIDTH_MAIN          <- 0.5
POINT_SIZE_MAIN         <- 2
ERRORBAR_WIDTH          <- 0.2
DODGE_WIDTH             <- 0.3
HLINE_COLOR             <- "grey50";
HLINE_TYPE              <- "dashed";
VLINE_COLOR             <- "grey50";
VLINE_TYPE              <- "dashed";
LEGEND_POSITION         <- "bottom";

# Common theme layer added on top of theme_minimal() for every DiD panel
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
                    size = 4.5, show.legend = FALSE,
                    max.overlaps = Inf, min.segment.length = 0,
                    box.padding = 0.8, point.padding = 0.5,
                    force = 3, force_pull = 1,
                    segment.size = 0.5, segment.alpha = 0.6) +
    scale_x_continuous(breaks = 1:length(levels(dataset$CHAPTER_NAME)),
                       labels = levels(dataset$CHAPTER_NAME)) +
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
    labs(title = expression(bold("A. Absolute Change Estimates, by ICD-10 Chapter")),
         x = "",
         y = "Change in Total Number of Prescriptions\n(Within the event year)") +
    shared_theme +
    theme(
        axis.text.x     = element_text(angle = 30, hjust = 1, size = TEXT_SIZE_AXIS_TEXT),
        legend.position = "none"   
    ) +
    geom_hline(yintercept = 0, linetype = HLINE_TYPE, color = HLINE_COLOR)

# Supplementary figure: boxplot of drop across chapters
set.seed(1)
JITTER_RANGE <- 0.2
dataset$x_jittered <- as.numeric(dataset$CHAPTER_NAME) + runif(nrow(dataset), -JITTER_RANGE, JITTER_RANGE)

# Count observations per chapter
chapter_counts <- dataset %>%
    group_by(CHAPTER_NAME) %>%
    summarise(n = n(), .groups = "drop") %>%
    mutate(has_boxplot = n >= 3)

# Add indicator to dataset
dataset <- dataset %>%
    left_join(chapter_counts %>% select(CHAPTER_NAME, has_boxplot), by = "CHAPTER_NAME")

p_supp <- ggplot(dataset, aes(x = CHAPTER_NAME, y = ATT_DROP, colour = CHAPTER_NAME, fill = CHAPTER_NAME)) +
    geom_hline(yintercept = 0, linetype = "dashed", colour = "grey50", linewidth = 0.5) +
    geom_point(
        aes(x = x_jittered),
        shape = 16,
        size = 1.5,
        alpha = 0.2
    ) +
    {if(any(dataset$has_boxplot)) geom_boxplot(
        aes(x = as.numeric(CHAPTER_NAME)),
        data = dataset %>% filter(has_boxplot),
        width    = 0.45,
        alpha    = 0.3,
        outlier.shape = NA,   # outliers already visible as jittered dots
        linewidth = 0.6
    )} +
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
    shared_theme +
    theme(
        axis.text.x = element_text(size = TEXT_SIZE_AXIS_TEXT, angle = 45, hjust = 1),
        legend.position = "none",
        panel.grid.major.y = element_line(colour = "grey93"),
        panel.grid.minor = element_blank(),
        plot.margin = margin(8, 12, 8, 8)
    )

# Save supplementary figure separately
ggsave(file.path(outdir, paste0("Supplementary_Chapter_Distribution_", TODAY, ".png")),
       plot = p_supp, width = 14, height = 8, dpi = 300)


# ============================================================
# 3. EXTRA PANELS
# ============================================================

# ------------------------------------------------
# Panel B 
# ------------------------------------------------

# load data
results_B <- fread(results_3B_file)
results_B <- results_B %>% filter(time >= -WIN & time <= WIN)

# Distinct, fixed colors for each phenotype being compared (recycled if more than 6)
palette   <- c("#1f77b4", "#d62728", "#2ca02c", "#ff7f0e", "#9467bd", "#8c564b")
ph_names  <- unique(results_B$phenotype)
phenotype_colors <- setNames(rep_len(palette, length(ph_names)), ph_names)

p_B <- ggplot(results_B, aes(x = time, y = att, color = phenotype, group = phenotype)) +
    geom_line(linewidth = LINEWIDTH_MAIN, position = position_dodge(width = DODGE_WIDTH)) +
    geom_point(size = POINT_SIZE_MAIN, position = position_dodge(width = DODGE_WIDTH)) +
    geom_errorbar(
        aes(ymin = att - CI_MULT * se, ymax = att + CI_MULT * se),
        width = ERRORBAR_WIDTH, position = position_dodge(width = DODGE_WIDTH)
    ) +
    geom_hline(yintercept = 0, linetype = HLINE_TYPE, color = HLINE_COLOR) +
    geom_vline(xintercept = 0, linetype = VLINE_TYPE, color = VLINE_COLOR) +
    scale_color_manual(values = phenotype_colors) +
    labs(
        title    = expression(bold("B. Depression and Mental Distress")),
        x        = "Years from Event",
        y        = "Change in Total Number of Prescriptions\n(compared to controls)",
        color    = "Phenotype"
    ) +
    scale_x_continuous(breaks = -WIN:WIN) +
    shared_theme

# ------------------------------------------------
# Panel D 
# ------------------------------------------------

p_D <- ggplot() +
    annotate("text", x = 0.5, y = 0.5,
             label = "Panel D (placeholder; analysis to be filled in)",
             size = 5, color = "grey45", hjust = 0.5, vjust = 0.5) +
    xlim(0, 1) + ylim(0, 1) +
    labs(
        title    = expression(bold("D. Sick Leave results (placeholder)"))
    ) +
    theme_void() +
    theme(
        plot.title      = element_text(face = "bold", size = TEXT_SIZE_TITLE, hjust = 0),
        plot.subtitle   = element_text(size = TEXT_SIZE_AXIS_TITLE, color = "grey40", hjust = 0),
        plot.background = element_rect(fill = "grey97", color = "grey70", linetype = "dashed")
    )

# ------------------------------------------------
# Panel C — Childbirth
# ------------------------------------------------

# Load data
results_C1 <- fread(results_3C1_file)
results_C1 <- results_C1 %>% filter(time >= -WIN & time <= WIN)

results_C2 <- fread(results_3C2_file)
results_C2 <- results_C2 %>% filter(time >= -WIN & time <= WIN)

# Plot
results_C1$doctor_sex <- "Female doctors \n(who had a childbirth event)"
results_C2$doctor_sex <- "Male doctors \n(whose spouse had a childbirth event)"
results_C <- bind_rows(results_C1, results_C2)
doctor_sex_colors <- c(
    "Female doctors \n(who had a childbirth event)" = "#2ca02c",
    "Male doctors \n(whose spouse had a childbirth event)" = "#1f77b4"
)

p_C <- ggplot(results_C, aes(x = time, y = att, color = doctor_sex, group = doctor_sex)) +
    geom_line(linewidth = LINEWIDTH_MAIN, position = position_dodge(width = DODGE_WIDTH)) +
    geom_point(size = POINT_SIZE_MAIN, position = position_dodge(width = DODGE_WIDTH)) +
    geom_errorbar(
        aes(ymin = att - CI_MULT * se, ymax = att + CI_MULT * se),
        width = ERRORBAR_WIDTH, position = position_dodge(width = DODGE_WIDTH)
    ) +
    geom_hline(yintercept = 0, linetype = HLINE_TYPE, color = HLINE_COLOR) +
    geom_vline(xintercept = 0, linetype = VLINE_TYPE, color = VLINE_COLOR) +
    scale_color_manual(values = doctor_sex_colors) +
    labs(
        title    = expression(bold("C. Childbirth")),
        x = "Years from event",
        y = "Change in Total Number of Prescriptions\n(compared to controls)",
        color   = NULL
    ) +
    scale_x_continuous(breaks = -WIN:WIN) +
    shared_theme


# ------------------------------------------------
# Panel E — Phlebitis
# ------------------------------------------------

# load and prepare data
results_E <- fread(results_3E_file)
results_E <- results_E[time >= -WIN & time <= WIN]
results_E[, phenotype := factor(phenotype, levels = c(
    "Unspecified", 
    "Superficial", 
    "Deep", 
    "Other"
), labels = c(
    "Unspecified \n(lower extremities)", 
    "Superficial \n(lower extremities)", 
    "Deep \n(lower extremities)", 
    "Other"
))]

phenotype_colors_E <- c(
    "Unspecified \n(lower extremities)" = "#000000",
    "Superficial \n(lower extremities)" = "#ff7f0e",
    "Deep \n(lower extremities)" = "#2ca02c",
    "Other" = "#9467bd"
)

# plot results
p_E <- ggplot(results_E, aes(x = time, y = att, color = phenotype, group = phenotype)) +
    geom_line(linewidth = LINEWIDTH_MAIN, position = position_dodge(width = DODGE_WIDTH)) +
    geom_point(size = POINT_SIZE_MAIN, position = position_dodge(width = DODGE_WIDTH)) +
    geom_errorbar(
        aes(ymin = att - CI_MULT * se, ymax = att + CI_MULT * se),
        width = ERRORBAR_WIDTH, position = position_dodge(width = DODGE_WIDTH)
    ) +
    geom_hline(yintercept = 0, linetype = HLINE_TYPE, color = HLINE_COLOR) +
    geom_vline(xintercept = 0, linetype = VLINE_TYPE, color = VLINE_COLOR) +
    scale_color_manual(values = phenotype_colors_E) +
    scale_x_continuous(breaks = -WIN:WIN) +
    labs(
        title    = expression(bold("E. Phlebitis and Thrombophlebitis")),
        x = "Years from Event",
        y = "Change in Total Number of Prescriptions\n(compared to controls)",
        color = "Phenotype"
    ) +
    shared_theme

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