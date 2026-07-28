# =============================================================================
# 1. LIBRARIES
# =============================================================================

.libPaths("/shared-directory/sd-tools/apps/R/lib/")

library(ggplot2)
library(dplyr)
library(readr)
library(tidyr)
library(scales)
library(ggrepel)
library(data.table)
library(grid)      
library(gridExtra)  


# =============================================================================
# 2. FILE PATHS
# =============================================================================

InDir <- "/media/volume/Projects/jg/Output_files"

diag_docs_file    <- file.path(InDir, "diag_docs_results_20251125_162616.csv")
diag_nondocs_file <- file.path(InDir, "diag_nondocs_results_20251125_184815.csv")
medi_docs_file    <- file.path(InDir, "medi_docs_results_20251127_102704.csv")
medi_nondocs_file <- file.path(InDir, "medi_nondocs_results_20251127_115429.csv")

# Date-stamped output directory
TODAY   <- format(Sys.time(), "%Y%m%d")
OutDir <- "/media/volume/Projects/DSGELabProject1/Plots/ManuscriptFinal/"

# -- Outputs --
outfile_fig2_png      <- paste0(OutDir, "Figure2_", TODAY, ".png")
outfile_fig2_pdf      <- paste0(OutDir, "Figure2_", TODAY, ".pdf")
outfile_results_csv   <- paste0(OutDir, "Figure2_all_results_", TODAY, ".csv")

# -- Checkpoint data files (saved after Section 6, reloaded in Sections 7-8 for plotting) --: 
outfile_diag_data_csv <- paste0(OutDir, "Figure2_diag_data_", TODAY, ".csv")
outfile_medi_data_csv <- paste0(OutDir, "Figure2_medi_data_", TODAY, ".csv")


# =============================================================================
# 3. GLOBAL VARIABLES & CONSTANTS
# =============================================================================

# --- ATC chapter names (WHO ATC classification) ----------------------------
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
  "P" = "Antiparasitic Products, Insecticides and Repellents",
  "R" = "Respiratory System",
  "S" = "Sensory Organs",
  "V" = "Various"
)

# --- ICD-10 chapter names (WHO ICD-10 classification) ----------------------
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
  "R" = "Symptoms, signs and abnormal clinical and laboratory findings",
  "S" = "Injuries",
  "T" = "Poisoning and certain other consequences of external causes",
  "U" = "Codes for special purposes",
  "V" = "Transport accidents",
  "W" = "Other external causes of accidental injury",
  "X" = "Other external causes of accidental injury",
  "Z" = "Factors influencing health status \nand contact with health services"
)

# --- Colour-blind-friendly palette -----------------------------------------
cb_palette <- c(
  "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7", 
  "#999999", "#000000", "#E6AB02", "#7570B3", "#66A61E", "#E7298A", "#A6761D", 
  "#666666", "#1B9E77", "#D95F02", "#7570B3", "#E7298A", "#66A61E", "#E6AB02"
)

# --- Plotting parameters ----------------------------------------------------

ALPHA               <- 0.05

POINT_SIZE_SIG      <- 2.5    # significant points (given ALPHA)
POINT_SIZE_NONSIG   <- 1.5    # non-significant points 

ALPHA_CI            <- 0.10   # IC95% error bars (high transparency)
ALPHA_SIG           <- 0.50   # significant points
ALPHA_NONSIG        <- 0.10   # non-significant points (same as CI bars)

CI_ERRORBAR_LINEWIDTH <- 0.35

TEXT_SIZE_TITLE      <- 16
TEXT_SIZE_SUBTITLE   <- 12
TEXT_SIZE_AXIS_TITLE <- 14
TEXT_SIZE_AXIS_TEXT  <- 10
TEXT_SIZE_LEGEND     <- 8

LEGEND_POS_X         <- 0.01              # fraction from left edge of panel
LEGEND_POS_Y         <- 1.2               # fraction from bottom edge of panel
LEGEND_JUST          <- c(0, 1)           # anchor: left-top corner of the legend box
LEGEND_KEY_SIZE      <- unit(0.5, "lines")
LEGEND_DOT_SIZE      <- 1.5               # dot size inside legend keys
LEGEND_BG_FILL       <- alpha("white", 0.75)   # semi-transparent white background
LEGEND_NROW_MEDI     <- 5                 # rows in Panel A (ATC)
LEGEND_NROW_DIAG     <- 7                 # rows in Panel B (ICD-10)

# --- Reference line styling (identity line + zero lines), shared by both panels
REFLINE_DIAG_COLOR     <- "gray50"   # dashed y = x identity line
REFLINE_DIAG_LINEWIDTH <- 1
REFLINE_ZERO_COLOR     <- "gray70"   # dotted lines at x = 0 and y = 0
REFLINE_ZERO_LINEWIDTH <- 0.8

# --- ggrepel label styling, shared by both panels' geom_text_repel() -------
REPEL_TEXT_SIZE     <- 3.5
REPEL_MAX_ITER      <- 50000
REPEL_MAX_TIME      <- 10
REPEL_BOX_PADDING   <- 0.35
REPEL_POINT_PADDING <- 0.25
REPEL_FORCE         <- 5
REPEL_FORCE_PULL    <- 0.5
REPEL_SEGMENT_SIZE  <- 0.5
REPEL_SEED          <- 42

# --- Panel layout & combined-figure export ----------------------------------
PLOT_MARGIN       <- margin(10, 30, 10, 30)   # top, right, bottom, left
PANEL_LABEL_SIZE  <- 22   # font size of "A."/"B." tags
WIDTH             <- 22   # inches
HEIGHT            <- 13   # inches
RES               <- 300  # PNG resolution (dpi)

# =============================================================================
# 4. DATA LOADING & PREPARATION
# =============================================================================

diag_docs    <- read_csv(diag_docs_file)
diag_nondocs <- read_csv(diag_nondocs_file)
medi_docs    <- read_csv(medi_docs_file)
medi_nondocs <- read_csv(medi_nondocs_file)

diag_combined <- diag_docs %>%
  left_join(diag_nondocs, by = "ICD_CODE", suffix = c("_docs", "_nondocs")) %>%
  rename(CODE = ICD_CODE)

medi_combined <- medi_docs %>%
  left_join(medi_nondocs, by = "ATC_CODE", suffix = c("_docs", "_nondocs")) %>%
  rename(CODE = ATC_CODE)

dataset <- bind_rows(
  diag_combined %>% mutate(TYPE = "diagnosis"),
  medi_combined %>% mutate(TYPE = "medication")
)

dataset_diag <- dataset %>%
  filter(TYPE == "diagnosis") %>%
  mutate(CHAPTER = substr(CODE, 1, 1))

dataset_medi <- dataset %>%
  filter(TYPE == "medication") %>%
  mutate(CHAPTER = substr(CODE, 1, 1))

dataset_diag <- dataset_diag %>%
  arrange(TYPE, CHAPTER) %>%
  mutate(CHAPTER = factor(CHAPTER, levels = unique(CHAPTER)))

dataset_medi <- dataset_medi %>%
  arrange(TYPE, CHAPTER) %>%
  mutate(CHAPTER = factor(CHAPTER, levels = unique(CHAPTER)))

dataset_diag <- dataset_diag %>%
  mutate(CHAPTER_NAME = icd10_chapter_map[as.character(CHAPTER)]) %>%
  filter(!is.na(CHAPTER_NAME)) %>%
  mutate(CHAPTER_NAME = factor(CHAPTER_NAME, levels = unique(CHAPTER_NAME)))

dataset_medi <- dataset_medi %>%
  mutate(CHAPTER_NAME = atc_chapter_map[as.character(CHAPTER)]) %>%
  filter(!is.na(CHAPTER_NAME)) %>%
  mutate(CHAPTER_NAME = factor(CHAPTER_NAME, levels = unique(CHAPTER_NAME)))


# =============================================================================
# 5. STATISTICAL COMPARISON FUNCTION
# =============================================================================
# Computes, for each code (ICD-10 or ATC):
# the crude incidence rate (IR) in group A (doctors) and group B (general population), 
# their ratio (IRR), a 95% CI on the IRR (via the log-IRR standard error), 
# and a two-sided p-value for the null hypothesis IRR = 1 (reported on log10 scale)

compare_IRs_crude <- function(
    data,
    events_A, py_A,
    events_B, py_B,
    mult = 1000
) {
  events_A <- rlang::enquo(events_A)
  py_A     <- rlang::enquo(py_A)
  events_B <- rlang::enquo(events_B)
  py_B     <- rlang::enquo(py_B)

  z_crit <- qnorm(0.975)

  data %>%
    dplyr::mutate(
      rate_A    = mult * (!!events_A) / (!!py_A),
      rate_B    = mult * (!!events_B) / (!!py_B),
      IRR       = rate_A / rate_B,
      SE_logIRR = sqrt(1 / (!!events_A) + 1 / (!!events_B)),
      IRR_lo    = exp(log(IRR) - z_crit * SE_logIRR),
      IRR_hi    = exp(log(IRR) + z_crit * SE_logIRR),
      z_IRR     = log(IRR) / SE_logIRR,
      log10_p_IRR = log10(2) + pnorm(abs(z_IRR),lower.tail = FALSE, log.p = TRUE) / log(10) 
)
}

diag_plot <- compare_IRs_crude(
  data     = dataset_diag,
  events_A = events_docs,
  py_A     = person_years_docs,
  events_B = events_nondocs,
  py_B     = person_years_nondocs,
  mult     = 1000
)

medi_plot <- compare_IRs_crude(
  data     = dataset_medi,
  events_A = events_docs,
  py_A     = person_years_docs,
  events_B = events_nondocs,
  py_B     = person_years_nondocs,
  mult     = 1000
)


# =============================================================================
# 6. SAVE & RELOAD FINALIZED DATA (checkpoint before plotting)
# =============================================================================

write_csv(diag_plot, outfile_diag_data_csv)
write_csv(medi_plot, outfile_medi_data_csv)

diag_plot <- read_csv(outfile_diag_data_csv)
medi_plot <- read_csv(outfile_medi_data_csv)

# restore the CHAPTER and CHAPTER_NAME factors to ensure consistent ordering in the plots
diag_plot <- diag_plot %>%
  mutate(
    CHAPTER      = factor(CHAPTER, levels = unique(CHAPTER)),
    CHAPTER_NAME = factor(CHAPTER_NAME, levels = unique(CHAPTER_NAME))
  )

medi_plot <- medi_plot %>%
  mutate(
    CHAPTER      = factor(CHAPTER, levels = unique(CHAPTER)),
    CHAPTER_NAME = factor(CHAPTER_NAME, levels = unique(CHAPTER_NAME))
  )


# =============================================================================
# 7. FIGURE 2 PANEL A – Medication IR scatter: doctors vs general population
# =============================================================================
set.seed(1)

# --- Bonferroni correction -----------------------------------------------
n_tests_medi <- nrow(medi_plot)
medi_plot <- medi_plot %>%
  mutate(
    LOG10_ADJ_PVAL = pmin(
      log10_p_IRR + log10(n_tests_medi),
      0),
    sig_bonf = LOG10_ADJ_PVAL < log10(ALPHA)
  )

n_sig_medi <- sum(medi_plot$sig_bonf, na.rm = TRUE)
medi_plot <- medi_plot %>%
  mutate(
    distance_from_diag = log(adj_IR_1k_docs) - log(adj_IR_1k_nondocs),
    point_alpha        = ifelse(sig_bonf, ALPHA_SIG, ALPHA_NONSIG),
    point_size         = ifelse(sig_bonf, POINT_SIZE_SIG, POINT_SIZE_NONSIG)
  )

# --- Label lists ---------------------------------------------------------
code_labels_extreme <- tibble(
  CODE = c(
    "N04BB01", 
    "M03AX01", 
    "C01CA24", 
    "S01EE03",
    "M01AX17", 
    "N05AH03", 
    "M01AB08", 
    "M01AC01", 
    "M01AB05",
    "H02BX01"
  ),
  LABEL = c(
    "Amantadine\n",
    "Botulinum toxin",
    "Epinephrine\n",
    "Bimatoprost\n(glaucoma)",
    "Nimesulide\n(NSAID)",
    "Olanzapine\n(schizophrenia/bipolar disorder)",
    "Etodolac\n(NSAID)",
    "Piroxicam\n(NSAID)",
    "Diclofenac\n(NSAID)",
    "methylprednisolone, combinations\n(corticosteroid)"
  )
)

code_labels_highlow <- tibble(
  CODE = c(
    "M01AE01", 
    "J01DB01", 
    "N02BE01",
    "G03CX01", 
    "S01EE03", 
    "M01AG01"
  ),
  LABEL = c(
    "Ibuprofen\n(NSAID)",
    "Cefalexin\n(antibiotic)",
    "Paracetamol",
    "Tibolone\n(estrogen)",
    "Bimatoprost\n(glaucoma)",
    "Mefenamic acid\n(NSAID)"
  )
)

code_labels_handpicked <- tibble(
  CODE = c(
    "L01AA01", 
    "R03AC02", 
    "A10AB01", 
    "C09AA03",
    "N02BE01", 
    "R06AX13", 
    "A03FA01", 
    "R05CB06", 
    "D01AE15"
  ),
  LABEL = c(
    "Cyclophosphamide\n(cancer)",
    "Salbutamol\n(asthma/COPD)",
    "Insulin human",
    "Lisinopril\n(ACE inhibitors)",
    "Paracetamol",
    "Loratadine\n(allergy)",
    "Metoclopramide\n(nausea/vomiting)",
    "Ambroxol\n(mucolytic/cough)",
    "Terbinafine\n(fungal infection)"
  )
)

codes_to_label_medi <- unique(c(
  code_labels_extreme$CODE,
  code_labels_highlow$CODE,
  code_labels_handpicked$CODE
))

labeled_points_medi <- medi_plot %>%
  filter(CODE %in% codes_to_label_medi) %>%
  left_join(
    bind_rows(code_labels_extreme, code_labels_handpicked, code_labels_highlow) %>%
      distinct(CODE, .keep_all = TRUE),
    by = "CODE"
  )

# --- Axis limits ---------------------------------------------------------
axis_min_medi <- min(medi_plot$ci_lower_docs, medi_plot$ci_lower_nondocs, na.rm = TRUE)
axis_max_medi <- max(medi_plot$ci_upper_docs, medi_plot$ci_upper_nondocs, na.rm = TRUE)

# --- Build plot ----------------------------------------------------------
fig_2A_new <- ggplot(

    medi_plot,
    aes(x = adj_IR_1k_nondocs, y = adj_IR_1k_docs,color = CHAPTER_NAME)) +

    # Reference lines: dashed y = x identity line, dotted lines at x = 0 / y = 0
    geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = REFLINE_DIAG_COLOR, linewidth = REFLINE_DIAG_LINEWIDTH) +
    geom_vline(xintercept = 0, linetype = "dotted", color = REFLINE_ZERO_COLOR, linewidth = REFLINE_ZERO_LINEWIDTH) +
    geom_hline(yintercept = 0, linetype = "dotted", color = REFLINE_ZERO_COLOR, linewidth = REFLINE_ZERO_LINEWIDTH) +

    # IC95% error bars 
    geom_errorbarh(
        aes(xmin = ci_lower_nondocs, xmax = ci_upper_nondocs),
        alpha = ALPHA_CI, linewidth = CI_ERRORBAR_LINEWIDTH, height = 0
    ) +
    geom_errorbar(
        aes(ymin = ci_lower_docs, ymax = ci_upper_docs),
        alpha = ALPHA_CI, linewidth = CI_ERRORBAR_LINEWIDTH, width = 0
    ) +

    # Points
    geom_point(aes(size = point_size, alpha = point_alpha)) +

    # Labels
    geom_text_repel(
        data               = labeled_points_medi,
        aes(label          = LABEL),
        color              = "black",       
        size               = REPEL_TEXT_SIZE,
        fontface           = "italic",
        show.legend        = FALSE,
        max.overlaps       = Inf,
        max.iter           = REPEL_MAX_ITER,
        max.time           = REPEL_MAX_TIME,
        min.segment.length = 0,
        box.padding        = REPEL_BOX_PADDING,
        point.padding      = REPEL_POINT_PADDING,
        force              = REPEL_FORCE,
        force_pull         = REPEL_FORCE_PULL,
        direction          = "both",
        segment.size       = REPEL_SEGMENT_SIZE,
        segment.alpha      = 1,
        segment.color      = "black",
        seed               = REPEL_SEED
    ) +

    # Axis scales, log10 transformation
    scale_y_log10(limits = c(axis_min_medi, axis_max_medi)) +
    scale_x_log10(limits = c(axis_min_medi, axis_max_medi)) +
    scale_size_identity() +
    scale_alpha_identity() +
    scale_color_manual(values = cb_palette, name = "ATC Chapter") +

    # Labels and titles
    labs(
        title    = "Age & Sex Adjusted IR across Medications, Doctors vs General Population",
        subtitle = sprintf("ATC codes: %d total | Bonferroni-significant differences: %d",nrow(medi_plot), n_sig_medi),
        x = "Adjusted IR (per 1,000 person-years, log scale), General Population",
        y = "Adjusted IR (per 1,000 person-years, log scale), Doctors"
    ) +

    theme_minimal() +
    theme(
        axis.text.x       = element_text(size = TEXT_SIZE_AXIS_TEXT),
        axis.text.y       = element_text(size = TEXT_SIZE_AXIS_TEXT),
        axis.title.x      = element_text(size = TEXT_SIZE_AXIS_TITLE),
        axis.title.y      = element_text(size = TEXT_SIZE_AXIS_TITLE),
        plot.title        = element_text(size = TEXT_SIZE_TITLE),
        plot.subtitle     = element_text(size = TEXT_SIZE_SUBTITLE),
        legend.text       = element_text(size = TEXT_SIZE_LEGEND),
        legend.title      = element_text(size = TEXT_SIZE_LEGEND, face = "bold"),
        legend.position   = c(LEGEND_POS_X, LEGEND_POS_Y),
        legend.justification = LEGEND_JUST,
        legend.direction  = "vertical",
        legend.key.size   = LEGEND_KEY_SIZE,
        legend.background = element_rect(fill = LEGEND_BG_FILL, color = NA),
        plot.margin       = PLOT_MARGIN
    ) +
    guides(
        color = guide_legend(
        nrow          = LEGEND_NROW_MEDI,
        override.aes  = list(size = LEGEND_DOT_SIZE, alpha = 0.8)
        )
    ) +
    coord_fixed(clip = "off")


# =============================================================================
# 8. FIGURE 2 PANEL B – Diagnosis IR scatter: doctors vs general population
# =============================================================================
set.seed(1)

# --- Bonferroni correction -----------------------------------------------
n_tests_diag <- nrow(diag_plot)
diag_plot <- diag_plot %>%
  mutate(
    LOG10_ADJ_PVAL = pmin(
      log10_p_IRR + log10(n_tests_diag),
      0),
    sig_bonf = LOG10_ADJ_PVAL < log10(ALPHA)
  )

n_sig_diag <- sum(diag_plot$sig_bonf, na.rm = TRUE)
diag_plot <- diag_plot %>%
  mutate(
    distance_from_diag = log(adj_IR_1k_docs) - log(adj_IR_1k_nondocs),
    point_alpha        = ifelse(sig_bonf, ALPHA_SIG, ALPHA_NONSIG),
    point_size         = ifelse(sig_bonf, POINT_SIZE_SIG, POINT_SIZE_NONSIG)
  )

# --- Label lists ---------------------------------------------------------

code_labels_extreme <- tibble(
  CODE = c(
    "L57", 
    "H33", 
    "H43", 
    "D48", 
    "Z25",
    "L02", 
    "F10", 
    "N30", 
    "H10"
  ),
  LABEL = c(
    "Skin changes due to chronic\nexposure to nonionizing radiation",
    "Retinal detachments and breaks",
    "Disorders of vitreous body",
    "Neoplasm of uncertain or\nunknown behaviour",
    "Need for prophylactic vaccination",
    "Cutaneous abscess, furuncle\nand carbuncle",
    "Mental and behavioural disorders\ndue to use of alcohol",
    "Cystitis",
    "Conjunctivitis"
  )
)

code_labels_highlow <- tibble(
  CODE = c(
    "K02", 
    "O80", 
    "R10", 
    "R33"
  ),
  LABEL = c(
    "Dental caries",
    "Single spontaneous delivery",
    "Abdominal and pelvic pain",
    "Retention of urine"
  )
)

code_labels_handpicked <- tibble(
  CODE = c(
    "I21", 
    "I50", 
    "I63", 
    "E11",
    "J11", 
    "J00", 
    "M54", 
    "J04",
    "D22",
    "D23"
  ),
  LABEL = c(
    "Acute myocardial infarction",
    "Heart failure",
    "Cerebral infarction",
    "Type 2 diabetes mellitus",
    "Influenza, virus not identified",
    "Acute nasopharyngitis\n(common cold)",
    "Dorsalgia",
    "Acute laryngitis and tracheitis",
    "Melanocytic nevi (moles)",
    "Other benign neoplasms of skin"
  )
)

codes_to_label_diag <- unique(c(
  code_labels_extreme$CODE,
  code_labels_highlow$CODE,
  code_labels_handpicked$CODE
))

labeled_points_diag <- diag_plot %>%
  filter(CODE %in% codes_to_label_diag) %>%
  left_join(
    bind_rows(code_labels_extreme, code_labels_highlow, code_labels_handpicked) %>%
      distinct(CODE, .keep_all = TRUE),
    by = "CODE"
  )

# --- Axis limits ---------------------------------------------------------
axis_min_diag <- min(diag_plot$ci_lower_docs, diag_plot$ci_lower_nondocs, na.rm = TRUE)
axis_max_diag <- max(diag_plot$ci_upper_docs, diag_plot$ci_upper_nondocs, na.rm = TRUE)

# --- Build plot ----------------------------------------------------------
fig_2B_new <- ggplot(

    diag_plot,
    aes(x = adj_IR_1k_nondocs, y = adj_IR_1k_docs, color = CHAPTER_NAME)) +

    # Reference lines: dashed y = x identity line, dotted lines at x = 0 / y = 0
    geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = REFLINE_DIAG_COLOR, linewidth = REFLINE_DIAG_LINEWIDTH) +
    geom_vline(xintercept = 0, linetype = "dotted", color = REFLINE_ZERO_COLOR, linewidth = REFLINE_ZERO_LINEWIDTH) +
    geom_hline(yintercept = 0, linetype = "dotted", color = REFLINE_ZERO_COLOR, linewidth = REFLINE_ZERO_LINEWIDTH) +

    # IC95% error bars  
    geom_errorbarh(
        aes(xmin = ci_lower_nondocs, xmax = ci_upper_nondocs),
        alpha = ALPHA_CI, linewidth = CI_ERRORBAR_LINEWIDTH, height = 0
    ) +
    geom_errorbar(
        aes(ymin = ci_lower_docs, ymax = ci_upper_docs),
        alpha = ALPHA_CI, linewidth = CI_ERRORBAR_LINEWIDTH, width = 0
    ) +

    # Points
    geom_point(aes(size = point_size, alpha = point_alpha)) +

    # Labels 
    geom_text_repel(
      data               = labeled_points_diag,
      aes(label          = LABEL),
      color              = "black",
      size               = REPEL_TEXT_SIZE,
      fontface           = "italic",
      show.legend        = FALSE,
      max.overlaps       = Inf,
      max.iter           = REPEL_MAX_ITER,
      max.time           = REPEL_MAX_TIME,
      min.segment.length = 0,
      box.padding        = REPEL_BOX_PADDING,
      point.padding      = REPEL_POINT_PADDING,
      force              = REPEL_FORCE,
      force_pull         = REPEL_FORCE_PULL,
      direction          = "both",
      segment.size       = REPEL_SEGMENT_SIZE,
      segment.alpha      = 1,
      segment.color      = "black",
      seed               = REPEL_SEED
    ) +

    # Axis scales, log10 transformation
    scale_y_log10(limits = c(axis_min_diag, axis_max_diag)) +
    scale_x_log10(limits = c(axis_min_diag, axis_max_diag)) +
    scale_size_identity() +
    scale_alpha_identity() +
    scale_color_manual(values = cb_palette, name = "ICD-10 Chapter") +

    # Labels and titles
    labs(
        title    = "Age & Sex Adjusted IR across Diagnoses, Doctors vs General Population",
        subtitle = sprintf("ICD-10 codes: %d total | Bonferroni-significant differences: %d",nrow(diag_plot), n_sig_diag),
        x = "Adjusted IR (per 1,000 person-years, log scale), General Population",
        y = "Adjusted IR (per 1,000 person-years, log scale), Doctors"
    ) +

    theme_minimal() +
    theme(
        axis.text.x       = element_text(size = TEXT_SIZE_AXIS_TEXT),
        axis.text.y       = element_text(size = TEXT_SIZE_AXIS_TEXT),
        axis.title.x      = element_text(size = TEXT_SIZE_AXIS_TITLE),
        axis.title.y      = element_text(size = TEXT_SIZE_AXIS_TITLE),
        plot.title        = element_text(size = TEXT_SIZE_TITLE),
        plot.subtitle     = element_text(size = TEXT_SIZE_SUBTITLE),
        legend.text       = element_text(size = TEXT_SIZE_LEGEND),
        legend.title      = element_text(size = TEXT_SIZE_LEGEND, face = "bold"),
        legend.position   = c(LEGEND_POS_X, LEGEND_POS_Y),
        legend.justification = LEGEND_JUST,
        legend.direction  = "vertical",
        legend.key.size   = LEGEND_KEY_SIZE,
        legend.background = element_rect(fill = LEGEND_BG_FILL, color = NA),
        plot.margin       = PLOT_MARGIN
    ) +
    guides(
        color = guide_legend(
        nrow          = LEGEND_NROW_DIAG,
        override.aes  = list(size = LEGEND_DOT_SIZE, alpha = 0.8)
        )
    ) +
    coord_fixed(clip = "off")


# =============================================================================
# EXPORT: Figure 2 – combined panels A and B with grid panel labels
# =============================================================================

add_panel_label <- function(plot, label) {
  # Wraps a ggplot in a grob with a bold panel label in the top-left corner
  g <- ggplotGrob(plot)
  grid.arrange(
    g,
    top = textGrob(
      label,
      x    = unit(0, "npc"),
      just = "left",
      gp   = gpar(fontsize = PANEL_LABEL_SIZE, fontface = "bold")
    )
  )
}

# Build labelled grobs
grob_A <- add_panel_label(fig_2A_new, "A.")
grob_B <- add_panel_label(fig_2B_new, "B.")

# --- PNG export --------------------------------------------------------------
png(
  filename = outfile_fig2_png,
  width    = WIDTH,
  height   = HEIGHT,
  units    = "in",
  res      = RES
)
grid.arrange(grob_A, grob_B, ncol = 2)
dev.off()

# --- PDF export --------------------------------------------------------------
pdf(
  file   = outfile_fig2_pdf,
  width  = WIDTH,
  height = HEIGHT
)
grid.arrange(grob_A, grob_B, ncol = 2)
dev.off()

# =============================================================================
# CSV EXPORT – All diagnosis and medication results (for Supplements)
# =============================================================================

# --- Diagnosis results -------------------------------------------------------
m_diag <- nrow(diag_plot)
bonf_diag <- ALPHA / m_diag
log10_bonf_diag <- log10(bonf_diag)

diag_csv_docs <- diag_plot %>%
  transmute(
    CODE            = CODE,
    SOURCE          = "diagnosis (ICD10)",
    GROUP           = "doctor",
    ADJ_IR_1K       = adj_IR_1k_docs,
    CI95_LOW        = ci_lower_docs,
    CI95_HIGH       = ci_upper_docs,
    N_EVENTS        = events_docs,
    TOT_PERSON_YEAR = person_years_docs,
    LOG10_PVAL      = log10_p_IRR,
    LOG10_ADJ_PVAL  = LOG10_ADJ_PVAL,
    ADJ_SIGNIFICANT = (LOG10_ADJ_PVAL <= log10_bonf_diag)
  )

diag_csv_nondocs <- diag_plot %>%
  transmute(
    CODE            = CODE,
    SOURCE          = "diagnosis (ICD10)",
    GROUP           = "general population",
    ADJ_IR_1K       = adj_IR_1k_nondocs,
    CI95_LOW        = ci_lower_nondocs,
    CI95_HIGH       = ci_upper_nondocs,
    N_EVENTS        = events_nondocs,
    TOT_PERSON_YEAR = person_years_nondocs,
    LOG10_PVAL      = log10_p_IRR,
    LOG10_ADJ_PVAL  = LOG10_ADJ_PVAL,
    ADJ_SIGNIFICANT = (LOG10_ADJ_PVAL <= log10_bonf_diag)
  )

# --- Medication results ------------------------------------------------------
m_medi <- nrow(medi_plot)
bonf_medi <- ALPHA / m_medi
log10_bonf_medi <- log10(bonf_medi)

medi_csv_docs <- medi_plot %>%
  transmute(
    CODE            = CODE,
    SOURCE          = "medication (ATC)",
    GROUP           = "doctor",
    ADJ_IR_1K       = adj_IR_1k_docs,
    CI95_LOW        = ci_lower_docs,
    CI95_HIGH       = ci_upper_docs,
    N_EVENTS        = events_docs,
    TOT_PERSON_YEAR = person_years_docs,
    LOG10_PVAL      = log10_p_IRR,
    LOG10_ADJ_PVAL  = LOG10_ADJ_PVAL,
    ADJ_SIGNIFICANT = (LOG10_ADJ_PVAL <= log10_bonf_medi)
  )

medi_csv_nondocs <- medi_plot %>%
  transmute(
    CODE            = CODE,
    SOURCE          = "medication (ATC)",
    GROUP           = "general population",
    ADJ_IR_1K       = adj_IR_1k_nondocs,
    CI95_LOW        = ci_lower_nondocs,
    CI95_HIGH       = ci_upper_nondocs,
    N_EVENTS        = events_nondocs,
    TOT_PERSON_YEAR = person_years_nondocs,
    LOG10_PVAL      = log10_p_IRR,
    LOG10_ADJ_PVAL  = LOG10_ADJ_PVAL,
    ADJ_SIGNIFICANT = (LOG10_ADJ_PVAL <= log10_bonf_medi)
  )

# --- Combine and export ------------------------------------------------------
all_results_csv <- bind_rows(
  diag_csv_docs,
  diag_csv_nondocs,
  medi_csv_docs,
  medi_csv_nondocs
) %>%
  arrange(SOURCE, CODE, GROUP)

write_csv(
  all_results_csv,
  outfile_results_csv
)