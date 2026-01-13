# -------------------------------------------
# Libraries
# -------------------------------------------

.libPaths("/shared-directory/sd-tools/apps/R/lib/")
library(ggplot2)
library(dplyr)
library(readr)
library(tidyr)
library(scales)
library(patchwork)
library(ggrepel)
library(data.table)
library(R.utils)

# -------------------------------------------
# File paths
# -------------------------------------------

InDir = "/media/volume/Projects/jg/Output_files"
diag_docs_file <- file.path(InDir, "diag_docs_results_20251125_162616.csv")
diag_nondocs_file <- file.path(InDir, "diag_nondocs_results_20251125_184815.csv")
medi_docs_file <- file.path(InDir, "medi_docs_results_20251127_102704.csv")
medi_nondocs_file <- file.path(InDir, "medi_nondocs_results_20251127_115429.csv")

DATE <- format(Sys.time(), "%Y%m%d")
OutDir <- paste0("/media/volume/Projects/DSGELabProject1/Plots/Figure2_", DATE)
dir.create(OutDir, showWarnings = FALSE, recursive = TRUE)

# -------------------------------------------
# Global Variables
# -------------------------------------------

# ATC Chapter names mapping
# Based on WHO ATC classification system
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

# Color-blind friendly palette
cb_palette <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", 
                "#D55E00", "#CC79A7", "#999999", "#000000", "#E6AB02",
                "#7570B3", "#66A61E", "#E7298A", "#A6761D", "#666666",
                "#1B9E77", "#D95F02", "#7570B3", "#E7298A", "#66A61E", "#E6AB02")


# Plotting parameters
JITTER_RANGE <- 0.3
TOP_EXTREME_COUNT <- 10
TOP_CHAPTER_COUNT <- 3
POINT_SIZE <- 3
BAR_SIZE <- 0.8
BAR_WIDTH <- 0.2
TEXT_SIZE_TITLE <- 16
TEXT_SIZE_SUBTITLE <- 12
TEXT_SIZE_AXIS_TITLE <- 14
TEXT_SIZE_AXIS_TEXT <- 10
TEXT_SIZE_LEGEND <- 12

# -----------------------------------------------------------
# Data processing & preparation
# -----------------------------------------------------------

# Load data
diag_docs <- read_csv(diag_docs_file)
diag_nondocs <- read_csv(diag_nondocs_file)
medi_docs <- read_csv(medi_docs_file)
medi_nondocs <- read_csv(medi_nondocs_file)

# Join diangosis/medication data for doctors and general population
diag_combined <- diag_docs %>% left_join(diag_nondocs, by = "ICD_CODE", suffix = c("_docs", "_nondocs")) %>% rename(CODE = ICD_CODE)
medi_combined <- medi_docs %>%left_join(medi_nondocs, by = "ATC_CODE", suffix = c("_docs", "_nondocs")) %>% rename(CODE = ATC_CODE)
# Combine diagnosis and medication data 
dataset <- bind_rows(
    diag_combined %>% mutate(TYPE = "diagnosis"),
    medi_combined %>% mutate(TYPE = "medication")
) 

# Extract diagnosis/medication chapter from CODE
dataset_diag <- dataset %>% filter(TYPE == "diagnosis") %>% mutate(CHAPTER = substr(CODE, 1, 1))
dataset_medi <- dataset %>% filter(TYPE == "medication") %>% mutate(CHAPTER = substr(CODE, 1, 1))

# Sort CHAPTER alphabetically for x axis
dataset_diag <- dataset_diag %>%
    arrange(TYPE, CHAPTER) %>%
    mutate(CHAPTER = factor(CHAPTER, levels = unique(CHAPTER)))
dataset_medi <- dataset_medi %>%
    arrange(TYPE, CHAPTER) %>%
    mutate(CHAPTER = factor(CHAPTER, levels = unique(CHAPTER)))

# Map chapter letters to full names, remove unknown levels
dataset_diag <- dataset_diag %>%
    mutate(CHAPTER_NAME = icd10_chapter_map[as.character(CHAPTER)]) %>%
    filter(!is.na(CHAPTER_NAME)) %>%
    mutate(CHAPTER_NAME = factor(CHAPTER_NAME, levels = unique(CHAPTER_NAME)))
dataset_medi <- dataset_medi %>%
    mutate(CHAPTER_NAME = atc_chapter_map[as.character(CHAPTER)]) %>%
    filter(!is.na(CHAPTER_NAME)) %>%
    mutate(CHAPTER_NAME = factor(CHAPTER_NAME, levels = unique(CHAPTER_NAME)))


# Compare Incidence Rates (IRs) between doctors and non-doctors & calculate p-values
compare_IRs_crude <- function(
  data,
  events_A, py_A,
  events_B, py_B,
  mult = 1000  # e.g. 1000 for "per 1,000 Person Years (PY)"
) {
  # Tidy evaluation setup
  events_A <- rlang::enquo(events_A)
  py_A     <- rlang::enquo(py_A)
  events_B <- rlang::enquo(events_B)
  py_B     <- rlang::enquo(py_B)
  
  data %>%
    dplyr::mutate(
      # Crude incidence rates
      IR_A      = mult * (!!events_A) / (!!py_A),
      IR_B      = mult * (!!events_B) / (!!py_B),
      
      # --- Crude IRR (A vs B) ---
      IRR       = ((!!events_A) / (!!py_A)) / ((!!events_B) / (!!py_B)),
      SE_logIRR = sqrt(1 / (!!events_A) + 1 / (!!events_B)),
      IRR_lo    = exp(log(IRR) - 1.96 * SE_logIRR),
      IRR_hi    = exp(log(IRR) + 1.96 * SE_logIRR),
      z_IRR     = log(IRR) / SE_logIRR,
      p_IRR     = 2 * (1 - pnorm(abs(z_IRR))),
      
      # --- Crude rate difference (A - B), per `mult` PY ---
      rate_diff = IR_A - IR_B,
      # Var(rate_per_mult) ≈ mult * rate_per_mult / PY for crude Poisson
      SE_diff   = sqrt(
        mult * IR_A / (!!py_A) +
        mult * IR_B / (!!py_B)
      ),
      z_diff    = rate_diff / SE_diff,
      p_diff    = 2 * (1 - pnorm(abs(z_diff)))
    )
}

# for diagnoses
diag_plot <- compare_IRs_crude(
  data = dataset_diag,
  events_A = events_docs,
  py_A     = person_years_docs,
  events_B = events_nondocs,
  py_B     = person_years_nondocs,
  mult     = 1000   # rates per 1,000 PY
)

# for medications
medi_plot <- compare_IRs_crude(
  data = dataset_medi,
  events_A = events_docs,
  py_A     = person_years_docs,
  events_B = events_nondocs,
  py_B     = person_years_nondocs,
  mult     = 1000   # rates per 1,000 PY
)

# -----------------------------------------------------------
# Figure 2A
# -----------------------------------------------------------

# Set seed for reproducible jittering in plots
set.seed(1)

# Identify top 10 IRs
top_10_overall <- diag_plot %>%
    slice_max(order_by = abs(adj_IR_1k_docs), n = TOP_EXTREME_COUNT) %>%
    pull(CODE)

# Identify top 3 IRs per ICD10 chapter
top_3_per_chapter <- diag_plot %>%
    group_by(CHAPTER) %>%
    slice_max(order_by = abs(adj_IR_1k_docs), n = TOP_CHAPTER_COUNT) %>%
    ungroup() %>%
    pull(CODE)

# Get union of top 10 overall and top 3 per chapter
codes_to_label <- union(top_10_overall, top_3_per_chapter)
labeled_points <- diag_plot %>% filter(CODE %in% codes_to_label)

# Order CHAPTER_NAME by CHAPTER levels (inverted)
diag_plot <- diag_plot %>%
    arrange(desc(CHAPTER)) %>%
    mutate(CHAPTER_NAME = factor(CHAPTER_NAME, levels = unique(CHAPTER_NAME)))

# Calculate size and alpha within each chapter based on IR
diag_plot <- diag_plot %>%
    mutate(
        point_size = scales::rescale(abs(adj_IR_1k_docs), to = c(3, 4)),
        point_alpha = scales::rescale(abs(adj_IR_1k_docs), to = c(0.4, 0.8)),
        bar_size = scales::rescale(abs(adj_IR_1k_docs), to = c(0.3, 0.6)),
        bar_width = scales::rescale(abs(adj_IR_1k_docs), to = c(0.05, 0.3))
    )

# Add jittered positions manually for all points
diag_plot$x_jittered <- as.numeric(diag_plot$CHAPTER_NAME) + runif(nrow(diag_plot), -JITTER_RANGE, JITTER_RANGE)
labeled_points <- diag_plot %>% filter(CODE %in% codes_to_label)

# Plot incidence rates diagnoses docs 
fig_2A <- ggplot(diag_plot, aes(x = x_jittered, y = adj_IR_1k_docs, color = CHAPTER_NAME)) +
    geom_errorbar(aes(ymin = ci_lower_docs, ymax = ci_upper_docs, size = bar_size, alpha = point_alpha), width = diag_plot$bar_width) +
    geom_point(aes(size = point_size, alpha = point_alpha, stroke = 1)) +
    geom_text_repel(data = labeled_points, 
                    aes(label = CODE), 
                    size = 4, 
                    fontface = "bold",
                    show.legend = FALSE,
                    max.overlaps = Inf,
                    min.segment.length = 0,
                    box.padding = 0.5,
                    point.padding = 0.3,
                    force = 2,
                    force_pull = 0.5) +
    scale_x_continuous(
        breaks = 1:length(levels(diag_plot$CHAPTER_NAME)),
        labels = levels(diag_plot$CHAPTER_NAME)
    ) +
    scale_size_identity() +
    scale_alpha_identity() +
    scale_color_manual(values = cb_palette, name = "Chapter", guide = "none") +
    labs(
        title = "Age & Sex Adjusted Incidence Rates (IR) across Diagnoses - Doctors only",
        subtitle = paste0("Number of ICD-10 codes considered: ", nrow(diag_plot)),
        x = "ICD-10 Code Chapter",
        y = "Adjusted IR (per 1,000 person-years)"
    ) +
    theme_minimal() +
    theme(
        axis.text.x = element_text(hjust = 1, size = TEXT_SIZE_AXIS_TEXT),
        axis.text.y = element_text(size = TEXT_SIZE_AXIS_TEXT),
        axis.title.x = element_text(size = TEXT_SIZE_AXIS_TITLE),
        axis.title.y = element_text(size = TEXT_SIZE_AXIS_TITLE),
        plot.title = element_text(size = TEXT_SIZE_TITLE),
        plot.subtitle = element_text(size = TEXT_SIZE_SUBTITLE),
        legend.position = "none" 
    ) +
    coord_flip()

# Save results
ggsave(
    filename = file.path(OutDir, "Figure2A.pdf"),
    plot = fig_2A,
    width = 20,
    height = 12,
    dpi = 300
)

ggsave(
    filename = file.path(OutDir, "Figure2A.png"),
    plot = fig_2A,
    width = 20,
    height = 12,
    dpi = 300
)

# -----------------------------------------------------------
# Figure 2B
# -----------------------------------------------------------

# Set seed for reproducible jittering in plots
set.seed(1)

# Identify top 10 IRs
top_10_overall <- medi_plot %>%
    slice_max(order_by = abs(adj_IR_1k_docs), n = TOP_EXTREME_COUNT) %>%
    pull(CODE)

# Identify top 3 IRs per ATC chapter
top_3_per_chapter <- medi_plot %>%
    group_by(CHAPTER) %>%
    slice_max(order_by = abs(adj_IR_1k_docs), n = TOP_CHAPTER_COUNT) %>%
    ungroup() %>%
    pull(CODE)

# Get union of top 10 overall and top 3 per chapter
codes_to_label <- union(top_10_overall, top_3_per_chapter)
labeled_points <- medi_plot %>% filter(CODE %in% codes_to_label)

# Order CHAPTER_NAME by CHAPTER levels (inverted)
medi_plot <- medi_plot %>%
    arrange(desc(CHAPTER)) %>%
    mutate(CHAPTER_NAME = factor(CHAPTER_NAME, levels = unique(CHAPTER_NAME)))

# Calculate size and alpha within each chapter based on IR
medi_plot <- medi_plot %>%
    mutate(
        point_size = scales::rescale(abs(adj_IR_1k_docs), to = c(3, 4)),
        point_alpha = scales::rescale(abs(adj_IR_1k_docs), to = c(0.4, 0.8)),
        bar_size = scales::rescale(abs(adj_IR_1k_docs), to = c(0.3, 0.6)),
        bar_width = scales::rescale(abs(adj_IR_1k_docs), to = c(0.05, 0.3))
    )

# Add jittered positions manually for all points
medi_plot$x_jittered <- as.numeric(medi_plot$CHAPTER_NAME) + runif(nrow(medi_plot), -JITTER_RANGE, JITTER_RANGE)
labeled_points <- medi_plot %>% filter(CODE %in% codes_to_label)

# Plot incidence rates medications docs 
fig_2B <- ggplot(medi_plot, aes(x = x_jittered, y = adj_IR_1k_docs, color = CHAPTER_NAME)) +
    geom_errorbar(aes(ymin = ci_lower_docs, ymax = ci_upper_docs, size = bar_size, alpha = point_alpha), width = medi_plot$bar_width) +
    geom_point(aes(size = point_size, alpha = point_alpha)) +
    geom_text_repel(data = labeled_points, 
                    aes(label = CODE), 
                    size = 4, 
                    fontface = "bold",
                    show.legend = FALSE,
                    max.overlaps = Inf,
                    min.segment.length = 0,
                    box.padding = 0.5,
                    point.padding = 0.3,
                    force = 2,
                    force_pull = 0.5) +
    scale_x_continuous(
        breaks = 1:length(levels(medi_plot$CHAPTER_NAME)),
        labels = levels(medi_plot$CHAPTER_NAME)
    ) +
    scale_size_identity() +
    scale_alpha_identity() +
    scale_color_manual(values = cb_palette, name = "Chapter", guide = "none") +
    labs(
        title = "Age & Sex Adjusted Incidence Rates (IR) across Medications - Doctors only",
        subtitle = paste0("Number of ATC codes considered: ", nrow(medi_plot)),
        x = "ATC Code Chapter",
        y = "Adjusted IR (per 1,000 person-years)"
    ) +
    theme_minimal() +
    theme(
        axis.text.x = element_text(hjust = 1, size = TEXT_SIZE_AXIS_TEXT),
        axis.text.y = element_text(size = TEXT_SIZE_AXIS_TEXT),
        axis.title.x = element_text(size = TEXT_SIZE_AXIS_TITLE),
        axis.title.y = element_text(size = TEXT_SIZE_AXIS_TITLE),
        plot.title = element_text(size = TEXT_SIZE_TITLE),
        plot.subtitle = element_text(size = TEXT_SIZE_SUBTITLE),
        legend.position = "none" 
    ) +
    coord_flip()

# Save results
ggsave(
    filename = file.path(OutDir, "Figure2B.pdf"),
    plot = fig_2B,
    width = 20,
    height = 12,
    dpi = 300
)

ggsave(
    filename = file.path(OutDir, "Figure2B.png"),
    plot = fig_2B,
    width = 20,
    height = 12,
    dpi = 300
)


# -----------------------------------------------------------
# Figure 2C 
# -----------------------------------------------------------

# Set seed for reproducible jittering in plots
set.seed(1)

TOP_EXTREME_COUNT = 5
# Identify top 5 IRs - furthest above the diagonal line (doctors > general population)
top_above <- medi_plot %>%
    mutate(distance_from_diag = log(adj_IR_1k_docs) - log(adj_IR_1k_nondocs)) %>%
    slice_max(order_by = distance_from_diag, n = TOP_EXTREME_COUNT) %>%
    pull(CODE)

# Identify top 5 IRs - furthest below the diagonal line (doctors < general population)
top_below <- medi_plot %>%
    mutate(distance_from_diag = log(adj_IR_1k_docs) - log(adj_IR_1k_nondocs)) %>%
    slice_min(order_by = distance_from_diag, n = TOP_EXTREME_COUNT) %>%
    pull(CODE)

# Select specific medications for labeling
code_labels <- tibble(
    CODE = c(
        "S01EE03", "S01EC01", "H02AB09", "M03AX01", "N04BB01", "H02BX01", "D06BB03", "D10AX03", "A07DA03", "C01CA24", 
        "M01AB08", "M01AC01", "N05AH03", "J01DA09", "M01AX17", "R05FB02", "M02AA15", "M03BC51", "M01AC06", "M01AB05"
    ),
    LABEL = c("bimatoprost", "acetazolamide", "hydrocortisone", "botulinum toxin","amantadine",
              "methylprednisolone, combinations", "aciclovir", "azelaic acid", "loperamide", "epinephrine",
              "etodolac", "piroxicam", "olanzapine ", "cefadroxil (renamed in 2005 to J01DB05)", "nimesulide ", 
              "cough suppressants and expectorants", "diclofenac", "orphenadrine, combinations", "meloxicam", "diclofenac" 
    )
)

# Get union of top 5 above and top 5 below
codes_to_label <- union(top_above, top_below)
labeled_points <- medi_plot %>% filter(CODE %in% codes_to_label) %>% inner_join(code_labels, by = "CODE")

# Determine axis limits to make them equal
axis_min <- min(medi_plot$ci_lower_docs, medi_plot$ci_lower_nondocs, na.rm = TRUE)
axis_max <- max(medi_plot$ci_upper_docs, medi_plot$ci_upper_nondocs, na.rm = TRUE)

# Calculate size and alpha based on distance from the diagonal
medi_plot <- medi_plot %>%
    mutate(
        distance_from_diag = log(adj_IR_1k_docs) - log(adj_IR_1k_nondocs),
        point_size = scales::rescale(abs(distance_from_diag), to = c(2, 6)),
        point_alpha = scales::rescale(abs(distance_from_diag), to = c(0.2, 0.5)),
        bar_size = scales::rescale(abs(distance_from_diag), to = c(0.3, 0.6))
    )

# Plot medications: doctors vs non-doctors
fig_2C <- ggplot(medi_plot, aes(x = adj_IR_1k_nondocs, y = adj_IR_1k_docs, color = CHAPTER_NAME)) +
    geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "gray50", linewidth = 1) +
    geom_vline(xintercept = 0, linetype = "dotted", color = "gray70", linewidth = 1) +
    geom_hline(yintercept = 0, linetype = "dotted", color = "gray70", linewidth = 1) +
    geom_errorbar(aes(ymin = ci_lower_docs, ymax = ci_upper_docs, size = bar_size, alpha = point_alpha), width = 0) +
    geom_errorbarh(aes(xmin = ci_lower_nondocs, xmax = ci_upper_nondocs, size = bar_size, alpha = point_alpha), height = 0) +
    geom_point(aes(size = point_size, alpha = point_alpha)) +
    geom_text_repel(data = labeled_points, 
                    aes(label =  paste0(CODE, ": ", LABEL)),
                    size = 5,
                    fontface = "bold.italic",
                    show.legend = FALSE,
                    max.overlaps = Inf,
                    min.segment.length = 0,
                    box.padding = 0.5,
                    point.padding = 0.3,
                    force = 2,
                    force_pull = 0.5) +
    scale_y_log10(limits = c(axis_min, axis_max)) +
    scale_x_log10(limits = c(axis_min, axis_max)) +
    scale_size_identity() +
    scale_alpha_identity() +
    scale_color_manual(values = cb_palette, name = "Chapter") +
    labs(
        title = "Age & Sex Adjusted Incidence Rates (IR) across Medications - Doctors vs General Population",
        subtitle = sprintf("Number of ATC codes considered: %d", nrow(medi_plot)),
        x = "Adjusted IR (per 1,000 person-years, log scale) - General Population",
        y = "Adjusted IR (per 1,000 person-years, log scale) - Doctors"
    ) +
    theme_minimal() +
    theme(
        axis.text.x = element_text(size = TEXT_SIZE_AXIS_TEXT),
        axis.text.y = element_text(size = TEXT_SIZE_AXIS_TEXT),
        axis.title.x = element_text(size = TEXT_SIZE_AXIS_TITLE),
        axis.title.y = element_text(size = TEXT_SIZE_AXIS_TITLE),
        plot.title = element_text(size = TEXT_SIZE_TITLE),
        plot.subtitle = element_text(size = TEXT_SIZE_SUBTITLE),
        legend.text = element_text(size = TEXT_SIZE_LEGEND),
        legend.title = element_text(size = TEXT_SIZE_LEGEND),
        legend.position = "right"
    ) +
    coord_fixed()

# Save results
ggsave(
    filename = file.path(OutDir, "Figure2C.pdf"),
    plot = fig_2C,
    width = 20,
    height = 12,
    dpi = 300
)

ggsave(
    filename = file.path(OutDir, "Figure2C.png"),
    plot = fig_2C,
    width = 20,
    height = 12,
    dpi = 300
)


