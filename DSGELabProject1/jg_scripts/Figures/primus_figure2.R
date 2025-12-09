# Libraries
# .libPaths("/shared-directory/sd-tools/apps/R/lib/")
library(ggplot2)
library(dplyr)
library(readr)
library(tidyr)
library(scales)
library(patchwork)
library(ggrepel)
library(data.table)
library(R.utils)

# Plot directory
plot_dir <- "/path/to/plots"

# Create timestamp for file names
timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")

# Read files
diag_docs <- fread("../Output_files/diag_docs_results.csv")
diag_nondocs <- fread("../Output_files/diag_nondocs_results.csv")
medi_docs <- fread("../Output_files/medi_docs_results.csv")
medi_nondocs <- fread("../Output_files/medi_nondocs_results.csv")

# Preprocess files
# Left join diag_docs and diag_nondocs by ICD_CODE
diag_combined <- diag_docs %>%
    left_join(diag_nondocs, by = "ICD_CODE", suffix = c("_docs", "_nondocs")) %>%
    rename(CODE = ICD_CODE)

# Left join medi_docs and medi_nondocs by OUTCOME_CODE
medi_combined <- medi_docs %>%
    left_join(medi_nondocs, by = "ATC_CODE", suffix = c("_docs", "_nondocs")) %>%
    rename(CODE = ATC_CODE)

# Combine diagnosis and medication data
dataset <- bind_rows(
    diag_combined %>% mutate(TYPE = "diagnosis"),
    medi_combined %>% mutate(TYPE = "medication")
) 

dataset_diag <- dataset %>%
    filter(TYPE == "diagnosis")
dataset_medi <- dataset %>%
    filter(TYPE == "medication")


# Extract diagnosis/medication chapter from CODE
dataset_diag <- dataset_diag %>%
    mutate(CHAPTER = substr(CODE, 1, 1))
dataset_medi <- dataset_medi %>%
    mutate(CHAPTER = substr(CODE, 1, 1))

# Sort CHAPTER alphabetically for x axis
dataset_diag <- dataset_diag %>%
    arrange(TYPE, CHAPTER) %>%
    mutate(CHAPTER = factor(CHAPTER, levels = unique(CHAPTER)))
dataset_medi <- dataset_medi %>%
    arrange(TYPE, CHAPTER) %>%
    mutate(CHAPTER = factor(CHAPTER, levels = unique(CHAPTER)))

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

# ICD Chapter names mapping
# Based on ICD-10 classification system
icd_chapter_map <- c(
    "A" = "Infectious and Parasitic Diseases",
    "B" = "Infectious and Parasitic Diseases",
    "C" = "Neoplasms",
    "D" = "Neoplasms / Blood and Blood-forming Organs",
    "E" = "Endocrine, Nutritional and Metabolic Diseases",
    "F" = "Mental and Behavioural Disorders",
    "G" = "Nervous System",
    "H" = "Eye and Adnexa / Ear and Mastoid Process",
    "I" = "Circulatory System",
    "J" = "Respiratory System",
    "K" = "Digestive System",
    "L" = "Skin and Subcutaneous Tissue",
    "M" = "Musculoskeletal System and Connective Tissue",
    "N" = "Genitourinary System",
    "O" = "Pregnancy, Childbirth and Puerperium",
    "P" = "Conditions Originating in Perinatal Period",
    "Q" = "Congenital Malformations and Chromosomal Abnormalities",
    "R" = "Symptoms, Signs and Abnormal Findings NEC",
    "S" = "Injury, Poisoning and External Causes",
    "T" = "Injury, Poisoning and External Causes",
    "V" = "External Causes of Morbidity and Mortality",
    "W" = "External Causes of Morbidity and Mortality",
    "X" = "External Causes of Morbidity and Mortality",
    "Y" = "External Causes of Morbidity and Mortality",
    "Z" = "Factors Influencing Health Status"
)


# Map chapter letters to full names, remove unknown levels
dataset_diag <- dataset_diag %>%
    mutate(CHAPTER_NAME = icd_chapter_map[as.character(CHAPTER)]) %>%
    filter(!is.na(CHAPTER_NAME)) %>%
    mutate(CHAPTER_NAME = factor(CHAPTER_NAME, levels = unique(CHAPTER_NAME)))
dataset_medi <- dataset_medi %>%
    mutate(CHAPTER_NAME = atc_chapter_map[as.character(CHAPTER)]) %>%
    filter(!is.na(CHAPTER_NAME)) %>%
    mutate(CHAPTER_NAME = factor(CHAPTER_NAME, levels = unique(CHAPTER_NAME)))


# Color-blind friendly palette
cb_palette <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", 
                "#D55E00", "#CC79A7", "#999999", "#000000", "#E6AB02",
                "#7570B3", "#66A61E", "#E7298A", "#A6761D", "#666666",
                "#1B9E77", "#D95F02", "#7570B3", "#E7298A", "#66A61E", "#E6AB02")


# Individual points
# Identify top 10 extreme values by adjIR
top_extreme_diag_docs <- dataset_diag %>%
    filter(TYPE == "diagnosis") %>%
    filter(abs(adj_IR_1k_docs) >= sort(abs(adj_IR_1k_docs), decreasing = TRUE)[10])

top_extreme_medi_docs <- dataset_medi %>%
    filter(TYPE == "medication") %>%
    filter(abs(adj_IR_1k_docs) >= sort(abs(adj_IR_1k_docs), decreasing = TRUE)[10]) 

top_extreme_diag_nondocs <- dataset_diag %>%
    filter(TYPE == "diagnosis") %>%
    filter(abs(adj_IR_1k_nondocs) >= sort(abs(adj_IR_1k_nondocs), decreasing = TRUE)[10])

top_extreme_medi_nondocs <- dataset_medi %>%
    filter(TYPE == "medication") %>%
    filter(abs(adj_IR_1k_nondocs) >= sort(abs(adj_IR_1k_nondocs), decreasing = TRUE)[10])


# Set seed for reproducible jittering
set.seed(42)
# Global Variables for Plotting
JITTER_RANGE <- 0.2
TOP_EXTREME_COUNT <- 10
POINT_SIZE_FDR <- 4
POINT_SIZE_PVAL <- 4
POINT_SIZE_NOT_SIG <- 4
ALPHA_FDR <- 1
ALPHA_PVAL <- 1
ALPHA_NOT_SIG <- 0.2
TEXT_SIZE_TITLE <- 16
TEXT_SIZE_AXIS_TITLE <- 14
TEXT_SIZE_AXIS_TEXT <- 10
TEXT_SIZE_LEGEND <- 12

# Add jittered positions to both datasets
dataset_diag$x_jittered <- as.numeric(dataset_diag$CHAPTER_NAME) + runif(nrow(dataset_diag), -JITTER_RANGE, JITTER_RANGE)
dataset_medi$x_jittered <- as.numeric(dataset_medi$CHAPTER_NAME) + runif(nrow(dataset_medi), -JITTER_RANGE, JITTER_RANGE)
top_extreme_diag_docs$x_jittered <- dataset_diag$x_jittered[match(interaction(top_extreme_diag_docs$CHAPTER_NAME, top_extreme_diag_docs$CODE), interaction(dataset_diag$CHAPTER_NAME, dataset_diag$CODE))]
top_extreme_medi_docs$x_jittered <- dataset_medi$x_jittered[match(interaction(top_extreme_medi_docs$CHAPTER_NAME, top_extreme_medi_docs$CODE), interaction(dataset_medi$CHAPTER_NAME, dataset_medi$CODE))]
top_extreme_diag_nondocs$x_jittered <- dataset_diag$x_jittered[match(interaction(top_extreme_diag_nondocs$CHAPTER_NAME, top_extreme_diag_nondocs$CODE), interaction(dataset_diag$CHAPTER_NAME, dataset_diag$CODE))]
top_extreme_medi_nondocs$x_jittered <- dataset_medi$x_jittered[match(interaction(top_extreme_medi_nondocs$CHAPTER_NAME, top_extreme_medi_nondocs$CODE), interaction(dataset_medi$CHAPTER_NAME, dataset_medi$CODE))]

# Plot incidence rates diagnoses docs 
p_diag_docs <- ggplot(dataset_diag %>% filter(TYPE == "diagnosis"), aes(x = x_jittered, y = adj_IR_1k_docs, color = CHAPTER_NAME)) +
    geom_errorbar(aes(ymin = ci_lower_docs, ymax = ci_upper_docs), 
                                width = 0.1, alpha = 0.5) +
    geom_point() +
    geom_text_repel(data = top_extreme_diag_docs, aes(label = CODE), 
                        size = 4, 
                        show.legend = FALSE,
                        max.overlaps = Inf,
                        min.segment.length = 0,
                        box.padding = 0.5,
                        point.padding = 0.3,
                        force = 2,
                        force_pull = 0.5) +
    scale_x_continuous(
        breaks = 1:length(levels(dataset_diag$CHAPTER_NAME)),
        labels = levels(dataset_diag$CHAPTER_NAME)
    ) +
    scale_y_log10() +
    scale_color_manual(values = cb_palette, name = "Chapter", guide = "none") +
    labs(
        title = "Age and Sex Adjusted Incidence Rates across Diagnoses (Doctors)",
        x = "ICD-10 Code Chapter",
        y = "Adjusted Incidence Rate\n(per 1,000 person-years, log scale)"
    ) +
    theme_minimal() +
    theme(
        axis.text.x = element_text(hjust = 1, size = TEXT_SIZE_AXIS_TEXT),
        axis.text.y = element_text(size = TEXT_SIZE_AXIS_TEXT),
        axis.title.x = element_text(size = TEXT_SIZE_AXIS_TITLE),
        axis.title.y = element_text(size = TEXT_SIZE_AXIS_TITLE),
        plot.title = element_text(size = TEXT_SIZE_TITLE),
        legend.text = element_text(size = TEXT_SIZE_LEGEND),
        legend.title = element_text(size = TEXT_SIZE_LEGEND),
        legend.position = "right" 
    )+
    coord_flip()

# Plot incidence rates diagnoses nondocs
p_diag_nondocs <- ggplot(dataset_diag %>% filter(TYPE == "diagnosis"), aes(x = x_jittered, y = adj_IR_1k_nondocs, color = CHAPTER_NAME)) +
    geom_errorbar(aes(ymin = ci_lower_nondocs, ymax = ci_upper_nondocs), 
                                width = 0.1, alpha = 0.5) +
    geom_point() +
    geom_text_repel(data = top_extreme_diag_nondocs, aes(label = CODE), 
                        size = 4, 
                        show.legend = FALSE,
                        max.overlaps = Inf,
                        min.segment.length = 0,
                        box.padding = 0.5,
                        point.padding = 0.3,
                        force = 2,
                        force_pull = 0.5) +
    scale_y_log10() +
    scale_x_continuous(
        breaks = 1:length(levels(dataset_diag$CHAPTER_NAME)),
        labels = levels(dataset_diag$CHAPTER_NAME)
    ) +
    scale_color_manual(values = cb_palette, name = "Chapter", guide = "none") +
    labs(
        title = "Age and Sex Adjusted Incidence Rates across Diagnoses (General Population w/o Doctors)",
        x = "ICD-10 Code Chapter",
        y = "Adjusted Incidence Rate\n(per 1,000 person-years)"
    ) +
    theme_minimal() +
theme(
        axis.text.x = element_text(hjust = 1, size = TEXT_SIZE_AXIS_TEXT),
        axis.text.y = element_text(size = TEXT_SIZE_AXIS_TEXT),
        axis.title.x = element_text(size = TEXT_SIZE_AXIS_TITLE),
        axis.title.y = element_text(size = TEXT_SIZE_AXIS_TITLE),
        plot.title = element_text(size = TEXT_SIZE_TITLE),
        legend.text = element_text(size = TEXT_SIZE_LEGEND),
        legend.title = element_text(size = TEXT_SIZE_LEGEND),
        legend.position = "right" 
)+
    coord_flip()

# Combine diagnosis plots side by side
p_combined_diag <- p_diag_docs + p_diag_nondocs + 
    plot_layout(ncol = 2, widths = c(1, 1)) +
    plot_annotation(
        title = "Age and Sex Adjusted Incidence Rates across Diagnoses",
        subtitle = sprintf("Doctors vs General Population | Number of diagnoses tested: %d", nrow(dataset_diag))
    )

# # Save combined diagnosis plot
# output_file_diag <- file.path(plot_dir, paste0("diag_combined_", timestamp, ".png"))
# ggsave(output_file_diag, plot = p_combined_diag, width = 24, height = 12, dpi = 300, device = "png")


# Plot diagnoses: doctors vs non-doctors
p_diag_scatter <- ggplot(dataset_diag, aes(x = adj_IR_1k_nondocs, y = adj_IR_1k_docs, color = CHAPTER_NAME)) +
    geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "gray50", linewidth = 1) +
    geom_errorbar(aes(ymin = ci_lower_docs, ymax = ci_upper_docs), 
                  width = 0, alpha = 0.3) +
    geom_errorbarh(aes(xmin = ci_lower_nondocs, xmax = ci_upper_nondocs), 
                   height = 0, alpha = 0.3) +
    geom_point(size = POINT_SIZE_NOT_SIG, alpha = ALPHA_NOT_SIG) +
    geom_text_repel(data = bind_rows(
                        top_extreme_diag_docs %>% select(CODE, adj_IR_1k_nondocs, adj_IR_1k_docs, CHAPTER_NAME),
                        top_extreme_diag_nondocs %>% select(CODE, adj_IR_1k_nondocs, adj_IR_1k_docs, CHAPTER_NAME)
                    ) %>% distinct(CODE, .keep_all = TRUE), 
                    aes(label = CODE), 
                    size = 4, 
                    show.legend = FALSE,
                    max.overlaps = Inf,
                    min.segment.length = 0,
                    box.padding = 0.5,
                    point.padding = 0.3,
                    force = 2,
                    force_pull = 0.5) +
    scale_y_log10() +
    scale_x_log10() +
    scale_color_manual(values = cb_palette, name = "Chapter") +
    labs(
        title = "Age and Sex Adjusted Incidence Rates: Doctors vs General Population",
        subtitle = sprintf("Number of diagnoses tested: %d", nrow(dataset_diag)),
        x = "Adjusted IR (per 1,000 person-years) - General Population",
        y = "Adjusted IR (per 1,000 person-years) - Doctors"
    ) +
    theme_minimal() +
    theme(
        axis.text.x = element_text(size = TEXT_SIZE_AXIS_TEXT),
        axis.text.y = element_text(size = TEXT_SIZE_AXIS_TEXT),
        axis.title.x = element_text(size = TEXT_SIZE_AXIS_TITLE),
        axis.title.y = element_text(size = TEXT_SIZE_AXIS_TITLE),
        plot.title = element_text(size = TEXT_SIZE_TITLE),
        plot.subtitle = element_text(size = TEXT_SIZE_AXIS_TITLE),
        legend.text = element_text(size = TEXT_SIZE_LEGEND),
        legend.title = element_text(size = TEXT_SIZE_LEGEND),
        legend.position = "right"
    )


    # Plot incidence rates medication docs 
    p_medi_docs <- ggplot(dataset_medi %>% filter(TYPE == "medication"), aes(x = x_jittered, y = adj_IR_1k_docs, color = CHAPTER_NAME)) +
        geom_errorbar(aes(ymin = ci_lower_docs, ymax = ci_upper_docs), 
                      width = 0.1, alpha = 0.5) +
        geom_point() +
        geom_text_repel(data = top_extreme_medi_docs, aes(label = CODE), 
                            size = 4, 
                            show.legend = FALSE,
                            max.overlaps = Inf,
                            min.segment.length = 0,
                            box.padding = 0.5,
                            point.padding = 0.3,
                            force = 2,
                            force_pull = 0.5) +
        scale_y_log10() +
        scale_x_continuous(
            breaks = 1:length(levels(dataset_medi$CHAPTER_NAME)),
            labels = levels(dataset_medi$CHAPTER_NAME)
        ) +
        scale_color_manual(values = cb_palette, name = "Chapter", guide = "none") +
        labs(
            title = "Age and Sex Adjusted Incidence Rates across Medications (Doctors)",
            x = "ATC Code Chapter",
            y = "Adjusted Incidence Rate\n(per 1,000 person-years)"
        ) +
        theme_minimal() +
        theme(
            axis.text.x = element_text(hjust = 1, size = TEXT_SIZE_AXIS_TEXT),
            axis.text.y = element_text(size = TEXT_SIZE_AXIS_TEXT),
            axis.title.x = element_text(size = TEXT_SIZE_AXIS_TITLE),
            axis.title.y = element_text(size = TEXT_SIZE_AXIS_TITLE),
            plot.title = element_text(size = TEXT_SIZE_TITLE),
            legend.text = element_text(size = TEXT_SIZE_LEGEND),
            legend.title = element_text(size = TEXT_SIZE_LEGEND),
            legend.position = "right" 
        )+
        coord_flip()

    # Plot incidence rates medication nondocs
    p_medi_nondocs <- ggplot(dataset_medi %>% filter(TYPE == "medication"), aes(x = x_jittered, y = adj_IR_1k_nondocs, color = CHAPTER_NAME)) +
        geom_errorbar(aes(ymin = ci_lower_nondocs, ymax = ci_upper_nondocs), 
                      width = 0.1, alpha = 0.5) +
        geom_point() +
        geom_text_repel(data = top_extreme_medi_nondocs, aes(label = CODE), 
                            size = 4, 
                            show.legend = FALSE,
                            max.overlaps = Inf,
                            min.segment.length = 0,
                            box.padding = 0.5,
                            point.padding = 0.3,
                            force = 2,
                            force_pull = 0.5) +
        scale_y_log10() +
        scale_x_continuous(
            breaks = 1:length(levels(dataset_medi$CHAPTER_NAME)),
            labels = levels(dataset_medi$CHAPTER_NAME)
        ) +
        scale_color_manual(values = cb_palette, name = "Chapter", guide = "none") +
        labs(
            title = "Age and Sex Adjusted Incidence Rates across Medications (General Population w/o Doctors)",
            x = "ATC Code Chapter",
            y = "Adjusted Incidence Rate\n(per 1,000 person-years)"
        ) +
        theme_minimal() +
        theme(
            axis.text.x = element_text(hjust = 1, size = TEXT_SIZE_AXIS_TEXT),
            axis.text.y = element_text(size = TEXT_SIZE_AXIS_TEXT),
            axis.title.x = element_text(size = TEXT_SIZE_AXIS_TITLE),
            axis.title.y = element_text(size = TEXT_SIZE_AXIS_TITLE),
            plot.title = element_text(size = TEXT_SIZE_TITLE),
            legend.text = element_text(size = TEXT_SIZE_LEGEND),
            legend.title = element_text(size = TEXT_SIZE_LEGEND),
            legend.position = "right" 
        )+
        coord_flip()

    # Combine medication plots side by side
    p_combined_medi <- p_medi_docs + p_medi_nondocs + 
            plot_layout(ncol = 2, widths = c(1, 1)) +
            plot_annotation(
                    title = "Age and Sex Adjusted Incidence Rates across Medications",
                    subtitle = sprintf("Doctors vs General Population | Number of medications tested: %d", nrow(dataset_medi))
            )

    # Plot medications: doctors vs non-doctors
    p_medi_scatter <- ggplot(dataset_medi, aes(x = adj_IR_1k_nondocs, y = adj_IR_1k_docs, color = CHAPTER_NAME)) +
            geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "gray50", linewidth = 1) +
            geom_errorbar(aes(ymin = ci_lower_docs, ymax = ci_upper_docs), 
                          width = 0, alpha = 0.3) +
            geom_errorbarh(aes(xmin = ci_lower_nondocs, xmax = ci_upper_nondocs), 
                           height = 0, alpha = 0.3) +
            geom_point(size = POINT_SIZE_NOT_SIG, alpha = ALPHA_NOT_SIG) +
            geom_text_repel(data = bind_rows(
                                                    top_extreme_medi_docs %>% select(CODE, adj_IR_1k_nondocs, adj_IR_1k_docs, CHAPTER_NAME),
                                                    top_extreme_medi_nondocs %>% select(CODE, adj_IR_1k_nondocs, adj_IR_1k_docs, CHAPTER_NAME)
                                            ) %>% distinct(CODE, .keep_all = TRUE), 
                                            aes(label = CODE), 
                                            size = 4, 
                                            show.legend = FALSE,
                                            max.overlaps = Inf,
                                            min.segment.length = 0,
                                            box.padding = 0.5,
                                            point.padding = 0.3,
                                            force = 2,
                                            force_pull = 0.5) +
            scale_y_log10() +
            scale_x_log10() +
            scale_color_manual(values = cb_palette, name = "Chapter") +
            labs(
                    title = "Age and Sex Adjusted Incidence Rates: Doctors vs General Population",
                    subtitle = sprintf("Number of medications tested: %d", nrow(dataset_medi)),
                    x = "Adjusted IR (per 1,000 person-years) - General Population",
                    y = "Adjusted IR (per 1,000 person-years) - Doctors"
            ) +
            theme_minimal() +
            theme(
                    axis.text.x = element_text(size = TEXT_SIZE_AXIS_TEXT),
                    axis.text.y = element_text(size = TEXT_SIZE_AXIS_TEXT),
                    axis.title.x = element_text(size = TEXT_SIZE_AXIS_TITLE),
                    axis.title.y = element_text(size = TEXT_SIZE_AXIS_TITLE),
                    plot.title = element_text(size = TEXT_SIZE_TITLE),
                    plot.subtitle = element_text(size = TEXT_SIZE_AXIS_TITLE),
                    legend.text = element_text(size = TEXT_SIZE_LEGEND),
                    legend.title = element_text(size = TEXT_SIZE_LEGEND),
                    legend.position = "right"
            )

# Compare IRs between doctors and non-doctors
# calculate p-values for that

compare_IRs_crude <- function(
  data,
  events_A, py_A,
  events_B, py_B,
  mult = 1000  # e.g. 1000 for "per 1,000 PY"
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
diag_compare <- compare_IRs_crude(
  data = dataset_diag,
  events_A = events_docs,
  py_A     = person_years_docs,
  events_B = events_nondocs,
  py_B     = person_years_nondocs,
  mult     = 1000   # rates per 1,000 PY
)

# for medications
medi_compare <- compare_IRs_crude(
  data = dataset_medi,
  events_A = events_docs,
  py_A     = person_years_docs,
  events_B = events_nondocs,
  py_B     = person_years_nondocs,
  mult     = 1000   # rates per 1,000 PY
)

# Plot diag_compare
p_diag_compare <- ggplot(diag_compare %>% filter(TYPE == "diagnosis") %>%
                         mutate(significant = p_IRR <= 0.05), 
                         aes(x = x_jittered, y = IRR, color = CHAPTER_NAME, shape = significant)) +
    geom_errorbar(aes(ymin = IRR_lo, ymax = IRR_hi), 
                                width = 0.1, alpha = 0.5) +
    geom_point(size = 3, alpha = 0.7) +
    geom_text_repel(data = top_extreme_diag_docs, aes(label = CODE), 
                        size = 4, 
                        show.legend = FALSE,
                        max.overlaps = Inf,
                        min.segment.length = 0,
                        box.padding = 0.5,
                        point.padding = 0.3,
                        force = 2,
                        force_pull = 0.5) +
    scale_x_continuous(
        breaks = 1:length(levels(diag_compare$CHAPTER_NAME)),
        labels = levels(diag_compare$CHAPTER_NAME)
    ) +
    scale_color_manual(values = cb_palette, name = "Chapter", guide = "none") +
    scale_shape_manual(values = c("TRUE" = 16, "FALSE" = 1), 
                       name = "Significance", 
                       labels = c("TRUE" = "p ≤ 0.05", "FALSE" = "p > 0.05")) +
    labs(
        title = "Incidence Rate Ratios across Diagnoses (Doctors vs General Population)",
        x = "ICD-10 Code Chapter",
        y = "Incidence Rate Ratio"
    ) +
    theme_minimal() +
    theme(
        axis.text.x = element_text(hjust = 1, size = TEXT_SIZE_AXIS_TEXT),
        axis.text.y = element_text(size = TEXT_SIZE_AXIS_TEXT),
        axis.title.x = element_text(size = TEXT_SIZE_AXIS_TITLE),
        axis.title.y = element_text(size = TEXT_SIZE_AXIS_TITLE),
        plot.title = element_text(size = TEXT_SIZE_TITLE),
        legend.text = element_text(size = TEXT_SIZE_LEGEND),
        legend.title = element_text(size = TEXT_SIZE_LEGEND),
        legend.position = "right" 
    )+
    coord_flip()


#### From here the final figures: ####

# Identify top 2 extreme adjIR values by Chapter
top_extreme_diag_compare_docs <- diag_compare %>%
    filter(TYPE == "diagnosis") %>%
    group_by(CHAPTER_NAME) %>%
    slice_max(order_by = abs(adj_IR_1k_docs), n = 2) %>%
    ungroup()

top_extreme_medi_compare_docs <- medi_compare %>%
    filter(TYPE == "medication") %>%
    group_by(CHAPTER_NAME) %>%
    slice_max(order_by = abs(adj_IR_1k_docs), n = 2) %>%
    ungroup()

top_extreme_medi_compare_IRR <- medi_compare %>%
    filter(TYPE == "medication") %>%
    arrange(desc(IRR)) %>%
    slice(c(1:5, (n()-4):n())) %>%
    ungroup()


# Plot Figure 2A:
# Plot incidence rates diagnoses docs 
p_fig_2A <- ggplot(diag_compare %>% filter(TYPE == "diagnosis"), aes(x = x_jittered, y = adj_IR_1k_docs, color = CHAPTER_NAME)) +
    geom_errorbar(aes(ymin = ci_lower_docs, ymax = ci_upper_docs), 
                                width = 0.1, alpha = 0.5) +
    geom_point() +
    geom_text_repel(data = top_extreme_diag_compare_docs, aes(label = CODE), 
                        size = 4, 
                        show.legend = FALSE,
                        max.overlaps = Inf,
                        min.segment.length = 0,
                        box.padding = 0.5,
                        point.padding = 0.3,
                        force = 2,
                        force_pull = 0.5) +
    scale_x_continuous(
        breaks = 1:length(levels(dataset_diag$CHAPTER_NAME)),
        labels = levels(dataset_diag$CHAPTER_NAME)
    ) +
    # scale_y_log10() +
    scale_color_manual(values = cb_palette, name = "Chapter", guide = "none") +
    labs(
        title = "Age and Sex Adjusted Incidence Rates across Diagnoses (Doctors)",
        x = "ICD-10 Code Chapter",
        y = "Adjusted Incidence Rate\n(per 1,000 person-years, log scale)"
    ) +
    theme_minimal() +
    theme(
        axis.text.x = element_text(hjust = 1, size = TEXT_SIZE_AXIS_TEXT),
        axis.text.y = element_text(size = TEXT_SIZE_AXIS_TEXT),
        axis.title.x = element_text(size = TEXT_SIZE_AXIS_TITLE),
        axis.title.y = element_text(size = TEXT_SIZE_AXIS_TITLE),
        plot.title = element_text(size = TEXT_SIZE_TITLE),
        legend.text = element_text(size = TEXT_SIZE_LEGEND),
        legend.title = element_text(size = TEXT_SIZE_LEGEND),
        legend.position = "right" 
    )+
    coord_flip()

p_fig_2A

# Plot Figure 2B:
# Plot incidence rates medication docs
p_fig_2B <- ggplot(medi_compare %>% filter(TYPE == "medication"), aes(x = x_jittered, y = adj_IR_1k_docs, color = CHAPTER_NAME)) +
    geom_errorbar(aes(ymin = ci_lower_docs, ymax = ci_upper_docs), 
                                width = 0.1, alpha = 0.5) +
    geom_point() +
    geom_text_repel(data = top_extreme_medi_compare_docs, aes(label = CODE), 
                        size = 4, 
                        show.legend = FALSE,
                        max.overlaps = Inf,
                        min.segment.length = 0,
                        box.padding = 0.5,
                        point.padding = 0.3,
                        force = 2,
                        force_pull = 0.5) +
    scale_x_continuous(
        breaks = 1:length(levels(medi_compare$CHAPTER_NAME)),
        labels = levels(medi_compare$CHAPTER_NAME)
    ) +
    # scale_y_log10() +
    scale_color_manual(values = cb_palette, name = "Chapter", guide = "none") +
    labs(
            title = "Age and Sex Adjusted Incidence Rates across Medications (Doctors)",
            x = "ATC Code Chapter",
            y = "Adjusted Incidence Rate\n(per 1,000 person-years)"
        ) +
    theme_minimal() +
    theme(
        axis.text.x = element_text(hjust = 1, size = TEXT_SIZE_AXIS_TEXT),
        axis.text.y = element_text(size = TEXT_SIZE_AXIS_TEXT),
        axis.title.x = element_text(size = TEXT_SIZE_AXIS_TITLE),
        axis.title.y = element_text(size = TEXT_SIZE_AXIS_TITLE),
        plot.title = element_text(size = TEXT_SIZE_TITLE),
        legend.text = element_text(size = TEXT_SIZE_LEGEND),
        legend.title = element_text(size = TEXT_SIZE_LEGEND),
        legend.position = "right" 
    )+
    coord_flip()

p_fig_2B

# Plot Figure 2C:
# Plot medications: doctors vs non-doctors
# Annotate top 10 extreme IRR values
p_fig_2C <- ggplot(medi_compare, aes(x = adj_IR_1k_nondocs, y = adj_IR_1k_docs, color = CHAPTER_NAME)) +
            geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "gray50", linewidth = 1) +
            geom_errorbar(aes(ymin = ci_lower_docs, ymax = ci_upper_docs), 
                          width = 0, alpha = 0.3) +
            geom_errorbarh(aes(xmin = ci_lower_nondocs, xmax = ci_upper_nondocs), 
                           height = 0, alpha = 0.3) +
            geom_point(size = POINT_SIZE_NOT_SIG, alpha = ALPHA_NOT_SIG) +
            geom_text_repel(data = top_extreme_medi_compare_IRR %>% 
                                            distinct(CODE, .keep_all = TRUE), 
                                            aes(label = CODE), 
                                            size = 4, 
                                            show.legend = FALSE,
                                            max.overlaps = Inf,
                                            min.segment.length = 0,
                                            box.padding = 0.5,
                                            point.padding = 0.3,
                                            force = 2,
                                            force_pull = 0.5) +
            scale_y_log10() +
            scale_x_log10() +
            scale_color_manual(values = cb_palette, name = "Chapter") +
            labs(
                    title = "Age and Sex Adjusted Incidence Rates: Doctors vs General Population",
                    subtitle = sprintf("Number of medications tested: %d", nrow(medi_compare)),
                    x = "Adjusted IR (per 1,000 person-years) - General Population",
                    y = "Adjusted IR (per 1,000 person-years) - Doctors"
            ) +
            theme_minimal() +
            theme(
                    axis.text.x = element_text(size = TEXT_SIZE_AXIS_TEXT),
                    axis.text.y = element_text(size = TEXT_SIZE_AXIS_TEXT),
                    axis.title.x = element_text(size = TEXT_SIZE_AXIS_TITLE),
                    axis.title.y = element_text(size = TEXT_SIZE_AXIS_TITLE),
                    plot.title = element_text(size = TEXT_SIZE_TITLE),
                    plot.subtitle = element_text(size = TEXT_SIZE_AXIS_TITLE),
                    legend.text = element_text(size = TEXT_SIZE_LEGEND),
                    legend.title = element_text(size = TEXT_SIZE_LEGEND),
                    legend.position = "right"
            )

p_fig_2C


# Save what needs to be saved 
# i have the plot dir already and the timstamp variable defined at the top
# save with pdf() and dev.off() to get vector graphics

# Save Figure 2A
output_file_fig_2A <- file.path(plot_dir, paste0("Figure_2A_diag_IRs_docs_", timestamp, ".pdf"))
pdf(output_file_fig_2A, width = 11, height = 8.5)
print(p_fig_2A)
dev.off()

# Save Figure 2B
output_file_fig_2B <- file.path(plot_dir, paste0("Figure_2B_medi_IRs_docs_", timestamp, ".pdf"))
pdf(output_file_fig_2B, width = 11, height = 8.5)
print(p_fig_2B) 
dev.off()

# Save Figure 2C
output_file_fig_2C <- file.path(plot_dir, paste0("Figure_2C_medi_IRs_scatter_", timestamp, ".pdf"))
pdf(output_file_fig_2C, width = 11, height = 8.5)
print(p_fig_2C)
dev.off()

# Save diag_compare data 
output_file_diag_compare <- file.path(plot_dir, paste0("diag_compare_", timestamp, ".csv"))
fwrite(diag_compare, output_file_diag_compare) 

# Save medi_compare data 
output_file_medi_compare <- file.path(plot_dir, paste0("medi_compare_", timestamp, ".csv"))
fwrite(medi_compare, output_file_medi_compare)