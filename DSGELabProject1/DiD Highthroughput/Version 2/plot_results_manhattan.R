# Libraries
library(ggplot2)
library(dplyr)
library(readr)
library(tidyr)
library(scales)

# Global Variables
DATE = "20250921"
TYPE = "ICD" # "ICD" or "ATC"
TYPE2 = "Diag" # "Diag" or "Purch" 
dataset_file <- sprintf('/media/volume/Projects/DSGELabProject1/DiD_Experiments/Version2_Highthroughput/Results/Results_%s_%s.csv', TYPE, DATE)
output_file <- sprintf('/media/volume/Projects/DSGELabProject1/DiD_Experiments/Version2_Highthroughput/Results/Manhattan_%s_%s.png', TYPE2, DATE)

# Main 
dataset <- read_csv(dataset_file, show_col_types = FALSE)

# Check required columns
required_cols <- c("EVENT_CODE", "OUTCOME_CODE", "EFFECT_SIZE", "P_VALUE")
missing_cols <- setdiff(required_cols, names(dataset))
if (length(missing_cols) > 0) {
stop(paste("Missing required columns:", paste(missing_cols, collapse = ", ")))
}

# Clean the dataset
cat("Processing dataset...\n")
dataset_clean <- dataset %>%
    mutate(
        effect_size = as.numeric(EFFECT_SIZE),
        p_value = as.numeric(P_VALUE)
    ) %>%
    filter(!is.na(effect_size) & !is.na(p_value)) %>%
    select(event_code = EVENT_CODE, outcome_code = OUTCOME_CODE, effect_size, p_value)

# Get all unique codes from data
event_codes <- unique(dataset_clean$event_code)
outcome_codes <- unique(dataset_clean$outcome_code)
cat(paste("Unique event codes:", length(event_codes), "\n"))
cat(paste("Unique outcome codes:", length(outcome_codes), "\n"))

# Calculate expected combinations and available combinations
expected_cells <- length(event_codes) * length(outcome_codes)
present_cells <- nrow(dataset_clean)
cat(paste("Matrix size (event x outcome):", expected_cells, "\n"))
cat(paste("Available matrix cells values:", present_cells, "\n"))

# Calculate Bonferroni correction threshold
bonferroni_threshold <- 0.05 / present_cells  # using number of present (estimated) cells
cat(paste("Bonferroni threshold:", format(bonferroni_threshold, scientific = TRUE), "\n"))

# Calculate FDR correction threshold using Benjamini-Hochberg method
fdr_adjusted_p <- p.adjust(dataset_clean$p_value, method = "fdr")
fdr_threshold <- max(dataset_clean$p_value[fdr_adjusted_p <= 0.05], na.rm = TRUE)
if (is.infinite(fdr_threshold) || is.na(fdr_threshold)) {
    fdr_threshold <- 0  # No significant results after FDR correction
}
cat(paste("FDR threshold (BH method):", format(fdr_threshold, scientific = TRUE), "\n"))

# Prepare data for Manhattan plot in one step
manhattan_data <- dataset_clean %>%
    mutate(
        event_code = as.character(event_code),
        outcome_code = as.character(outcome_code),
        fdr_adjusted_p = p.adjust(p_value, method = "fdr")
    ) %>%
    complete(event_code = event_codes, outcome_code = outcome_codes) %>%
    mutate(
        log10_p = -log10(p_value),
        event_group = substr(gsub(paste0(TYPE2, "_"), "", event_code), 1, 1),
        bonferroni_label = ifelse(!is.na(p_value) & p_value < bonferroni_threshold, outcome_code, NA),
        fdr_label = ifelse(!is.na(p_value) & p_value < fdr_threshold, outcome_code, NA)
    )

# Calculate effect size range for color scale (no rounding)
effect_range <- range(manhattan_data$effect_size, na.rm = TRUE)
max_abs_effect <- max(abs(effect_range), na.rm = TRUE)
cat(paste("Effect size range:", effect_range[1], "to", effect_range[2], "\n"))

# Order event codes for x-axis
manhattan_data$event_code <- factor(manhattan_data$event_code, levels = unique(manhattan_data$event_code))

# Create Manhattan plot
cat("Creating Manhattan plot...\n")
n_groups <- length(unique(manhattan_data$event_group))
palette_colors <- scales::hue_pal()(n_groups)
event_group_label <- ifelse(TYPE == "Diag", "ICD 10 Diagnosis chapter", "ATC Medication chapter")

p <- ggplot(manhattan_data, aes(x = event_code, y = log10_p, color = event_group)) +
    geom_point(size = 2, alpha = 0.8, na.rm = TRUE) +
    geom_hline(yintercept = -log10(0.05), linetype = "dashed", color = "grey40", size = 0.7) +
    geom_hline(yintercept = -log10(bonferroni_threshold), linetype = "dotted", color = "red", size = 0.8) +
    geom_hline(yintercept = -log10(fdr_threshold), linetype = "solid", color = "blue", size = 0.8) +
    ggrepel::geom_text_repel(
        data = subset(manhattan_data, !is.na(fdr_label)),
        aes(label = paste(event_code, "-->", outcome_code)),
        size = 3, fontface = "bold", color = "black", max.overlaps = 20, na.rm = TRUE
    ) +
    scale_color_manual(values = palette_colors, name = event_group_label) +
    theme_minimal() +
    theme(
        axis.text.x = element_blank(), 
        axis.ticks.x = element_blank(), 
        axis.title.x = element_text(size = 12, face = "bold"),
        axis.text.y = element_text(size = 9),
        axis.title.y = element_text(size = 12, face = "bold"),
        plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
        plot.subtitle = element_text(size = 10, hjust = 0.5),
        legend.title = element_text(size = 10, face = "bold"),
        legend.text = element_text(size = 9),
        panel.grid = element_blank(),
        plot.margin = margin(20, 20, 20, 20)
    ) +
    labs(
        title = "Manhattan Plot of -log10(p-value) by Event Code",
        subtitle = paste0("Dashed: p < 0.05  |  Dotted: Bonferroni (p < ", format(bonferroni_threshold, scientific = TRUE, digits = 3), ")  |  Solid: FDR (p < ", format(fdr_threshold, scientific = TRUE, digits = 3), ")  |  Matrix size: ", length(event_codes), "×", length(outcome_codes)
        ),
        x = if (TYPE2 == "diag") "Event Code (ICD10 - 3 chars)" else if (TYPE2 == "Purch") "Event Code (ATC - 5 chars)" else "Event Code",
        y = expression(-log[10](p))
    )

# Save the plot
cat(paste("Saving plot to", output_file, "...\n"))
ggsave(output_file, plot = p, width = 16, height = 10, dpi = 300, bg = "white")

# Print summary statistics
cat("\n=== SUMMARY STATISTICS ===\n")
significance_summary <- manhattan_data %>%
    filter(!is.na(p_value)) %>%
    summarise(
        total_with_pvalue = n(),
        significant_05 = sum(p_value < 0.05),
        bonferroni_significant = sum(p_value < bonferroni_threshold),
        fdr_significant = sum(p_value < fdr_threshold),
        .groups = 'drop'
    )
cat(paste("Significant at p < 0.05:", significance_summary$significant_05, "\n"))
cat(paste("Bonferroni significant:", significance_summary$bonferroni_significant, "\n"))
cat(paste("FDR significant:", significance_summary$fdr_significant, "\n"))

# List Bonferroni significant pairs
bonferroni_pairs <- manhattan_data %>%
    filter(!is.na(p_value), p_value < bonferroni_threshold) %>%
    select(event_code, outcome_code, effect_size, p_value)

if (nrow(bonferroni_pairs) > 0) {
    cat("\nBonferroni significant pairs (event_code, outcome_code, effect_size, p_value):\n")
    print(bonferroni_pairs, n = Inf)
} else {
    cat("\nNo Bonferroni significant pairs found.\n")
}

# List FDR significant pairs
fdr_pairs <- manhattan_data %>%
    filter(!is.na(p_value), p_value < fdr_threshold) %>%
    select(event_code, outcome_code, effect_size, p_value, fdr_adjusted_p)

if (nrow(fdr_pairs) > 0) {
    cat("\nFDR significant pairs (event_code, outcome_code, effect_size, p_value, fdr_adjusted_p):\n")
    print(fdr_pairs, n = Inf)
} else {
    cat("\nNo FDR significant pairs found.\n")
}