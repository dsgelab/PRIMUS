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
output_file <- sprintf('/media/volume/Projects/DSGELabProject1/DiD_Experiments/Version2_Highthroughput/Results/Heatmap_%s_%s.png', TYPE2, DATE)

# Functions
create_significance_label <- function(p_value, bonferroni_threshold) {
    case_when(
        p_value < bonferroni_threshold ~ "⊗",  # Circle + X for Bonferroni corrected
        p_value < 0.05 ~ "○",                  # Empty circle for regular significance
        TRUE ~ ""                              # No marker for non-significant
    )
}

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

# Convert matrix to long format for plotting
heatmap_data <- dataset_clean %>%
    mutate(
        event_code = as.character(event_code),
        outcome_code = as.character(outcome_code)
    ) %>%
    complete(event_code = event_codes, outcome_code = outcome_codes) %>%
    mutate(significance = create_significance_label(p_value, bonferroni_threshold))

# Calculate effect size range for color scale (no rounding)
effect_range <- range(heatmap_data$effect_size, na.rm = TRUE)
max_abs_effect <- max(abs(effect_range), na.rm = TRUE)
cat(paste("Effect size range:", effect_range[1], "to", effect_range[2], "\n"))

# Create the heatmap
cat("Creating heatmap...\n")
p <- ggplot(heatmap_data, aes(x = event_code, y = outcome_code)) +
    geom_tile(aes(fill = effect_size), color = "white", size = 0.5) +
    geom_text(aes(label = significance), size = 3, color = "black", fontface = "bold") +
    scale_fill_gradient2(
        low = "blue", 
        mid = "white", 
        high = "red",
        midpoint = 0,
        limits = c(-max_abs_effect, max_abs_effect),
        name = "Effect\nSize",
        na.value = "grey90"
    ) +
    theme_minimal() +
    theme(
        axis.text.x = element_text(angle = 90, hjust = 1, size = 7),
        axis.text.y = element_text(size = 7),
        axis.title = element_text(size = 12, face = "bold"),
        plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
        plot.subtitle = element_text(size = 10, hjust = 0.5),
        legend.title = element_text(size = 10, face = "bold"),
        legend.text = element_text(size = 9),
        panel.grid = element_blank(),
        plot.margin = margin(20, 20, 20, 20)
    ) +
    labs(
        title = "Heatmap of Effect Sizes from Highthroughput Analysis",
        subtitle = paste0(
        "○ p < 0.05  |  ⊗ Bonferroni corrected (p < ", format(bonferroni_threshold, scientific = TRUE, digits = 3), ")  |  Matrix size: ", length(event_codes), "×", length(outcome_codes)
        ),
        x = if (TYPE2 == "diag") "Event Code (ICD10 - 3 chars)" else if (TYPE2 == "Purch") "Event Code (ATC - 5 chars)" else "Event Code",
        y = "Outcome Code"
    ) +
    coord_fixed()

# Save the plot
cat(paste("Saving plot to", output_file, "...\n"))
ggsave(output_file, plot = p, width = 16, height = 10, dpi = 300, bg = "white")

# Print summary statistics
cat("\n=== SUMMARY STATISTICS ===\n")
significance_summary <- heatmap_data %>%
    filter(!is.na(p_value)) %>%
    summarise(
        total_with_pvalue = n(),
        significant_05 = sum(p_value < 0.05),
        bonferroni_significant = sum(p_value < bonferroni_threshold),
        .groups = 'drop'
    )
cat(paste("Significant at p < 0.05:", significance_summary$significant_05, "\n"))
cat(paste("Bonferroni significant:", significance_summary$bonferroni_significant, "\n"))

# List Bonferroni significant pairs
bonferroni_pairs <- heatmap_data %>%
    filter(!is.na(p_value), p_value < bonferroni_threshold) %>%
    select(event_code, outcome_code, effect_size, p_value)

if (nrow(bonferroni_pairs) > 0) {
    cat("\nBonferroni significant pairs (event_code, outcome_code, effect_size, p_value):\n")
    print(bonferroni_pairs, n = Inf)
} else {
    cat("\nNo Bonferroni significant pairs found.\n")
}
