# Libraries
library(ggplot2)
library(dplyr)
library(readr)
library(tidyr)
library(scales)

# Global Variables
dataset_file = '/media/volume/Projects/DSGELabProject1/DiD_Experiments/Version1_Highthroughput/Results/Results_20250725.csv'
expected_codes_file = '/media/volume/Projects/DSGELabProject1/DiD_Experiments/Version1_Highthroughput/ProcessedOutcomes_20250725/outcome_codes.txt'
output_file = "/media/volume/Projects/DSGELabProject1/DiD_Experiments/Version1_Highthroughput/Results/highthroughput_results_20250725_heatmap.png"
output_file_clustered = "/media/volume/Projects/DSGELabProject1/DiD_Experiments/Version1_Highthroughput/Results/highthroughput_results_20250725_heatmap_clustered.png"
width = 12
height = 10

# Functions
clean_event_code <- function(event_code) {
    ifelse(startsWith(event_code, "Purch_"), substr(event_code, 7, nchar(event_code)),
        ifelse(startsWith(event_code, "Diag_"), substr(event_code, 6, nchar(event_code)), event_code))
}

create_significance_label <- function(p_value, bonferroni_threshold) {
    case_when(
        p_value < bonferroni_threshold ~ "⊗",  # Circle + X for Bonferroni corrected
        p_value < 0.05 ~ "○",                  # Empty circle for regular significance
        TRUE ~ ""                              # No marker for non-significant
    )
}

# Main to create the heatmap
  
# Load the dataset
cat("Loading dataset...\n")
dataset <- read_csv(dataset_file, show_col_types = FALSE)

# Check required columns
required_cols <- c("EVENT_CODE", "OUTCOME_CODE", "EFFECT_SIZE", "P_VALUE")
missing_cols <- setdiff(required_cols, names(dataset))
if (length(missing_cols) > 0) {
stop(paste("Missing required columns:", paste(missing_cols, collapse = ", ")))
}

# Load expected codes
cat("Loading expected codes...\n")
expected_codes <- readLines(expected_codes_file)
cat(paste("Expected codes loaded:", length(expected_codes), "codes\n"))

# Clean the dataset
cat("Processing dataset...\n")
dataset_clean <- dataset %>%
    mutate(
        event_code_clean = clean_event_code(EVENT_CODE),
        effect_size = as.numeric(EFFECT_SIZE),
        p_value = as.numeric(P_VALUE)
    ) %>%
    filter(!is.na(effect_size) & !is.na(p_value)) %>%
    select(event_code = event_code_clean, outcome_code = OUTCOME_CODE, effect_size, p_value)

cat(paste("Dataset processed:", nrow(dataset_clean), "records\n"))

# Get all unique codes from data
data_codes <- unique(c(dataset_clean$event_code, dataset_clean$outcome_code))
cat(paste("Unique codes found in data:", length(data_codes), "\n"))

# Compare with expected codes
missing_in_data <- setdiff(expected_codes, data_codes)
extra_in_data <- setdiff(data_codes, expected_codes)

cat(paste("Expected codes:", length(expected_codes), "\n"))
cat(paste("N of Codes missing in data:", length(missing_in_data), "\n"))
cat(paste("N of Extra codes in data (not expected):", length(extra_in_data), "\n"))

# Combine with expected codes and sort
all_codes <- sort(unique(c(expected_codes, data_codes)))

# Calculate Bonferroni correction threshold
total_pairs <- length(all_codes)^2
total_estimated_pairs <- sum(!is.na(dataset_clean$effect_size))
bonferroni_threshold <- 0.05 / total_estimated_pairs # using only estimated pairs
cat(paste("Bonferroni threshold:", format(bonferroni_threshold, scientific = TRUE), "\n"))

# Create complete grid of all possible combinations
complete_grid <- expand_grid(
    event_code = all_codes,
    outcome_code = all_codes
)

# Merge with actual data (including reverse pairs)
heatmap_data <- complete_grid %>%
    left_join(dataset_clean, by = c("event_code", "outcome_code")) %>%
    # If no direct match, try reverse match
    left_join(
        dataset_clean %>% 
        select(event_code = outcome_code, outcome_code = event_code, effect_size_rev = effect_size, p_value_rev = p_value),
        by = c("event_code", "outcome_code")
    ) %>%
    # Use reverse values if direct values are missing
    mutate(effect_size = coalesce(effect_size, effect_size_rev),p_value = coalesce(p_value, p_value_rev)) %>%
    select(-effect_size_rev, -p_value_rev) %>%
    # Create significance labels
    mutate(
        significance = create_significance_label(p_value, bonferroni_threshold),
        # Convert codes to factors to control ordering
        event_code = factor(event_code, levels = all_codes),
        outcome_code = factor(outcome_code, levels = rev(all_codes))  # Reverse for proper matrix orientation
    )

# Calculate effect size range for color scale
effect_range <- range(heatmap_data$effect_size, na.rm = TRUE)
max_abs_effect <- max(abs(effect_range), na.rm = TRUE)

cat(paste("Effect size range:", round(effect_range[1], 4), "to", round(effect_range[2], 4), "\n"))

# Create the heatmap
cat("Creating heatmap...\n")
p <- ggplot(heatmap_data, aes(x = event_code, y = outcome_code)) +
    geom_tile(aes(fill = effect_size), color = "white", size = 0.5) +
    geom_text(aes(label = significance), 
                size = 3, color = "black", fontface = "bold") +
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
        axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
        axis.text.y = element_text(size = 8),
        axis.title = element_text(size = 12, face = "bold"),
        plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
        plot.subtitle = element_text(size = 10, hjust = 0.5),
        legend.title = element_text(size = 10, face = "bold"),
        legend.text = element_text(size = 9),
        panel.grid = element_blank(),
        plot.margin = margin(20, 20, 20, 20)
    ) +
    labs(
        title = "Effect Size Heatmap with Statistical Significance",
        subtitle = paste0(
        "○ p < 0.05  |  ⊗ Bonferroni corrected (p < ", 
        format(bonferroni_threshold, scientific = TRUE, digits = 3), 
        ")  |  Matrix size: ", length(all_codes), "×", length(all_codes)
        ),
        x = "Event Code",
        y = "Outcome Code"
    ) +
    coord_fixed()  # Ensure square cells

# Save the plot
cat(paste("Saving plot to", output_file, "...\n"))
ggsave(output_file, plot = p, width = width, height = height, dpi = 300, bg = "white")

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


# Create clustered heatmap
cat("\n=== CREATING CLUSTERED HEATMAP ===\n")

# Create matrix for clustering
cat("Preparing data matrix for clustering...\n")
effect_wide <- heatmap_data %>%
    select(event_code, outcome_code, effect_size) %>%
    pivot_wider(names_from = outcome_code, values_from = effect_size, values_fill = 0)

# Convert to matrix with row names
effect_matrix <- as.matrix(effect_wide[, -1])  # Remove first column (event_code)
rownames(effect_matrix) <- effect_wide$event_code

# Handle missing values for clustering
# Replace NA with 0 (assuming no effect when data is missing)
effect_matrix[is.na(effect_matrix)] <- 0
cat(paste("Matrix dimensions for clustering:", nrow(effect_matrix), "x", ncol(effect_matrix), "\n"))

# Perform hierarchical clustering
cat("Performing hierarchical clustering...\n")

# Cluster rows (event codes)
if (nrow(effect_matrix) > 1) {
    row_dist <- dist(effect_matrix, method = "euclidean")
    row_clust <- hclust(row_dist, method = "ward.D2")
    row_order <- rownames(effect_matrix)[row_clust$order]
} else {
    row_order <- rownames(effect_matrix)
}

# Cluster columns (outcome codes)
if (ncol(effect_matrix) > 1) {
    col_dist <- dist(t(effect_matrix), method = "euclidean")
    col_clust <- hclust(col_dist, method = "ward.D2")
    col_order <- colnames(effect_matrix)[col_clust$order]
} else {
    col_order <- colnames(effect_matrix)
}

# Create clustered heatmap data
heatmap_data_clustered <- heatmap_data %>%
    mutate(
        # Reorder factors based on clustering
        event_code = factor(event_code, levels = row_order),
        outcome_code = factor(outcome_code, levels = rev(col_order))  # Reverse for proper matrix orientation
    )

# Create the clustered heatmap
cat("Creating clustered heatmap...\n")
p_clustered <- ggplot(heatmap_data_clustered, aes(x = event_code, y = outcome_code)) +
    geom_tile(aes(fill = effect_size), color = "white", size = 0.5) +
    geom_text(aes(label = significance), 
                size = 3, color = "black", fontface = "bold") +
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
        axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
        axis.text.y = element_text(size = 8),
        axis.title = element_text(size = 12, face = "bold"),
        plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
        plot.subtitle = element_text(size = 10, hjust = 0.5),
        legend.title = element_text(size = 10, face = "bold"),
        legend.text = element_text(size = 9),
        panel.grid = element_blank(),
        plot.margin = margin(20, 20, 20, 20)
    ) +
    labs(
        title = "Hierarchical Clustered Effect Size Heatmap",
        subtitle = paste0(
        "○ p < 0.05  |  ⊗ Bonferroni corrected (p < ", 
        format(bonferroni_threshold, scientific = TRUE, digits = 3), 
        ")  |  Clustered using Ward's method with Euclidean distance"
        ),
        x = "Event Code (Clustered Order)",
        y = "Outcome Code (Clustered Order)"
    ) +
    coord_fixed()  # Ensure square cells

# Save the clustered plot
cat(paste("Saving clustered plot to", output_file_clustered, "...\n"))
ggsave(output_file_clustered, plot = p_clustered, width = width, height = height, dpi = 300, bg = "white")

# Print clustering summary
cat("\n=== CLUSTERING SUMMARY ===\n")
cat("Clustering method: Ward's method with Euclidean distance\n")
cat("Missing values replaced with 0 for clustering\n")
cat(paste("Event clustering order (first 5):", paste(head(row_order, 5), collapse = ", "), "\n"))
cat(paste("Event clustering order (last 5):", paste(tail(row_order, 5), collapse = ", "), "\n"))
cat(paste("Outcome clustering order (first 5):", paste(head(col_order, 5), collapse = ", "), "\n"))
cat(paste("Outcome clustering order (last 5):", paste(tail(col_order, 5), collapse = ", "), "\n"))
