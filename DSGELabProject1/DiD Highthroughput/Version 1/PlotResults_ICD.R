# Libraries
library(ggplot2)
library(dplyr)
library(readr)
library(tidyr)
library(scales)
library(plotly)

# Global Variables
dataset_file = '/media/volume/Projects/DSGELabProject1/DiD_Experiments/Version1_Highthroughput/Results/Results_20250728.csv'
event_codes_file = '/media/volume/Projects/DSGELabProject1/DiD_Experiments/Version1_Highthroughput/ProcessedEvents_20250728/event_codes.txt'
outcome_codes_file = '/media/volume/Projects/DSGELabProject1/DiD_Experiments/Version1_Highthroughput/ProcessedOutcomes_20250728/outcome_codes.txt'
output_file = "/media/volume/Projects/DSGELabProject1/DiD_Experiments/Version1_Highthroughput/Results/manhattan_plot_20250728.png"
output_file_interactive = "/media/volume/Projects/DSGELabProject1/DiD_Experiments/Version1_Highthroughput/Results/manhattan_plot_20250728.html"
width = 16
height = 10

# Functions
clean_event_code <- function(event_code) {
    ifelse(startsWith(event_code, "Purch_"), substr(event_code, 7, nchar(event_code)),
        ifelse(startsWith(event_code, "Diag_"), substr(event_code, 6, nchar(event_code)), event_code))
}

# Load the dataset
cat("Loading dataset...\n")
dataset <- read_csv(dataset_file, show_col_types = FALSE)

# Check required columns
required_cols <- c("EVENT_CODE", "OUTCOME_CODE", "EFFECT_SIZE", "P_VALUE")
missing_cols <- setdiff(required_cols, names(dataset))
if (length(missing_cols) > 0) {
    stop(paste("Missing required columns:", paste(missing_cols, collapse = ", ")))
}

# Load event and outcome codes
cat("Loading event codes...\n")
event_codes <- readLines(event_codes_file)
cat(paste("Event codes loaded:", length(event_codes), "codes\n"))

cat("Loading outcome codes...\n")
outcome_codes <- readLines(outcome_codes_file)
cat(paste("Outcome codes loaded:", length(outcome_codes), "codes\n"))

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

# Create complete ExO grid of all possible combinations
cat("Creating Event x Outcome grid...\n")
complete_grid <- expand_grid(
    event_code = clean_event_code(event_codes),
    outcome_code = outcome_codes
)

# Merge with actual data (including reverse pairs)
manhattan_data <- complete_grid %>%
    left_join(dataset_clean, by = c("event_code", "outcome_code")) %>%
    # If no direct match, try reverse match
    left_join(
        dataset_clean %>% 
        select(event_code = outcome_code, outcome_code = event_code, effect_size_rev = effect_size, p_value_rev = p_value),
        by = c("event_code", "outcome_code")
    ) %>%
    # Use reverse values if direct values are missing
    mutate(
        effect_size = coalesce(effect_size, effect_size_rev),
        p_value = coalesce(p_value, p_value_rev)
    ) %>%
    select(-effect_size_rev, -p_value_rev) %>%
    # Filter out pairs with no data
    filter(!is.na(p_value) & !is.na(effect_size))

cat(paste("Manhattan data prepared:", nrow(manhattan_data), "pairs with data\n"))

# Calculate Bonferroni correction threshold
total_pairs <- nrow(manhattan_data)
bonferroni_threshold <- 0.05 / total_pairs
cat(paste("Total pairs for analysis:", total_pairs, "\n"))
cat(paste("Bonferroni threshold:", format(bonferroni_threshold, scientific = TRUE), "\n"))

# Prepare data for Manhattan plot
manhattan_data <- manhattan_data %>%
    # Convert p-values to -log10 scale
    mutate(
        neg_log10_p = -log10(p_value),
        # Create outcome code positions (numeric for x-axis)
        outcome_pos = as.numeric(as.factor(outcome_code)),
        # Create hover labels for interactive plot
        label = paste0(event_code, " → ", outcome_code, 
                      "\nEffect Size: ", round(effect_size, 4),
                      "\nP-value: ", format(p_value, scientific = TRUE, digits = 3)),
        # Color points based on significance
        significance_level = case_when(
            p_value < bonferroni_threshold ~ "Bonferroni Significant",
            p_value < 0.05 ~ "Nominally Significant", 
            TRUE ~ "Not Significant"
        )
    ) %>%
    # Order by outcome code for better visualization
    arrange(outcome_pos)

# Calculate threshold lines in -log10 scale
threshold_05 = -log10(0.05)
threshold_bonferroni = -log10(bonferroni_threshold)

# Create the Manhattan plot
cat("Creating Manhattan plot...\n")
p_manhattan <- ggplot(manhattan_data, aes(x = outcome_pos, y = neg_log10_p)) +
    # Add threshold lines
    geom_hline(yintercept = threshold_05, color = "blue", linetype = "dashed", size = 1, alpha = 0.7) +
    geom_hline(yintercept = threshold_bonferroni, color = "red", linetype = "dashed", size = 1, alpha = 0.7) +
    # Add points colored by significance
    geom_point(aes(color = significance_level), alpha = 0.7, size = 1.5) +
    # Color scheme
    scale_color_manual(
        values = c(
            "Bonferroni Significant" = "red",
            "Nominally Significant" = "orange", 
            "Not Significant" = "grey60"
        ),
        name = "Significance Level"
    ) +
    # Styling
    theme_minimal() +
    theme(
        axis.text.x = element_blank(),  # Remove x-axis labels as requested
        axis.ticks.x = element_blank(),
        axis.text.y = element_text(size = 10),
        axis.title = element_text(size = 12, face = "bold"),
        plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
        plot.subtitle = element_text(size = 10, hjust = 0.5),
        legend.title = element_text(size = 10, face = "bold"),
        legend.text = element_text(size = 9),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        plot.margin = margin(20, 20, 20, 20)
    ) +
    labs(
        title = "Manhattan Plot: ICD10 Diagnosis × ATC Prescription Associations",
        subtitle = paste0(
            "Blue line: p = 0.05  |  Red line: Bonferroni corrected (p = ", 
            format(bonferroni_threshold, scientific = TRUE, digits = 3), 
            ")  |  Total pairs: ", total_pairs
        ),
        x = "Outcome Codes (ATC Prescription Codes)",
        y = "-log₁₀(p-value)"
    ) +
    # Add annotations for threshold lines
    annotate("text", x = max(manhattan_data$outcome_pos) * 0.95, y = threshold_05 + 0.5, 
             label = "p = 0.05", color = "blue", fontface = "bold", hjust = 1) +
    annotate("text", x = max(manhattan_data$outcome_pos) * 0.95, y = threshold_bonferroni + 0.5, 
             label = paste0("Bonferroni\n(p = ", format(bonferroni_threshold, scientific = TRUE, digits = 2), ")"), 
             color = "red", fontface = "bold", hjust = 1)

# Save static plot
cat(paste("Saving Manhattan plot to", output_file, "...\n"))
ggsave(output_file, plot = p_manhattan, width = width, height = height, dpi = 300, bg = "white")

# Create interactive version with plotly
cat("Creating interactive Manhattan plot...\n")
p_interactive <- ggplot(manhattan_data, aes(x = outcome_pos, y = neg_log10_p, text = label)) +
    geom_hline(yintercept = threshold_05, color = "blue", linetype = "dashed", size = 1, alpha = 0.7) +
    geom_hline(yintercept = threshold_bonferroni, color = "red", linetype = "dashed", size = 1, alpha = 0.7) +
    geom_point(aes(color = significance_level), alpha = 0.7, size = 1.5) +
    scale_color_manual(
        values = c(
            "Bonferroni Significant" = "red",
            "Nominally Significant" = "orange", 
            "Not Significant" = "grey60"
        ),
        name = "Significance Level"
    ) +
    theme_minimal() +
    theme(
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.y = element_text(size = 10),
        axis.title = element_text(size = 12, face = "bold"),
        plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
        plot.subtitle = element_text(size = 10, hjust = 0.5),
        legend.title = element_text(size = 10, face = "bold"),
        legend.text = element_text(size = 9),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank()
    ) +
    labs(
        title = "Manhattan Plot: ICD10 Diagnosis × ATC Prescription Associations",
        subtitle = paste0(
            "Hover over points for details | Blue line: p = 0.05 | Red line: Bonferroni corrected | Total pairs: ", total_pairs
        ),
        x = "Outcome Codes (ATC Prescription Codes)",
        y = "-log₁₀(p-value)"
    )

# Convert to plotly and save
interactive_plot <- ggplotly(p_interactive, tooltip = "text")
htmlwidgets::saveWidget(interactive_plot, output_file_interactive, selfcontained = TRUE)

# Print summary statistics
cat("\n=== MANHATTAN PLOT SUMMARY STATISTICS ===\n")
significance_summary <- manhattan_data %>%
    summarise(
        total_pairs = n(),
        significant_05 = sum(p_value < 0.05),
        bonferroni_significant = sum(p_value < bonferroni_threshold),
        max_neg_log10_p = max(neg_log10_p),
        min_p_value = min(p_value),
        .groups = 'drop'
    )

cat(paste("Total pairs plotted:", significance_summary$total_pairs, "\n"))
cat(paste("Significant at p < 0.05:", significance_summary$significant_05, "\n"))
cat(paste("Bonferroni significant:", significance_summary$bonferroni_significant, "\n"))
cat(paste("Most significant p-value:", format(significance_summary$min_p_value, scientific = TRUE), "\n"))
cat(paste("Maximum -log10(p):", round(significance_summary$max_neg_log10_p, 2), "\n"))
