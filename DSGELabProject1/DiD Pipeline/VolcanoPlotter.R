library(ggplot2)
library(dplyr)

# Load the combined results
data <- read.csv("/media/volume/Projects/DSGELabProject1/DiD_Experiment/Version5/CombinedResults_20250722.csv")

# Calculate Bonferroni correction
n_tests <- nrow(data)
bonferroni_threshold <- 0.05 / n_tests
bonferroni_log10 <- -log10(bonferroni_threshold)

# Prepare data for plotting
volcano_data <- data %>%
  mutate(
    log10_p = -log10(p_value),
    significant = p_value < bonferroni_threshold,
    category = case_when(
      p_value < bonferroni_threshold & abs(effect_size) > 0.5 ~ "Highly Significant",
      p_value < bonferroni_threshold ~ "Significant", 
      TRUE ~ "Not Significant"
    )
  ) %>%
  filter(!is.na(effect_size) & !is.na(p_value) & p_value > 0)

# Print summary statistics
cat("Analysis Summary:\n")
cat("Total tests:", n_tests, "\n")
cat("Bonferroni threshold:", sprintf("%.2e", bonferroni_threshold), "\n")
cat("Significant results:", sum(volcano_data$significant), "\n")
cat("Highly significant results:", sum(volcano_data$category == "Highly Significant"), "\n\n")

# Calculate symmetric x-axis limits
x_range <- range(volcano_data$effect_size, na.rm = TRUE)
x_max <- max(abs(x_range))
x_padding <- x_max * 0.2
x_limits <- c(-(x_max + x_padding), x_max + x_padding)

# Create volcano plot
p <- ggplot(volcano_data, aes(x = effect_size, y = log10_p, color = category)) +
  geom_point(alpha = 0.7, size = 1.5) +
  
  # Add experiment labels
  geom_text(aes(label = source_experiment), size = 2, alpha = 0.9, 
            hjust = 0.5, vjust = -0.5, 
            check_overlap = TRUE, show.legend = FALSE) +
  
  # Add significance thresholds
  geom_hline(yintercept = -log10(0.05), linetype = "dashed", color = "blue", alpha = 0.8) +
  geom_hline(yintercept = bonferroni_log10, linetype = "dashed", color = "red", alpha = 0.8) +
  geom_vline(xintercept = c(-0.5, 0.5), linetype = "dashed", color = "gray", alpha = 0.6) +
  
  # Set x-axis limits
  xlim(x_limits) +
  
  # Color scheme
  scale_color_manual(values = c(
    "Highly Significant" = "#dc2626",
    "Significant" = "#ea580c", 
    "Not Significant" = "#6b7280"
  )) +
  
  # Labels and theme
  labs(
    title = "Volcano Plot with Bonferroni Correction",
    subtitle = paste0("Bonferroni threshold: α = 0.05/", n_tests, " = ", sprintf("%.2e", bonferroni_threshold)),
    x = "Effect Size",
    y = "-log10(p-value)",
    color = "Significance"
  ) +
  
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, size = 12),
    legend.position = "bottom",
    panel.grid.minor = element_blank()
  ) +
  
  # Add annotations for threshold lines
  annotate("text", x = Inf, y = bonferroni_log10, 
           label = "Bonferroni threshold", hjust = 1.1, vjust = -0.5, 
           color = "red", size = 3) +
  annotate("text", x = Inf, y = -log10(0.05), 
           label = "α = 0.05", hjust = 1.1, vjust = -0.5, 
           color = "blue", size = 3)

# Save the plot with more space for labels
ggsave("/media/volume/Projects/DSGELabProject1/DiD_Experiment/Version5/volcano_plot_20250722.png", plot = p, width = 12, height = 10, dpi = 300)
cat("Plot saved")