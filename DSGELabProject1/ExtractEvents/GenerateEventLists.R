suppressPackageStartupMessages({
    library(data.table)
    library(ggplot2)

})

# Configuration
INPUT_DIR <- "/media/volume/Projects/DSGELabProject1/DiD_Highthroughput/Version3/GeneratePairs/Pairs_20251028_0707/"  
OUTPUT_DIR <- INPUT_DIR

# Input files
atc_file <- file.path(INPUT_DIR, "ATC_AllCounts.csv")
icd_file <- file.path(INPUT_DIR, "ICD_AllCounts.csv")

# Output files
atc_output <- file.path(OUTPUT_DIR, paste0("event_codes_ATC_20251028_0707.csv"))
icd_output <- file.path(OUTPUT_DIR, paste0("event_codes_ICD_20251028_0707.csv"))
pairs_output <- file.path(OUTPUT_DIR, paste0("event_outcome_pairs_ATC_20251028_0707.csv"))


# Plot impact of different thresholds on amount of events retained
cat("Loading data for threshold analysis...\n")
atc_all <- fread(atc_file)
icd_all <- fread(icd_file)

# Test thresholds from 100 to max count in steps of 100
max_threshold <- max(c(max(atc_all$UNIQUE_ID_COUNT), max(icd_all$UNIQUE_ID_COUNT)))
thresholds <- seq(100, max_threshold, by = 100)

# Calculate number of codes retained at each threshold
threshold_results <- data.table(
    threshold = thresholds,
    ATC = sapply(thresholds, function(t) sum(atc_all$UNIQUE_ID_COUNT >= t)),
    ICD = sapply(thresholds, function(t) sum(icd_all$UNIQUE_ID_COUNT >= t))
)
threshold_results[, Total := ATC + ICD]
threshold_long <- melt(threshold_results, id.vars = "threshold", variable.name = "code_type", value.name = "count")

# Create plot
p <- ggplot(threshold_long[threshold <= 10000], aes(x = threshold, y = count, color = code_type)) +
    geom_line(linewidth = 1) +
    geom_point(size = 2) +
    geom_vline(xintercept = 1000, linetype = "solid", color = "black", linewidth = 0.5) +
    annotate("text", x = 1000, y = max(threshold_long[threshold <= 10000]$count) * 0.95, label = "N = 1000", hjust = -0.1, size = 3.5) +
    geom_vline(xintercept = 500, linetype = "solid", color = "black", linewidth = 0.5) +
    annotate("text", x = 500, y = max(threshold_long[threshold <= 10000]$count) * 0.90, label = "N = 500", hjust = -0.1, size = 3.5) +
    labs(x = "Minimum N of events required (thresholds)", y = "Number of Codes Retained ",color = "Code Type") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave(file.path(OUTPUT_DIR, "threshold_analysis.png"), p, width = 12, height = 6)

THRESHOLD <- 300 
# Read and filter ATC codes
cat("Processing ATC codes...\n")
atc <- fread(atc_file)
atc_filtered <- atc[UNIQUE_ID_COUNT >= THRESHOLD & CODE != ""]
fwrite(atc_filtered[, .(CODE)], atc_output, col.names = FALSE, quote = FALSE)

# Read and filter ICD codes
cat("Processing ICD codes...\n")
icd <- fread(icd_file)
icd_filtered <- icd[UNIQUE_ID_COUNT >= THRESHOLD & CODE != ""]
fwrite(icd_filtered[, .(CODE)], icd_output, col.names = FALSE, quote = FALSE)

# Generate event pairs 
cat("Generating ATC event pairs...\n")
atc_pairs <- data.table(
    event = paste0("Purch_", atc_filtered$CODE),
    outcome = atc_filtered$CODE
)
fwrite(atc_pairs, pairs_output, col.names = FALSE, quote = FALSE)


# Summary
cat("\n================================================\n")
cat("Filtering complete!\n")
cat(sprintf("Minimum count threshold: %d\n", THRESHOLD))
cat(sprintf("ATC output: %s\n", atc_output))
cat(sprintf("ICD output: %s\n", icd_output))
cat("================================================\n")