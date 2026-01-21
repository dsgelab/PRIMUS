suppressPackageStartupMessages({
    library(data.table)
    library(ggplot2)
})

# Configuration
renamed_ATC_file <- "/media/volume/Projects/ATC_renamed_codes.csv"
INPUT_DIR <- "/media/volume/Projects/DSGELabProject1/ProcessedData/Pairs_20251028/"  
OUTPUT_DIR <- INPUT_DIR

# Input files
atc_file <- file.path(INPUT_DIR, "ATC_AllCounts.csv")
icd_file <- file.path(INPUT_DIR, "ICD_AllCounts.csv")
# Output files
atc_output <- file.path(OUTPUT_DIR, paste0("ATC_codes_20251028.csv"))
icd_output <- file.path(OUTPUT_DIR, paste0("ICD_codes_20251028.csv"))

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
    labs(x = "Minimum N of events required (thresholds)", y = "Number of Codes Retained ",color = "Code Type") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave(file.path(OUTPUT_DIR, "threshold_analysis.png"), p, width = 12, height = 6)

# Set threshold for filtering
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

# Generate list of ICD10 diagnosis codes for DiD analysis
icd_filtered$NewCode <- paste0("Diag_", icd_filtered$CODE)
outfile = file.path("/media/volume/Projects/DSGELabProject1/DiD_Diagnosis/event_codes_ICD_20251028.csv")
fwrite(icd_filtered[, .(NewCode)], outfile, col.names = FALSE, quote = FALSE)

# Generate event and event-outcome pair ATC codes for DiD analysis
# Check renamed ATC codes and fix accordingly
renamed_atc <- fread(renamed_ATC_file)
event_output <- file.path("/media/volume/Projects/DSGELabProject1/DiD_Medications/event_codes_ATC_20251028.csv")
outcome_output <- file.path("/media/volume/Projects/DSGELabProject1/DiD_Medications/outcome_codes_ATC_20251028.csv")
pairs_output <- file.path("/media/volume/Projects/DSGELabProject1/DiD_Medications/event_outcome_pairs_ATC_20251028.csv")

old_codes <- renamed_atc[renamed_atc$ATC_NEW %in% atc_filtered$CODE, .(ATC_NEW, ATC_OLD)]
atc_filtered_new <- rbind(atc_filtered, data.table(CODE = old_codes$ATC_OLD, UNIQUE_ID_COUNT = NA), fill = TRUE)
atc_outcome <- atc_filtered_new[,.(CODE)]
cat("Adding old ATC codes to the list...\n")
cat("Number of old codes added to list: ", nrow(old_codes), "\n")
atc_events <- data.table(event = paste0("Purch_", atc_filtered_new$CODE))

fwrite(atc_outcome, outcome_output, col.names = FALSE, quote = FALSE)
fwrite(atc_events, event_output, col.names = FALSE, quote = FALSE)
atc_pairs <- data.table(
    event = paste0("Purch_", atc_filtered_new$CODE),
    outcome = atc_filtered_new$CODE
)
fwrite(atc_pairs, pairs_output, col.names = FALSE, quote = FALSE)


# Summary
cat("\n================================================\n")
cat("Filtering complete!\n")
cat(sprintf("ATC output: %s\n", atc_output))
cat(sprintf("ICD output: %s\n", icd_output))
cat(sprintf("Minimum count threshold: %d\n", THRESHOLD))
cat("Generated DiD analysis code lists:\n")
cat(sprintf("ICD diagnosis output: %s\n", outfile))
cat(sprintf("ATC event output: %s\n", event_output))
cat(sprintf("ATC outcome output: %s\n", outcome_output))
cat(sprintf("ATC pairs output: %s\n", pairs_output))
cat("================================================\n")
