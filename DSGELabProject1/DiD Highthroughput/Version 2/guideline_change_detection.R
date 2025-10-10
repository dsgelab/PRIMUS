.libPaths("/shared-directory/sd-tools/apps/R/lib/")

#### Libraries:
suppressPackageStartupMessages({
    library(data.table)
    library(arrow)
    library(dplyr)
    library(tidyr)
    library(lubridate)
    library(fixest)
    library(marginaleffects)
    library(ggplot2)
    library(patchwork)
    library(future.apply)
})

##### Arguments
args = commandArgs(trailingOnly = TRUE)
outcome_list = "/media/volume/Projects/DSGELabProject1/DiD_Experiments/Version2_Highthroughput/ProcessedOutcomes_20250820/outcome_codes_ATC_20250809.csv"
outcomes_file = "/media/volume/Projects/DSGELabProject1/DiD_Experiments/Version2_Highthroughput/ProcessedOutcomes_20250820/processed_outcomes.parquet"
doctor_list = "/media/volume/Projects/DSGELabProject1/doctors_20250424.csv"
output_file = "/media/volume/Projects/mattferr/notes/note_20250909_1126_highthroughputvariance/results_allmeds.csv"
output_plot = "/media/volume/Projects/mattferr/notes/note_20250909_1126_highthroughputvariance/plot_variance_allmeds.png"

#### Main
N_THREADS = 10
setDTthreads(N_THREADS) 
# not using all threads to easily run in background

# get list of outcomes
outcome_codes = fread(outcome_list, header = FALSE)$V1

# prepare result df
results_df = data.table(
    code = character(),
    overall_var = numeric(),
    diff_var = numeric(),
    threshold1 = numeric(),
    threshold2 = numeric(),
    max_roll_var = numeric(),
    detected_change = logical()
)

for (i in seq_along(outcome_codes)) {
    code <- outcome_codes[i]
    # Load outcomes with specific columns only
    outcomes_cols = c("DOCTOR_ID", "MONTH", "YEAR", paste0("N_", code), paste0("Y_", code), paste0("first_month_", code), paste0("last_month_", code))
    outcomes = as.data.table(read_parquet(outcomes_file, col_select = outcomes_cols))

    # --------------------------------------------
    # test variance method

    # window_size : number of years for rolling variance
    # K : multiplier threshold
    window_size <- 5
    K <- 2

    # --- Prepare yearly averages ---
    dt <- as.data.table(outcomes)
    dt[, YEAR := as.integer(YEAR)]
    outcome_col <- paste0("Y_", code)

    avg_by_year <- dt[, .(avg_Y = mean(get(outcome_col), na.rm = TRUE)), by = YEAR]
    avg_by_year <- avg_by_year[order(YEAR)]

    # --- Year-to-year differences (trend changes) ---
    avg_by_year[, diff_Y := c(NA, diff(avg_Y))]

    # --- Rolling variance of differences ---
    avg_by_year[, roll_var := zoo::rollapply(diff_Y, width = window_size, FUN = var, align = "right", fill = NA)]
    overall_var <- var(avg_by_year$avg_Y, na.rm = TRUE)
    diff_var <- var(na.omit(avg_by_year$diff_Y))

    # --- Threshold ---
    threshold1 <- K * diff_var
    avg_by_year[, flag := roll_var > threshold1]
    threshold2 <- K * overall_var

    # --- Results ---
    max_roll_var <- max(avg_by_year$roll_var, na.rm = TRUE)
    detected_change <- any(avg_by_year$flag, na.rm = TRUE)

    # Add results as a row in the dataframe
    results_df <- rbind(
        results_df,
        data.table(
            code = code,
            overall_var = overall_var,
            diff_var = diff_var,
            threshold1 = threshold1,
            threshold2 = threshold2,
            max_roll_var = max_roll_var,
            detected_change = detected_change
        ),
        fill = TRUE
    )

    # Print start and then every 50 codes
    if (i == 1) {
        cat("Starting processing, example process\n")
        print(results_df)
        flush.console()
    } else if (i %% 50 == 0) {
        cat(sprintf("Processed %d codes...\n", i))
        flush.console()
    }
}

# save dataframe to csv
fwrite(results_df, output_file)
# reload if necessary
# results_df = fread(output_file)

# Define threshold before plotting
threshold_val <- 0.5e-4

# Plot overall variance
p <- ggplot(results_df, aes(y = code, x = overall_var)) +
    geom_bar(stat = "identity", fill = "blue") +
    labs(title = "Overall Variance of Yearly Prescription Averages", y = NULL, x = "Overall Variance") +
    theme_minimal() +
    theme(
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank()
    ) +
    geom_text(
        data = results_df[overall_var > threshold_val],
        aes(label = code),
        hjust = -0.1, size = 3, color = "red"
    )

# save plot
ggsave(filename = output_plot, plot = p, width = 10, height = 8)

# Evaluate how many codes surpass the threshold
codes_above_threshold <- results_df[overall_var > threshold_val, code]
cat(sprintf("Number of codes with overall_var > %g: %d\n", threshold_val, length(codes_above_threshold)))
cat("Codes above threshold:\n")
print(codes_above_threshold)


