# Utility functions used in multiple scripts

# Function to get the latest file path in cases where there are multiple files with the same name
# but different dates, for example, diagnosis_20250505 and diagnosis_20250520.
get_latest_file <- function(prefix) {
    files <- list.files(pattern = paste0("^", prefix, "_[0-9]{8}\\.csv$"))

    if (length(files) == 0) {
        stop("No matching files found for prefix: ", prefix)
    }

    dates <- as.Date(gsub(paste0(".*", prefix, "_([0-9]{8})\\.csv"), "\\1", files), "%Y%m%d")
    filename <- files[which.max(dates)]
    filepath <- file.path(getwd(), filename)
    filepath
}