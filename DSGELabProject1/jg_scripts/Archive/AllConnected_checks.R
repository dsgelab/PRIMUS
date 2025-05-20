library(dplyr)
library(tidyr)
library(ggplot2)
library(readr)
library(purrr)
library(tibble)
library(stringr)
library(lubridate)
library(stringr)
library(data.table)
library(R.utils)

mpath <- "/media/volume/Projects/jg/"
setwd(mpath)

# Load the data
chunk_size <- 100000  # Define the number of rows per chunk
dp_prescpurch <- NULL  # Initialize an empty data frame

# Read the file in chunks and process each chunk
repeat {
    chunk <- fread("AllConnected_prescpurch_20250327_173343.csv", nrows = chunk_size, skip = nrow(dp_prescpurch), header = TRUE)
    if (nrow(chunk) == 0) break  # Exit loop if no more rows
    dp_prescpurch <- bind_rows(dp_prescpurch, chunk)
}
glimpse(dp_prescpurch)