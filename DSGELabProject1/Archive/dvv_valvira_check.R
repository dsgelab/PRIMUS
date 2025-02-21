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

mpath <- "C:/Users/.../"
setwd(mpath)

dvv  <- fread("DVV/dvv5.csv")
valvira <- fread("valvira.csv", encoding = "Latin-1")

# both datasets contain a column named "FID"
# check how many unique IDs are in each dataset
# check how many unique IDs in Valvira are also in DVV and get percentage of missingness

unique_IDs_valvira <- valvira %>% 
    select(FID) %>% 
    distinct() %>% 
    pull()

unique_IDs_dvv <- dvv %>%
    select(FID) %>% 
    distinct() %>% 
    pull()

# Find the number of Valvira IDs that are also in DVV
valvira_in_dvv <- sum(unique_IDs_valvira %in% unique_IDs_dvv)

# Find the number of Valvira IDs that are missing in DVV
valvira_missing_in_dvv <- length(unique_IDs_valvira) - valvira_in_dvv

# Calculate the percentage of Valvira IDs that are missing in DVV
percentage_missing <- (valvira_missing_in_dvv / length(unique_IDs_valvira)) * 100

# Print the results
cat("Number of Valvira IDs in DVV:", valvira_in_dvv, "\n")
cat("Number of Valvira IDs missing in DVV:", valvira_missing_in_dvv, "\n")
cat("Percentage of Valvira IDs missing in DVV:", percentage_missing, "%\n")

# Filter Valvira IDs that are missing in DVV
missing_valvira_ids <- unique_IDs_valvira[!(unique_IDs_valvira %in% unique_IDs_dvv)]

# Get the rows in Valvira that have the missing IDs
missing_valvira_rows <- valvira %>%
    filter(FID %in% missing_valvira_ids)

# Table the Degree country column for the missing Valvira IDs
table(missing_valvira_rows$Suoritusmaa)

# Check the distribution of the missing Valvira IDs by Suoritusmaa
missing_valvira_rows %>%
    group_by(Suoritusmaa) %>%
    summarise(count = n()) %>%
    arrange(desc(count)) %>%
    ggplot(aes(x = reorder(Suoritusmaa, count), y = count)) +
    geom_bar(stat = "identity") +
    coord_flip() +
    labs(title = "Distribution of missing Valvira IDs by Suoritusmaa",
         x = "Suoritusmaa",
         y = "Count") +
    theme_minimal()

# Calculate the percentage of FID missingness in DVV by each degree country
missingness_by_country <- valvira %>%
    distinct(FID, .keep_all = TRUE) %>%
    group_by(Suoritusmaa) %>%
    summarise(
        total = n(),
        missing = sum(FID %in% missing_valvira_ids),
        percentage_missing = (missing / total) * 100
    ) %>%
    arrange(desc(percentage_missing))

# Print the results
print(missingness_by_country)

# Visualize the percentage of FID missingness in DVV by each degree country along with absolute missingness
missingness_by_country %>%
    filter(percentage_missing > 0) %>%
    ggplot(aes(x = reorder(Suoritusmaa, percentage_missing), 
               y = percentage_missing, 
               size = missing, 
               color = percentage_missing)) +
    geom_point(alpha = 0.7) +
    scale_color_viridis_c() +
    labs(title = "Percentage and Absolute FID Missingness in DVV by Degree Country",
         x = "Degree Country",
         y = "Percentage Missing",
         size = "Absolute Missing") +
    theme_minimal() +
    coord_flip()

# How many Valvira IDs that are missing in DVV have a degree from Suomi?
missing_valvira_rows %>%
    distinct(FID, .keep_all = TRUE) %>%
    filter(Suoritusmaa == "Suomi") %>%
    nrow()/nrow(missing_valvira_rows)*100 
    # 13.63% of the missing Valvira IDs have a degree from Suomi


