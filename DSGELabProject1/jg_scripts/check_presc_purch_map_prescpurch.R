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

dp_prescpurch <- fread("filtered_dp_long_prescpurch_201418_2025-03-24.csv.gz")

glimpse(dp_prescpurch)
head(dp_prescpurch)
head(dp_prescpurch %>% filter(REGISTER == "Prescription"))
# dp_prescpurch <- dp_prescpurch %>%
#     mutate(
#         CODE = ifelse(REGISTER == "Purchase", PRESCRIPTION_DATE, CODE),
#         PRESCRIPTION_DATE = ifelse(REGISTER == "Purchase", CODE, PRESCRIPTION_DATE),
#         PRESCRIPTION_DATE = as.Date(PRESCRIPTION_DATE, format = "%Y-%m-%d")
#     )

summary(dp_prescpurch)
summary(dp_prescpurch %>% filter(REGISTER == "Purchase"))

# remove rows with empty CODES
dp_prescpurch <- dp_prescpurch %>% filter(!is.na(CODE) & CODE != "")


matched_purch <- dp_prescpurch %>%
    group_by(PATIENT_ID, CODE) %>%
    filter(
        REGISTER == "Prescription" |
        (REGISTER == "Purchase" & PRESCRIPTION_DATE %in% PRESCRIPTION_DATE[REGISTER == "Prescription"])
    ) %>%
    ungroup()

# choose some random PATIENT_IDs from dp_prescpurch to extract their raw Prescription and Purchase data
set.seed(123)
sample(dp_prescpurch$PATIENT_ID, 10)

# !! After extracting the raw data using the dp_long_extract_patients.py script !!
# raw extracted Prescription and Purchase data
patlist <- fread("doctor_patient_prescpurch_patlist_20250327.csv")
patlist <- patlist %>% filter(DATE >= "2014-01-01" & DATE <= "2018-01-01") # to select the same interval

# History of purchases and prescriptions for a specific PATIENT_ID XXXXXXX
tmp_A  <- dp_prescpurch %>% filter(PATIENT_ID == "XXXXXXX") %>% arrange(CODE, DATE)
tmp_B  <- patlist %>% filter(PATIENT_ID == "XXXXXXX") %>% arrange(CODE, DATE)


tmp_retained_prescpurch <- tmp_A %>% 
  filter(paste(CODE, PRESCRIPTION_DATE) %in% paste(tmp_B$CODE, tmp_B$PRESCRIPTION_DATE))

tmp_A <- tmp_A %>% 
    semi_join(tmp_B, by = c("CODE", "PRESCRIPTION_DATE"))


# Create a timeline plot with two stacked timelines (one for each REGISTER) for tmp_A and tmp_B
plot_A <- tmp_A %>%
    ggplot(aes(x = DATE, y = REGISTER, color = REGISTER)) +
    geom_point() +
    scale_y_discrete(limits = c("Prescription", "Purchase")) +
    scale_x_date(limits = range(c(tmp_A$DATE, tmp_B$DATE))) + # Align x-axis
    labs(
        title = "Timeline of Prescriptions and Purchases (tmp_A)",
        x = "Date",
        y = "Register"
    ) +
    theme_minimal() +
    theme(legend.position = "none")

plot_B <- tmp_B %>%
    ggplot(aes(x = DATE, y = REGISTER, color = REGISTER)) +
    geom_point() +
    scale_y_discrete(limits = c("Prescription", "Purchase")) +
    scale_x_date(limits = range(c(tmp_A$DATE, tmp_B$DATE))) + # Align x-axis
    labs(
        title = "Timeline of Prescriptions and Purchases (tmp_B)",
        x = "Date",
        y = "Register"
    ) +
    theme_minimal() +
    theme(legend.position = "none")

# Combine the two plots, stacking one over the other
library(patchwork)
plot_A / plot_B