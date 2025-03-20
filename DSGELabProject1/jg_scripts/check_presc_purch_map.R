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

dp_longitudinal <- fread("filtered_dp_longitudinal_2014182025-03-18.csv.gz")
glimpse(dp_longitudinal)
head(dp_longitudinal)
table(dp_longitudinal$REGISTER) # Results are in JG PRIMUS NOTES DOC

# remove rows with empty CODES
dp_longitudinal <- dp_longitudinal %>% filter(!is.na(CODE) & CODE != "")

dp_first <- dp_longitudinal %>% 
    group_by(DOCTOR_ID, PATIENT_ID, CODE, REGISTER) %>% 
    arrange(DATE) %>%
    slice(1) 

# fwrite(dp_first, "/media/volume/Projects/jg/first_presc_purch_2014182025-03-18.csv.gz")

table(dp_first$REGISTER) # Results are in JG PRIMUS NOTES DOC

# dp_first <- dp_first %>%
#     mutate(group_id = group_indices(., DOCTOR_ID, PATIENT_ID, CODE))

# dp_first %>% 
#     ungroup() %>% 
#     group_by(DOCTOR_ID, PATIENT_ID, CODE) %>%
#     arrange(DOCTOR_ID, PATIENT_ID, CODE, DATE)

dp_wide <- dp_first %>%
    ungroup() %>%
  # We only need the key columns plus DATE
  select(DOCTOR_ID, PATIENT_ID, CODE, REGISTER, DATE) %>%
  # Pivot so that "Prescription" vs "Purchase" become columns
  pivot_wider(
    names_from  = REGISTER,     # "Prescription" or "Purchase"
    values_from = DATE,         # The earliest date (already in your data)
    names_prefix = "date_"      # So columns become date_Prescription / date_Purchase
  )

presc_purch_avail <- dp_wide %>%
    summarise(
        both_dates_count = sum(!is.na(date_Prescription) & !is.na(date_Purchase)),
        only_prescription_date_count = sum(!is.na(date_Prescription) & is.na(date_Purchase)),
        only_purchase_date_count = sum(is.na(date_Prescription) & !is.na(date_Purchase))
    )

cat("Rows with both prescription and purchase dates:", presc_purch_avail$both_dates_count, "\n")
cat("Rows with only prescription date:", presc_purch_avail$only_prescription_date_count, "\n")
cat("Rows with only purchase date:", presc_purch_avail$only_purchase_date_count, "\n")

dp_wide2 <- dp_wide %>%
ungroup %>% 
  mutate(
    # Simple scenario classification
    scenario = case_when(
      !is.na(date_Prescription) & !is.na(date_Purchase) ~ "Both Prescription & Purchase",
      !is.na(date_Prescription) &  is.na(date_Purchase) ~ "Prescription only",
      is.na(date_Prescription)  & !is.na(date_Purchase) ~ "Purchase only",
      TRUE ~ "Neither" # if it’s possible to have neither
    ),
    # Check ordering if both exist
    purchase_after_prescription = case_when(
      !is.na(date_Prescription) & !is.na(date_Purchase) &
        (date_Purchase >= date_Prescription) ~ TRUE,
      TRUE ~ FALSE
    ),
    # Or compute difference in days if you want
    diff_days = as.numeric(as.Date(date_Purchase) - as.Date(date_Prescription))
  )

dp_wide2 %>%
  count(scenario)

dp_wide2 %>%
  filter(scenario == "Both Prescription & Purchase") %>%
  count(purchase_after_prescription)

dp_wide2 %>% 
    filter(scenario == "Both Prescription & Purchase") %>%
    filter(year(date_Purchase) >= 2016)
    filter(!purchase_after_prescription) %>%
    summarise(mead(diff_days, na.rm = TRUE))

# Visualize the diff_days for the scenario "Both Prescription & Purchase" using a boxplot
dp_wide2 %>%
  filter(scenario == "Both Prescription & Purchase") %>%
  ggplot(aes(x = scenario, y = diff_days)) +
  geom_boxplot() +
  labs(
    title = "Difference in Days between Prescription and Purchase",
    x = "Scenario",
    y = "Difference in Days"
  ) +
  theme_minimal()

dp_wide2 %>% 
  filter(diff_days >= 750)

