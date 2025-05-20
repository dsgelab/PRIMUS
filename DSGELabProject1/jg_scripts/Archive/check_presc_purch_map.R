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





# Calculate counts by year for each scenario
counts_by_scenario <- dp_wide2 %>%
  mutate(year = year(coalesce(date_Prescription, date_Purchase))) %>%
  count(year, scenario)

# Spread the counts into separate columns for each scenario
counts_by_year <- counts_by_scenario %>%
  pivot_wider(names_from = scenario, values_from = n, values_fill = list(n = 0))

# Plot the comparison
counts_by_year %>%
  ggplot(aes(x = factor(year))) +  # Use factor to ensure discrete year labels
  geom_bar(aes(y = `Both Prescription & Purchase`, fill = "Both Prescription & Purchase"), stat = "identity", color = "black", width = 0.7) +
  geom_bar(aes(y = `Prescription only`, fill = "Prescription only"), stat = "identity", color = "black", width = 0.5) +
  geom_bar(aes(y = `Purchase only`, fill = "Purchase only"), stat = "identity", color = "black", width = 0.3) +
  labs(
    title = "Counts by Year and Scenario",
    x = "Year",
    y = "Count",
    fill = "Scenario"
  ) +
  scale_fill_manual(values = c("Both Prescription & Purchase" = "lightgrey", "Prescription only" = "steelblue", "Purchase only" = "darkred")) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    axis.title.x = element_text(size = 14, face = "bold"),
    axis.title.y = element_text(size = 14, face = "bold"),
    axis.text = element_text(size = 12),
    legend.title = element_text(size = 14, face = "bold"),
    legend.text = element_text(size = 12)
  )






# Calculate counts by year for each scenario
counts_by_scenario <- dp_wide2 %>%
  mutate(year = year(coalesce(date_Prescription, date_Purchase))) %>%
  count(year, scenario)

# Spread the counts into separate columns for each scenario
counts_by_year <- counts_by_scenario %>%
  pivot_wider(names_from = scenario, values_from = n, values_fill = list(n = 0))

# Calculate the total count for each year
counts_by_year <- counts_by_year %>%
  mutate(total = `Both Prescription & Purchase` + `Prescription only` + `Purchase only`)

# Calculate the percentages for each scenario
counts_by_year <- counts_by_year %>%
  mutate(
    perc_both = (`Both Prescription & Purchase` / total) * 100,
    perc_prescription = (`Prescription only` / total) * 100,
    perc_purchase = (`Purchase only` / total) * 100
  )

# Reshape the data for plotting
counts_long <- counts_by_year %>%
  select(year, perc_both, perc_prescription, perc_purchase) %>%
  pivot_longer(cols = starts_with("perc_"), names_to = "scenario", values_to = "percentage")

# Plot the comparison
counts_long %>%
  ggplot(aes(x = factor(year), y = percentage, fill = scenario)) +
  geom_bar(stat = "identity", position = "stack", color = "black") +
  geom_text(aes(label = sprintf("%.1f%%", percentage)), 
            position = position_stack(vjust = 0.5), size = 3) +
  labs(
    title = "Percentage by Year and Scenario",
    x = "Year",
    y = "Percentage",
    fill = "Scenario"
  ) +
  scale_fill_manual(
    values = c(
      "perc_both" = "lightgrey",
      "perc_prescription" = "steelblue",
      "perc_purchase" = "darkred"
    ),
    labels = c(
      "perc_both" = "Both Prescription & Purchase",
      "perc_prescription" = "Prescription only",
      "perc_purchase" = "Purchase only"
    )
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    axis.title.x = element_text(size = 14, face = "bold"),
    axis.title.y = element_text(size = 14, face = "bold"),
    axis.text = element_text(size = 12),
    legend.title = element_text(size = 14, face = "bold"),
    legend.text = element_text(size = 12)
  )





dp_wide2 %>% 
  filter(scenario == "Purchase only") %>% 
  filter(year(date_Purchase) >= 2016)


standalone_Purchases <- dp_wide2 %>% 
  filter(scenario == "Purchase only") %>%
  select(PATIENT_ID, CODE, date_Purchase) 
  # 33,867,515 rows

standalone_Purchases %>% distinct(PATIENT_ID, CODE) %>% nrow() # 24,345,035
24345035/33867515 * 100 # 71.9% unique PATIENT_ID, CODE pairs


prescriptions_wawopurch <- dp_wide2 %>% 
  filter(scenario == "Prescriptions only" | scenario == "Both Prescription & Purchase") %>%
  filter(paste(PATIENT_ID, CODE) %in% paste(standalone_Purchases$PATIENT_ID, standalone_Purchases$CODE))

standalone_Purchases <- standalone_Purchases %>%
  left_join(prescriptions_wawopurch %>% select(PATIENT_ID, CODE, date_Prescription), by = c("PATIENT_ID", "CODE")) %>%
  mutate(
    date_diff = as.numeric(difftime(date_Purchase, date_Prescription, units = "days"))
  ) %>%
  group_by(PATIENT_ID, CODE, date_Purchase) %>%
  slice_min(order_by = abs(date_diff), n = 1) %>%
  ungroup() %>%
  select(PATIENT_ID, CODE, date_Purchase, date_Prescription)