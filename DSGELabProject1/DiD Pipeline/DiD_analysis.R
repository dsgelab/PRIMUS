#### Info:
# This script takes as input a list of doctor ids (cases + controls) and two datasets Events.csv and Outcomes.csv
# It then performs a difference-in-differences analysis based on the input data

#### Libraries:
library(data.table)
library(dplyr)
library(tidyr)
library(lubridate)
library(fixest)
library(ggplot2)
library(patchwork)

##### Arguments
args = commandArgs(trailingOnly = TRUE)
doctor_list = args[1]
events_file = args[2]
event_code = args[3]
outcomes_file = args[4]
outcome_code = args[5]
covariates_file = args[6]
outdir = args[7]

COLOR_MALE = "blue"
COLOR_FEMALE = "orange"

# Functions
enrichment_func_outcome <- function(s) {
    mean_Y_s = mean_Y[SPECIALTY == s]
    mean_Y_others = mean(mean_Y[SPECIALTY != s], na.rm = TRUE)
    ifelse(mean_Y_others == 0, NA, mean_Y_s / mean_Y_others)
}

#### Main

# Load data
doctor_ids = fread(doctor_list, header = FALSE)$V1
events = fread(events_file)
event_ids = setdiff(unique(events$PATIENT_ID), doctor_ids)
cat(paste0("All cases : ", length(event_ids), "\n"))
cat(paste0("Controls : ", length(doctor_ids)-length(event_ids), "\n"))
outcomes = fread(outcomes_file)
covariates = fread(covariates_file)

# prepare outcomes for DiD analysis
outcomes = outcomes[outcomes$DOCTOR_ID != outcomes$PATIENT_ID, ] # remove self-prescriptions
outcomes = outcomes[DOCTOR_ID %in% doctor_ids,] #only use doctors that are in our cohort
outcomes = outcomes[!is.na(CODE) & !is.na(DATE)]
outcomes = outcomes[DATE >= as.Date("1998-01-01")] # QC: remove events before 1998
outcomes[, MONTH := (as.numeric(format(DATE, "%Y")) - 1998) * 12 + as.numeric(format(DATE, "%m"))]
outcomes = outcomes[, .(
    Ni = sum(grepl(paste0("^", outcome_code), CODE)),
    N = .N
), by = .(DOCTOR_ID, MONTH)]
outcomes[, Y := fifelse(N == 0, NA_real_, Ni / N)]
outcomes[, YEAR := 1998 + (MONTH - 1) %/% 12]

# prepare events for DiD analysis + merge with outcomes
events = events[, .(PATIENT_ID, CODE, DATE)]
events = events[PATIENT_ID %in% doctor_ids,] #only use doctors that are in our cohort
events = events[events[, .I[which.min(DATE)], by = .(PATIENT_ID, CODE)]$V1] # only use first event
events = events[, c("PATIENT_ID", "DATE")] %>% rename("DOCTOR_ID" = "PATIENT_ID")
df_merged = left_join(outcomes, events, by = "DOCTOR_ID")
df_merged = df_merged %>%
    mutate(
        EVENT = if_else(!is.na(DATE), 1, 0),
        EVENT_YEAR = if_else(!is.na(DATE), as.numeric(format(DATE, "%Y")), NA_real_),
        EVENT_MONTH = if_else(!is.na(DATE), (as.numeric(format(DATE, "%Y")) - 1998) * 12 + as.numeric(format(DATE, "%m")), NA_real_),
    ) %>%
    select(-DATE)

# Prepare  covariates and specialty + merge them in the main dataframe
covariates_new = covariates %>%
    select(DOCTOR_ID, BIRTH_DATE, SEX, INTERPRETATION) %>%
    mutate(SPECIALTY = as.character(INTERPRETATION)) %>% #currently using interpretation of longest specialty
    mutate(BIRTH_YEAR = as.numeric(substr(BIRTH_DATE, 1, 4))) %>% # date format is YYYY-MM-DD
    select(-BIRTH_DATE, -INTERPRETATION)
df_complete = merge(df_merged, covariates_new, by = "DOCTOR_ID", how = "left") %>% as_tibble()
df_complete = df_complete %>% 
    mutate(
        AGE = YEAR - BIRTH_YEAR,
        AGE_IN_2023 = 2023 - BIRTH_YEAR,
        AGE_AT_EVENT = if_else(is.na(EVENT_YEAR), NA_real_, EVENT_YEAR - BIRTH_YEAR)
    )
events_after65 = df_complete %>% filter(AGE_AT_EVENT > 65) %>% pull(DOCTOR_ID) %>% unique()
df_complete = df_complete %>% 
    filter(!(DOCTOR_ID %in% events_after65)) %>% # remove people which experiment the event after pension (age 65)
    filter(AGE <= 65) # remove all prescriptions done after pension (age 65)

# check distribution of events over the years
df_plot = df_complete %>% distinct(DOCTOR_ID, .keep_all = TRUE) %>% na.omit(EVENT_YEAR)
p1_general = ggplot(df_plot, aes(x = factor(EVENT_YEAR))) +
    geom_bar(aes(y = ..count..)) +
    labs(title = paste0("Count of (First) Events Over the Years, N = ",length(unique(df_plot$DOCTOR_ID))), x = "Event Year") 
specialty_enrichment = df_complete %>%
    group_by(DOCTOR_ID) %>% slice_tail(n = 1) %>% ungroup() %>% # use only one row per ID, last one
    group_by(SPECIALTY) %>%
    summarise(event_count = sum(EVENT, na.rm = TRUE),total_count = n(),freq = event_count / total_count) %>%
    mutate(enrichment = freq / sapply(SPECIALTY, function(s) {mean(freq[SPECIALTY != s], na.rm = TRUE)}), enrichment = round(enrichment, 2)) %>%
    arrange(desc(enrichment))
p1_specialty = ggplot(specialty_enrichment, aes(x = SPECIALTY, y = enrichment)) +
    geom_bar(stat = "identity") +
    labs(title = "Enrichment of Event in Specialties", x = "Specialty", y = "Enrichment Ratio") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
p1_age = ggplot(df_plot, aes(x = AGE_AT_EVENT, fill = factor(SEX))) +
    geom_density(alpha = 0.5, show.legend = FALSE) +
    scale_fill_manual(values = c("1" = COLOR_MALE, "2" = COLOR_FEMALE)) +
    facet_grid(~ factor(SEX, levels = c(1,2), labels = c(paste0("Male (n=", sum(df_plot$SEX == 1, na.rm = TRUE), ")"),paste0("Female (n=", sum(df_plot$SEX == 2, na.rm = TRUE), ")"))), drop = FALSE) +
    labs(title = "Distribution of Age at First Event by Sex", x = "Age") +
    theme_minimal()
combined_plot1 = p1_general / p1_specialty / p1_age
ggsave(filename = file.path(outdir, "distribution_events.png"), plot = combined_plot1, width = 10, height = 12)

# Report number of cases and controls
cat(paste0("Cases that prescribed at least once: ", length(unique(df_plot$DOCTOR_ID)), "\n"))
cat(paste0("Controls : ", length(doctor_ids)-length(unique(df_plot$DOCTOR_ID)), "\n"))

# check distribution of outcome prescription over the years
p2_general = ggplot(df_complete, aes(x = YEAR)) +
    stat_summary(aes(y = Y), fun = mean, geom = "line", size = 1) +
    labs(title = "general Doctor Population Prescription Ratio Y=Ni/N Over the Years")
specialty_enrichment_outcome = df_complete %>%
    group_by(SPECIALTY) %>%
    summarise(mean_Y = mean(Y, na.rm = TRUE),n = n()) %>%
    ungroup() %>% mutate(enrichment = sapply(SPECIALTY, enrichment_func_outcome)) %>%
    arrange(desc(enrichment))
p2_specialty = ggplot(specialty_enrichment_outcome, aes(x = SPECIALTY, y = enrichment)) +
    geom_bar(stat = "identity", fill = "steelblue") +
    labs(title = "Enrichment of Outcome Ratio (Y) by Specialty", x = "Specialty", y = "Enrichment Ratio") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
df_birthyear_sex = df_complete %>%
    group_by(BIRTH_YEAR, SEX) %>%
    summarise(mean_Y = mean(Y, na.rm = TRUE)) %>%
    ungroup()
p2_birthyear = ggplot(df_birthyear_sex, aes(x = BIRTH_YEAR, y = mean_Y, color = factor(SEX), group = SEX)) +
    geom_line(size = 1) +
    scale_color_manual(values = c("1" = COLOR_MALE, "2" = COLOR_FEMALE), labels = c("Male", "Female")) +
    labs(title = "Average Prescription Ratio (Y) by Birth Year and Sex", x = "Birth Year", y = "Mean Y", color = "Sex") +
    theme_minimal()
combined_plot2 = p2_general / p2_specialty / p2_birthyear
ggsave(filename = file.path(outdir, "distribution_outcomes.png"), plot = combined_plot2, width = 10, height = 12)

# DiD analysis model

# Model 1: Comparing prescription ratios in Events vs Non-Events
# - adjusting analysis for age, sex and specialty
# - adding interaction with age and year of event
model_formula = as.formula("Y ~ AGE_IN_2023 + SEX + factor(SPECIALTY) + EVENT")
model = fixest::feols(model_formula, data = df_complete, fixef.rm = "none")
results = data.frame(summary(model)$coeftable)
write.csv(results, file = paste0(outdir, "/Coef_Model1.csv"), row.names = TRUE)

# Visualization: Difference in Y between EVENT and non-EVENT
df_plot_event_year = df_complete %>%
    mutate(EVENT = factor(EVENT, levels = c(0, 1), labels = c("No Event", "Event"))) %>%
    group_by(EVENT, YEAR) %>%
    summarise(mean_Y = mean(Y, na.rm = TRUE), se_Y = sd(Y, na.rm = TRUE)/sqrt(sum(!is.na(Y)))) %>%
    ungroup()
p_event_year = ggplot(df_plot_event_year, aes(x = YEAR, y = mean_Y, color = EVENT, fill = EVENT)) +
    geom_line(size = 1) +
    geom_ribbon(aes(ymin = mean_Y - 1.96 * se_Y, ymax = mean_Y + 1.96 * se_Y), alpha = 0.2, color = NA) +
    labs(title = "Mean prescription rate (Y) given Event", x = "Year", y = "Mean Y") +
    scale_color_manual(values = c("No Event" = "gray70", "Event" = "steelblue")) +
    scale_fill_manual(values = c("No Event" = "gray70", "Event" = "steelblue")) +
    theme_minimal()
ggsave(filename = file.path(outdir, "Plot_Model1.png"), plot = p_event_year, width = 10, height = 12)

# Model 2: Comparing (average) prescription ratios Before and After Event 
# - adjusting for age in 2023, sex, specialty + age and year of event
df_model = df_complete %>%
    mutate(PERIOD = case_when(
            !is.na(EVENT_MONTH) & MONTH < EVENT_MONTH ~ "BEFORE",
            !is.na(EVENT_MONTH) & MONTH > EVENT_MONTH ~ "AFTER",
            is.na(EVENT_MONTH) ~ NA_character_)) %>% 
    filter(!is.na(PERIOD)) %>%
    mutate(PERIOD = factor(PERIOD, levels = c("BEFORE", "AFTER"))) # set BEFORE as reference
model_formula = as.formula("Y ~ AGE_IN_2023 + AGE_AT_EVENT + SEX + factor(SPECIALTY) + PERIOD")
model = fixest::feols(model_formula, data = df_model, fixef.rm = "none")
results = data.frame(summary(model)$coeftable)
write.csv(results, file = paste0(outdir, "/Coef_Model2.csv"), row.names = TRUE)

# Visualization: Average Y by PERIOD (Before vs After), centered by event month using pre/post dummies
df_event <- df_complete %>%
    mutate(time_from_event = MONTH - EVENT_MONTH) %>%
    filter(time_from_event >= -36 & time_from_event <= 36)
df_event_summary <- df_event %>%
    group_by(time_from_event) %>%
    summarise(mean_Y = mean(Y, na.rm = TRUE),se_Y = sd(Y, na.rm = TRUE) / sqrt(sum(!is.na(Y))),n = sum(!is.na(Y))) %>%
    ungroup()

before_lm <- lm(mean_Y ~ time_from_event, data = df_event_summary %>% filter(time_from_event < 0))
after_lm  <- lm(mean_Y ~ time_from_event, data = df_event_summary %>% filter(time_from_event > 0))
before_df <- df_event_summary %>% filter(time_from_event < 0)
before_df$reg <- predict(before_lm, newdata = before_df)
after_df <- df_event_summary %>% filter(time_from_event > 0)
after_df$reg <- predict(after_lm, newdata = after_df)

p_centered <- ggplot(df_event_summary, aes(x = time_from_event, y = mean_Y)) +
    geom_line(size = 1, color = "steelblue") +
    geom_ribbon(aes(ymin = mean_Y - 1.96 * se_Y, ymax = mean_Y + 1.96 * se_Y), alpha = 0.2, fill = "steelblue") +
    geom_vline(xintercept = 0, linetype = "dashed", color = "red") +
    geom_line(data = before_df, aes(x = time_from_event, y = reg), color = "darkorange", size = 1) +
    geom_line(data = after_df, aes(x = time_from_event, y = reg), color = "darkgreen", size = 1) +
    labs(title = "Average Prescription Ratio (Y) Centered on Event (Pre/Post)",x = "Months from Event", y = "Mean Y") +
    theme_minimal()
ggsave(filename = file.path(outdir, "Plot_Model2.png"), plot = p_centered, width = 10, height = 6)
