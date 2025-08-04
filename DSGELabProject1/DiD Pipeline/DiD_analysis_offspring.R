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
offspring_file = '/media/volume/Projects/DSGELabProject1/doctors_and_relative_20250521.csv'

COLOR_MALE = "blue"
COLOR_FEMALE = "orange"

# Functions
enrichment_func_outcome <- function(s, df) {
    mean_Y_s = df$mean_Y[df$SPECIALTY == s]
    mean_Y_others = mean(df$mean_Y[df$SPECIALTY != s], na.rm = TRUE)
    ifelse(mean_Y_others == 0, NA, mean_Y_s / mean_Y_others)
}

#### Main
setDTthreads(0)

# Load data
doctor_ids = fread(doctor_list, header = FALSE)$V1
offspring_data = fread(offspring_file)
offspring_data = offspring_data[offspring_data$DOCTOR_ID %in% doctor_ids, ] # filter offspring data to only include doctors in our cohort
offspring_ids = offspring_data$RELATIVE_ID

events = fread(events_file)
event_ids_offspring = intersect(unique(events$PATIENT_ID), offspring_ids)
event_ids_doctors = unique(offspring_data[offspring_data$RELATIVE_ID %in% event_ids_offspring,]$DOCTOR_ID)

# CHECK 1 : if N of events is less than 500, stop the analysis
cat(paste0("Cases : ", length(event_ids_doctors), "\n"))
cat(paste0("Controls : ", length(doctor_ids)-length(event_ids_doctors), "\n"))
if (length(event_ids_doctors) < 500) {
    stop("Number of events (CHECK 1) is less than 500, SKIP ANALYSIS.")
}

outcomes = fread(outcomes_file)
covariates = fread(covariates_file)

# CHECK 2 : if doctor has less than 20 prescriptions for the outcome of interest, remove doctor from analysis
prescriptions_per_doctor <- outcomes[grepl(paste0("^", outcome_code), CODE), .N, by = DOCTOR_ID]
write.csv(prescriptions_per_doctor, file = file.path(outdir, "Outcomes.csv"), row.names = FALSE)
doctors_to_keep <- prescriptions_per_doctor[N >= 20, DOCTOR_ID]
event_ids <- intersect(event_ids_doctors, doctors_to_keep)
control_ids <- setdiff(setdiff(doctor_ids, event_ids_doctors), setdiff(doctor_ids, doctors_to_keep))

# CHECK 1 (again) : if N of events is less than 500, stop the analysis
cat(paste0("Cases, with at least 20 prescriptions of outcome: ", length(event_ids), "\n"))
cat(paste0("Controls, with at least 20 prescriptions of outcome: ", length(control_ids), "\n"))
if (length(event_ids) < 500) {
    stop("Number of events (post CHECK 2) is less than 500, SKIP ANALYSIS.")
}
doctor_ids = c(event_ids, control_ids) 

# prepare outcomes for DiD analysis
outcomes = outcomes[outcomes$DOCTOR_ID != outcomes$PATIENT_ID, ] # remove self-prescriptions
outcomes = outcomes[DOCTOR_ID %in% doctor_ids,] # QC : only selected doctors
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
events = events[PATIENT_ID %in% event_ids_offspring,] 
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
    labs(title = paste0("Count of (First) Events in Offspring Over the Years, N = ",length(unique(df_plot$DOCTOR_ID))), x = "Event Year") 
specialty_enrichment = df_complete %>%
    group_by(DOCTOR_ID) %>% slice_tail(n = 1) %>% ungroup() %>% # use only one row per ID, last one
    group_by(SPECIALTY) %>%
    summarise(event_count = sum(EVENT, na.rm = TRUE),total_count = n(),freq = event_count / total_count) %>%
    mutate(enrichment = freq / sapply(SPECIALTY, function(s) {mean(freq[SPECIALTY != s], na.rm = TRUE)}), enrichment = round(enrichment, 2)) %>%
    arrange(desc(enrichment))
p1_specialty = ggplot(specialty_enrichment, aes(x = SPECIALTY, y = enrichment)) +
    geom_bar(stat = "identity") +
    labs(title = "Enrichment of Event in Offspring across Specialties", x = "Specialty", y = "Enrichment Ratio") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
p1_age = ggplot(df_plot, aes(x = AGE_AT_EVENT, fill = factor(SEX))) +
    geom_density(alpha = 0.5, show.legend = FALSE) +
    scale_fill_manual(values = c("1" = COLOR_MALE, "2" = COLOR_FEMALE)) +
    facet_grid(~ factor(SEX, levels = c(1,2), labels = c(paste0("Male (n=", sum(df_plot$SEX == 1, na.rm = TRUE), ")"),paste0("Female (n=", sum(df_plot$SEX == 2, na.rm = TRUE), ")"))), drop = FALSE) +
    labs(title = "Distribution of Age at First Event in Offspring by Sex", x = "Age") +
    theme_minimal()
combined_plot1 = p1_general / p1_specialty / p1_age
ggsave(filename = file.path(outdir, "distribution_events.png"), plot = combined_plot1, width = 10, height = 12)

# check distribution of outcome prescription over the years
p2_general = ggplot(df_complete, aes(x = YEAR)) +
    stat_summary(aes(y = Y), fun = mean, geom = "line", size = 1) +
    labs(title = "General Doctor Population Prescription Ratio Y=Ni/N Over the Years")
specialty_enrichment_outcome = df_complete %>%
    group_by(SPECIALTY) %>%
    summarise(mean_Y = mean(Y, na.rm = TRUE),n = n()) %>%
    data.frame() %>% mutate(enrichment = sapply(SPECIALTY, enrichment_func_outcome, df = .)) %>%
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

# check quantiles of N and Y around the event (apply Bayesian adjustment to Y)
df_plot = df_complete %>% mutate(time = MONTH - EVENT_MONTH, Y_adj = ifelse(!is.na(N),(Ni+mean(df_complete$Y))/(N+1),Y)) %>%
    filter(!is.na(time)) %>%
    filter(time >= -36 & time <= 36) # filter to 3 years before and after event
percentiles_N = df_plot %>%
    group_by(time = factor(time)) %>%
    summarise(
        p25 = quantile(N, 0.25, na.rm = TRUE),
        p50 = quantile(N, 0.5, na.rm = TRUE),
        p75 = quantile(N, 0.75, na.rm = TRUE))
percentiles_Y = df_plot %>%
    group_by(time = factor(time)) %>%
    summarise(
        p25 = quantile(Y_adj, 0.25, na.rm = TRUE),
        p50 = quantile(Y_adj, 0.5, na.rm = TRUE),
        p75 = quantile(Y_adj, 0.75, na.rm = TRUE))
p2_N = ggplot(percentiles_N, aes(x = time, group = 1)) +
    geom_line(aes(y = p25), color = "gray40", linetype = "dashed") +
    geom_line(aes(y = p50), color = "black", size = 1) +
    geom_line(aes(y = p75), color = "gray40", linetype = "dashed") +
    geom_vline(xintercept = which(levels(percentiles_N$time) == "0"), linetype = "dashed", color = "red") +
    labs(title = paste0("Number of Prescriptions N (quartiles),\nFocus on ±3 years for cases who prescribed = ", length(unique(df_plot$DOCTOR_ID))),x = "Months from Event", y = "N") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 8))
p2_Y = ggplot(percentiles_Y, aes(x = time, group = 1)) +
    geom_line(aes(y = p25), color = "gray40", linetype = "dashed") +
    geom_line(aes(y = p50), color = "black", size = 1) +
    geom_line(aes(y = p75), color = "gray40", linetype = "dashed") +
    geom_vline(xintercept = which(levels(percentiles_Y$time) == "0"), linetype = "dashed", color = "red") +
    labs(title = paste0("Population Adjusted Prescription Ratio Y (quartiles),\nFocus on ±3 years for cases who prescribed= ", length(unique(df_plot$DOCTOR_ID))), x = "Months from Event", y = "Population Adjusted Y") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 8))
combined_plot2 = p2_N / p2_Y
ggsave(filename = file.path(outdir, "quintiles_outcomes.png"), plot = combined_plot2, width = 10, height = 12)

# DiD analysis model

# Model 1: Comparing prescription ratios in Events vs Non-Events
# - adjusting analysis for age, sex and specialty
# - adding interaction with age and year of event
model_formula = as.formula("Y ~ AGE_IN_2023 + SEX + factor(SPECIALTY) + EVENT")
model = fixest::feols(model_formula, data = df_complete, fixef.rm = "none")
results = data.frame(summary(model)$coeftable)
write.csv(results, file = paste0(outdir, "/Coef_Model1.csv"), row.names = TRUE)

# Visualization: Difference in Y between EVENT and non-EVENT 
ref_age <- df_complete %>% count(AGE_IN_2023) %>% arrange(desc(n)) %>% slice(1) %>% pull(AGE_IN_2023)
ref_sex <- df_complete %>% count(SEX) %>% arrange(desc(n)) %>% slice(1) %>% pull(SEX)
ref_specialty <- df_complete %>% count(SPECIALTY) %>% arrange(desc(n)) %>% slice(1) %>% pull(SPECIALTY)
df_plot_event_ref = df_complete %>%
    filter(AGE_IN_2023 == ref_age, SEX == ref_sex, SPECIALTY == ref_specialty) %>%
    mutate(EVENT = factor(EVENT, levels = c(0, 1), labels = c("No Event", "Event"))) %>%
    group_by(EVENT, YEAR) %>%
    summarise(mean_Y = mean(Y, na.rm = TRUE), se_Y = sd(Y, na.rm = TRUE)/sqrt(sum(!is.na(Y)))) %>%
    ungroup()
# Plot for reference group: most common age, sex, and specialty in the dataset
p_event_year = ggplot(df_plot_event_ref, aes(x = YEAR, y = mean_Y, color = EVENT, fill = EVENT)) +
    geom_line(size = 1) +
    geom_ribbon(aes(ymin = mean_Y - 1.96 * se_Y, ymax = mean_Y + 1.96 * se_Y), alpha = 0.2, color = NA) +
    labs(title = paste0("Mean prescription rate (Y) given Event\n","Reference: Age (in 2023) = ", ref_age, ", Sex (1:Male, 2:Female) = ", ref_sex, ", Specialty = ", ref_specialty),x = "Year", y = "Mean Y") +
    scale_color_manual(values = c("No Event" = "gray70", "Event" = "steelblue")) +
    scale_fill_manual(values = c("No Event" = "gray70", "Event" = "steelblue")) +
    theme_minimal()
ggsave(filename = file.path(outdir, "Plot_Model1.png"), plot = p_event_year, width = 10, height = 12)

# Model 2: Comparing (average) prescription ratios Before and After Event 
# - adjusting for age in 2023, sex, specialty + age and year of event
df_model = df_complete %>%
    mutate(
        PERIOD = case_when(
            !is.na(EVENT_MONTH) & MONTH < EVENT_MONTH ~ "BEFORE",
            !is.na(EVENT_MONTH) & MONTH > EVENT_MONTH ~ "AFTER",
            is.na(EVENT_MONTH) ~ NA_character_),
        time_from_event = MONTH - EVENT_MONTH
    ) %>%
    filter(!is.na(PERIOD), time_from_event >= -36, time_from_event <= 36) %>%
    mutate(
        PERIOD = factor(PERIOD, levels = c("BEFORE", "AFTER")), # set BEFORE as reference
        SPECIALTY = factor(SPECIALTY, levels = c("", setdiff(unique(df_complete$SPECIALTY), ""))), # set no specialty as reference
        SEX = factor(SEX, levels = c(1, 2), labels = c("Male", "Female")) # set male as reference
    )
model_formula = as.formula("Y ~ PERIOD + MONTH + MONTH**2 + AGE_AT_EVENT + AGE_AT_EVENT**2 + AGE_IN_2023 + AGE_IN_2023**2 + SEX + SPECIALTY + AGE_AT_EVENT:PERIOD + AGE_IN_2023:PERIOD + SEX:PERIOD + SPECIALTY:PERIOD")
model = fixest::feglm(model_formula, family = binomial("logit"), data = df_model, cluster = ~DOCTOR_ID)
results = data.frame(summary(model)$coeftable)
write.csv(results, file = paste0(outdir, "/Coef_Model2.csv"), row.names = TRUE)

# PLOT 1: Average prescription ratio (Y) centered on event
# - sets of averages: non-adjusted (overall) and adjusted (by age,sex & specialty based on the model)
# - focus on +/- 36 months around the event month
df_centered <- df_model %>%
    mutate(time_from_event = MONTH - EVENT_MONTH) %>%
    filter(time_from_event >= -36 & time_from_event <= 36)
avg_Y_data <- df_centered %>% 
    group_by(PERIOD) %>% 
    summarise(mean_Y = mean(Y, na.rm = TRUE), .groups = 'drop')
avg_Y_model <- df_centered %>%
    mutate(predicted_Y = predict(model, newdata = .)) %>%
    group_by(PERIOD) %>%
    summarise(mean_Y = mean(predicted_Y, na.rm = TRUE),se_Y = sd(predicted_Y, na.rm = TRUE) / sqrt(n()),.groups = 'drop')
plot_data <- df_centered %>%
    mutate(predicted_Y = predict(model, newdata = .)) %>%
    group_by(time_from_event) %>%
    summarise(
        raw_mean_Y = mean(Y, na.rm = TRUE),
        raw_se_Y = sd(Y, na.rm = TRUE) / sqrt(n()),
        model_mean_Y = mean(predicted_Y, na.rm = TRUE),
        model_se_Y = sd(predicted_Y, na.rm = TRUE) / sqrt(n()),
        .groups = 'drop')

p_centered_subset <- ggplot(plot_data, aes(x = time_from_event)) +
    geom_ribbon(aes(ymin = model_mean_Y - 1.96 * model_se_Y, ymax = model_mean_Y + 1.96 * model_se_Y), alpha = 0.1, fill = "steelblue") +
    geom_ribbon(aes(ymin = raw_mean_Y - 1.96 * raw_se_Y, ymax = raw_mean_Y + 1.96 * raw_se_Y), alpha = 0.1, fill = "orange") +
    geom_line(aes(y = model_mean_Y), size = 1, color = "steelblue", alpha = 1) +
    geom_line(aes(y = raw_mean_Y), size = 1, color = "orange", alpha = 0.5) +
    geom_point(aes(y = model_mean_Y), size = 0.8, color = "steelblue", alpha = 1) +
    geom_point(aes(y = raw_mean_Y), size = 0.8, color = "orange", alpha = 0.5) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "red") +
    scale_x_continuous(breaks = seq(-36, 36, 12), labels = seq(-36, 36, 12), limits = c(-36, 36)) +
    labs(
        x = "Months from Event",
        y = "Average Prescription Ratio (Y)",
        title = "Average Prescription Ratio Before and After Event",
        subtitle = "Blue: Model-adjusted estimates, Orange: Raw data"
    ) +
    theme_minimal()
ggsave(filename = file.path(outdir, "Plot_Model2_adjusted.png"), plot = p_centered_subset, width = 10, height = 12)

# PLOT 2: Interaction Effects
source("/media/volume/Projects/DSGELabProject1/DiD_Pipeline/PlotInteractionEffects.R")
plots <- create_model_visualization(model, df_model, outdir)

ggsave(filename = file.path(outdir, "Model_Results_Comprehensive.png"), plot = plots$combined, width = 16, height = 12, dpi = 300)
ggsave(filename = file.path(outdir, "Specialty_Baseline_Differences.png"), plot = plots$baseline, width = 8, height = 6)
ggsave(filename = file.path(outdir, "Specialty_Interactions.png"), plot = plots$period, width = 8, height = 6)
ggsave(filename = file.path(outdir, "Age_Sex_Baseline.png"), plot = plots$age_sex_baseline, width = 8, height = 6)
ggsave(filename = file.path(outdir, "Age_Sex_Interactions.png"), plot = plots$age_sex_interactions, width = 8, height = 6)

# Export summary of results
.libPaths("/shared-directory/sd-tools/apps/R/lib/")
library(marginaleffects)

options(marginaleffects_parallel = TRUE)
marginal = avg_slopes(model, variables = "PERIOD")

# Save results
effect_size = marginal$estimate
p_value = marginal$p.value
ci_lower = marginal$conf.low
ci_upper = marginal$conf.high
n_cases = df_complete %>% filter(EVENT == 1) %>% pull(DOCTOR_ID) %>% unique() %>% length()
n_controls = df_complete %>% filter(EVENT == 0) %>% pull(DOCTOR_ID) %>% unique() %>% length()
results_summary = data.frame(
    effect_size = effect_size,
    p_value = p_value,
    ci_lower = ci_lower,
    ci_upper = ci_upper,
    n_cases = n_cases,
    n_controls = n_controls
)
write.csv(results_summary, file = paste0(outdir, "/results_summary.csv"), row.names = FALSE)