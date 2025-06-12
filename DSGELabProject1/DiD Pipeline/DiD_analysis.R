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
outdir = args[8]

COLOR_MALE = "blue"
COLOR_FEMALE = "orange"

# Functions
create_pre_post_dummies = function(data) {
    months = 1:300
    for (i in seq_along(months)) {
        t = i
        pre_name = paste0("PRE", t)
        post_name = paste0("POST", t)
        data[[pre_name]] = ifelse(!is.na(data$EVENT_MONTH) & data$MONTH == data$EVENT_MONTH - t, 1, 0)
        data[[post_name]] = ifelse(!is.na(data$EVENT_MONTH) & data$MONTH == data$EVENT_MONTH + t, 1, 0)
    }
    return(data)
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
        AGE_AT_EVENT = if_else(is.na(EVENT_YEAR), NA_real_, EVENT_YEAR - BIRTH_YEAR)
    )
events_after65 = df_complete %>% filter(AGE_AT_EVENT > 65) %>% pull(DOCTOR_ID) %>% unique()
df_complete = df_complete %>% 
    filter(!(DOCTOR_ID %in% events_after65)) %>% # remove people which experiment the event after pension (age 65)
    filter(MONTH <= 65) # remove all prescriptions done after pension (age 65)

# extract count of specialties 
spec_count = df_complete %>%
    distinct(DOCTOR_ID, SPECIALTY) %>%
    mutate(SPECIALTY = ifelse(SPECIALTY == "", "no specialty", SPECIALTY)) %>%
    count(SPECIALTY, name = "SPECIALTY_COUNT")

# check distribution of events over the years
df_plot = df_complete %>% distinct(DOCTOR_ID, .keep_all = TRUE) %>% na.omit(EVENT_YEAR)
p1_general = ggplot(df_plot, aes(x = factor(EVENT_YEAR))) +
    geom_bar(aes(y = ..count..)) +
    labs(title = paste0("Count of (First) Events Over the Years, N = ",length(unique(df_plot$DOCTOR_ID))), x = "Event Year") 
p1_specialty = df_plot %>%
    count(SPECIALTY, name = "event_count") %>%
    left_join(spec_count, by = "SPECIALTY") %>%
    mutate(rate = event_count / SPECIALTY_COUNT) %>%
    ggplot(aes(x = SPECIALTY, y = rate)) +
    geom_bar(stat = "identity") +
    labs(title = "Event Rate within each Specialty", x = "Specialty", y = "Event Rate") +
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

# check distribution of statin prescription over the years
p2_general = ggplot(df_complete, aes(x = YEAR)) +
    stat_summary(aes(y = Y), fun = mean, geom = "line", size = 1) +
    labs(title = "Entire Doctor Population Prescription Ratio Y=Ni/N Over the Years")
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
combined_plot2 = p2_general / (p2_N + p2_Y)
ggsave(filename = file.path(outdir, "distribution_outcomes.png"), plot = combined_plot2, width = 10, height = 12)

# DiD analysis model
df_model = create_pre_post_dummies(df_model) 
dummy_vars = grep("^(PRE|POST)\\d+$", names(df_model), value = TRUE)
interaction_terms = paste(paste0("YEAR*", dummy_vars), collapse = " + ")
model_formula = as.formula(paste("Y ~ AGE + SEX + factor(SPECIALTY) +", interaction_terms))
model = fixest::feols(model_formula, data = df_model, fixef.rm = "none")
results = data.frame(summary(model)$coeftable)
write.csv(results, file = paste0(outdir, "/DiD_coefficients.csv"), row.names = TRUE)

# Plot DiD results
results$time[grepl("PRE", rownames(results))] = -as.numeric(gsub("PRE", "", rownames(results)[grepl("PRE", rownames(results))]))
results$time[grepl("POST", rownames(results))] = as.numeric(gsub("POST", "", rownames(results)[grepl("POST", rownames(results))]))
results_plot = results %>% filter(!is.na(time) & abs(time) <= 5)
ggplot(results_plot, aes(x = time, y = Estimate)) +
    geom_point() +
    geom_errorbar(aes(ymin = Estimate - 1.96 * Std..Error, ymax = Estimate + 1.96 * Std..Error), width = 0.2) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "red") +
    geom_hline(yintercept = 0, linetype = "solid", color = "black", size = 0.1) +
    scale_x_continuous(breaks = seq(min(results_plot$time, na.rm = TRUE), max(results_plot$time, na.rm = TRUE), by = 1)) +
    theme_minimal() +
    labs(x = "Years from Event", y = "Coefficient")
ggsave(filename = file.path(outdir, "DiD_plot.png"), plot = last_plot(), width = 10, height = 12)