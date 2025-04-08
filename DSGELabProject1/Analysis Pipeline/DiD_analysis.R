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

##### Arguments
args = commandArgs(trailingOnly = TRUE)
doctor_list = args[1]
map_relatives = args[2]
use_relatives = args[3]
id_cases = args[4]
id_controls = args[5]
events_file = args[6]
outcomes_file = args[7]
outcome_code = args[8]
covariates_file = args[9]
outdir = args[10]

# Functions
create_pre_post_dummies = function(data) {
    years = 2015:2021 
    for (i in 1:length(years)) {
        t = i 
        pre_name = paste0("PRE", t)
        post_name = paste0("POST", t)
        data[[pre_name]] = ifelse(!is.na(data$EVENT_YEAR) & data$YEAR == data$EVENT_YEAR - t, 1, 0)
        data[[post_name]] = ifelse(!is.na(data$EVENT_YEAR) & data$YEAR == data$EVENT_YEAR + t, 1, 0)
    }
    return(data)
}

#### Main

# Load data
all_cases = fread(id_cases, header = FALSE)$V1
all_controls = fread(id_controls, header = FALSE)$V1
all_doctors = fread(doctor_list, header = FALSE)$V1
map_relatives = fread(map_relatives, header = TRUE)
events = fread(events_file)
outcomes = fread(outcomes_file)
outcomes = outcomes[outcomes[, .I[which.min(DATE)], by = .(DOCTOR_ID, PATIENT_ID, CODE)]$V1] # use only first date per outcome code
covariates = fread(covariates_file)

# Report number of cases and controls
if (use_relatives == "no") {
    cases = intersect(all_doctors, all_cases)
    controls = intersect(all_doctors, all_controls)
} else if (use_relatives == "yes") {
    extra_cases = map_relatives %>% filter(RELATIVE_FID %in% all_cases) %>% select(DOCTOR_ID) %>% unique()
    cases = union(cases_doctors, extra_cases)
    controls = setdiff(all_doctors, cases_doctors_and_relatives)
}
cat(paste0("Consider event in 1st-degree relatives? ", use_relatives, "\n"))
cat(paste0("cases : ", length(cases), "\n"))
cat(paste0("controls : ", length(controls), "\n"))
id_list = list(cases, controls)

# Prepare outcomes for DiD analysis
outcomes_new = outcomes %>%
    filter(!is.na(CODE) & !is.na(DATE)) %>%
    mutate(YEAR = as.numeric(format(DATE, "%Y"))) %>%
    group_by(DOCTOR_ID, YEAR) %>%
    summarise(
        Ni = sum(grepl(paste0("^", outcome_code), CODE)),
        N = n(),
        .groups = "drop"
    ) %>%
    complete(DOCTOR_ID, YEAR = seq(2014, 2022, by = 1), fill = list(Ni = 0, N = 0)) %>%
    mutate(Y = ifelse(N == 0, NA_real_, Ni / N))

# Report number of doctors with outcome of interest
doctors_with_outcomes = outcomes_new %>% group_by(DOCTOR_ID) %>% summarise(has_outcome = any(Y != 0, na.rm = TRUE)) %>% filter(has_outcome) %>% pull(DOCTOR_ID)
N_CASES_OUT = length(intersect(id_list[[1]], doctors_with_outcomes))
N_CONTROLS_OUT = length(intersect(id_list[[2]], doctors_with_outcomes))
cat(paste0("cases with outcome info (%): ", N_CASES_OUT * 100 / length(id_list[[1]]), "\n"))
cat(paste0("controls with outcome info (%): ", N_CONTROLS_OUT * 100 / length(id_list[[2]]), "\n"))

# Report availability of outcomes
outcome_availability = outcomes_new %>% group_by(YEAR) %>% summarise(available_outcomes = sum(!is.na(Y)), total_doctors = n(), frequency = available_outcomes / total_doctors) %>% complete(YEAR = 2014:2022, fill = list(available_outcomes = 0, total_doctors = 0, frequency = 0))
p1 = ggplot(outcome_availability, aes(x = factor(YEAR), y = frequency)) +
    geom_bar(stat = "identity") +
    theme_minimal() +
    labs(x = "Year", y = "Outcome info availability (%)")
ggsave(paste0(outdir, "/OutcomeAvailability.png"), plot = p1, width = 14, height = 8, dpi = 300)

# Prepare events for DiD analysis
tmp = events[, c("PATIENT_ID", "DATE")] %>% rename("DOCTOR_ID" = "PATIENT_ID")
df_merged = left_join(outcomes_new, tmp, by = "DOCTOR_ID")
df_merged = df_merged %>%
    mutate(
        EVENT = if_else(!is.na(DATE), 1, 0),
        EVENT_YEAR = if_else(!is.na(DATE), as.numeric(format(DATE, "%Y")), NA_real_),
    ) %>%
    filter(is.na(DATE) | as.Date(DATE) >= as.Date("2014-01-01")) %>% # select controls or cases after 2014
    select(-DATE)

# Prepare and add covariates
covariates = covariates %>%
    rename("DOCTOR_ID" = "FID") %>%
    select(DOCTOR_ID, BIRTH_DATE, SEX, SPECIALTY) %>%
    mutate(BIRTH_YEAR = as.numeric(substr(BIRTH_DATE, 1, 4))) %>% # date format is YYYYMMDD
    select(-BIRTH_DATE) 
df_model = merge(df_merged, covariates, by = "DOCTOR_ID", how = "left") %>% as_tibble()

# DiD analysis model
df_model = create_pre_post_dummies(df_model) %>% mutate(
    Y_missing = ifelse(is.na(Y), 1, 0),
    Y_filled = ifelse(is.na(Y), 0, Y))
dummy_vars = grep("^(PRE|POST)\\d+$", names(df_model), value = TRUE)
interaction_terms = paste(paste0("EVENT*", dummy_vars), collapse = " + ")
model_formula = as.formula(paste("Y_filled ~ YEAR + Y_missing + BIRTH_YEAR + SEX + factor(SPECIALTY) +", interaction_terms))
model = fixest::feols(model_formula, data = df_model, fixef.rm = "none")
results = data.frame(summary(model)$coeftable)

# Save model results
write.csv(results, file = paste0(outdir, "/DiD_coefficients.csv"), row.names = TRUE)

# Plot distribution of events:
tmp = tmp[tmp$DATE >= as.Date("2014-01-01"), ]
tmp$YEAR = as.numeric(format(tmp$DATE, "%Y"))
p2 = ggplot(tmp, aes(x = factor(YEAR))) +
    geom_bar() +
    theme_minimal() +
    labs(x = "Year", y = "Number of Events")
ggsave(paste0(outdir, "/EventCounts.png"), plot = p2, width = 14, height = 8, dpi = 300)

# Plot DiD results
results$time[grepl("PRE", rownames(results))] = -as.numeric(gsub("PRE", "", rownames(results)[grepl("PRE", rownames(results))]))
results$time[grepl("POST", rownames(results))] = as.numeric(gsub("POST", "", rownames(results)[grepl("POST", rownames(results))]))
results = results %>% filter(!is.na(time))
p3 = ggplot(results, aes(x = time, y = Estimate)) +
    geom_point() +
    geom_errorbar(aes(ymin = Estimate - 1.96 * Std..Error, ymax = Estimate + 1.96 * Std..Error), width = 0.2) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "red") +
    geom_hline(yintercept = 0, linetype = "solid", color = "black", size = 0.1) +
    scale_x_continuous(breaks = seq(min(results$time, na.rm = TRUE), max(results$time, na.rm = TRUE), by = 1)) +
    theme_minimal() +
    labs(x = "Years from Event", y = "Coefficient")
ggsave(paste0(outdir, "/DiD_plot.png"), plot = p3, width = 14, height = 8, dpi = 300)
