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
    years = 1999:2021
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
outcomes = outcomes[outcomes$DOCTOR_ID != outcomes$PATIENT_ID, ] # remove self-prescriptions
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
    mutate(Y = ifelse(N == 0, NA_real_, Ni / N)) %>%
    filter(YEAR >= 1998) #QC step to remove unexpected years

# Prepare events for DiD analysis
events_new = events[, c("PATIENT_ID", "DATE")] %>% rename("DOCTOR_ID" = "PATIENT_ID")
df_merged = left_join(outcomes_new, events_new, by = "DOCTOR_ID")
df_merged = df_merged %>%
    mutate(
        EVENT = if_else(!is.na(DATE), 1, 0),
        EVENT_YEAR = if_else(!is.na(DATE), as.numeric(format(DATE, "%Y")), NA_real_),
    ) %>%
    select(-DATE)

# Prepare and add covariates
covariates = covariates %>%
    rename("DOCTOR_ID" = "FID") %>%
    select(DOCTOR_ID, BIRTH_DATE, SEX, SPECIALTY) %>%
    mutate(BIRTH_YEAR = as.numeric(substr(BIRTH_DATE, 1, 4))) %>% # date format is YYYYMMDD
    select(-BIRTH_DATE)
df_model = merge(df_merged, covariates, by = "DOCTOR_ID", how = "left") %>% as_tibble()
df_model = df_model %>% filter(YEAR - BIRTH_YEAR >= 65) # remove info after doctor retirement

# add specialty labels
df_spec = fread("/media/volume/Projects/DSGELabProject1/condensed_specialty_dict.csv")
df_spec$SPECIALTY = as.character(df_spec$CODEVALUE)
df_model = merge(df_model, df_spec, by = "SPECIALTY", how = "left")
df_model$SPECIALTY = as.factor(df_model$INTERPRETATION)

# check distribution of events over the years 
df_model_unique = df_model %>% distinct(DOCTOR_ID, .keep_all = TRUE)
p1_general = ggplot(df_model_unique, aes(x = factor(EVENT_YEAR))) +
    geom_bar(aes(y = ..count..)) +
    labs(title = "Count of Events Over the Years")
p1_facet = ggplot(df_model_unique, aes(x = factor(EVENT_YEAR))) +
    geom_bar(aes(y = ..count..)) +
    facet_grid(factor(SEX, levels = c(1, 2), labels = c("Male", "Female")) ~ cut(BIRTH_YEAR, breaks = c(-Inf, 1940, 1960, 1980, 2000, Inf), labels = c("Before 1940", "1940-1960", "1960-1980", "1980-2000", "After 2000")), drop = FALSE) +
    labs(title = "Count of Events Over the Years, by Birth Year and Sex") +
    theme(strip.text = element_text(face = "bold"))
combined_plot1 = p1_general / p1_facet
ggsave(filename = file.path(outdir, "distribution_events.png"), plot = combined_plot1, width = 10, height = 12)

# check distribution of statin prescription over the years
p2_general = ggplot(df_model, aes(x = YEAR)) +
    stat_summary(aes(y = Y), fun = mean, geom = "line", size = 1) +
    labs(title = "Count of Outcome Y=Ni/N Over the Years")
p2_facet = ggplot(df_model, aes(x = YEAR)) +
    stat_summary(aes(y = Y), fun = mean, geom = "line", size = 1) +
    facet_wrap(~ EVENT_YEAR) +
    geom_vline(aes(xintercept = EVENT_YEAR), linetype = "dashed", color = "red") +
    labs(title = "Count of Outcome Y=Ni/N Over the Years, by Event Year")
combined_plot2 = p2_general / p2_facet
ggsave(filename = file.path(outdir, "distribution_outcomes.png"), plot = combined_plot2, width = 10, height = 12)

# DiD analysis model
df_model = create_pre_post_dummies(df_model)
dummy_vars = grep("^(PRE|POST)\\d+$", names(df_model), value = TRUE)
interaction_terms = paste(paste0("YEAR*", dummy_vars), collapse = " + ")
model_formula = as.formula(paste("Y ~ BIRTH_YEAR + SEX + factor(SPECIALTY) +", interaction_terms))
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
