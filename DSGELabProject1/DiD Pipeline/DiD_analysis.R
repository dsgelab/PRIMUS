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
args = commandArgs(trailingOnly=TRUE)
doctor_list = args[1]
events_file = args[2]
event_code = args[3]
outcomes_file = args[4]
outcome_code = args[5]
covariates_file = args[6]
outdir = args[7]

# Global Variables
N_THREADS = 10
COLOR_MALE = "blue" 
COLOR_FEMALE = "orange" 

# Functions
enrichment_func_outcome <- function(s, df) {
    mean_Y_s = df$mean_Y[df$SPECIALTY == s]
    mean_Y_others = mean(df$mean_Y[df$SPECIALTY != s], na.rm = TRUE)
    ifelse(mean_Y_others == 0, NA, mean_Y_s / mean_Y_others)
}

#### Main
setDTthreads(N_THREADS)

# Load data
doctor_ids = fread(doctor_list, header = FALSE)$V1
events = fread(events_file)
events = events[grepl(paste0("^", event_code), CODE)]
event_ids = intersect(unique(events$PATIENT_ID), doctor_ids)

# CHECK 1 : if N of events is less than 500, stop the analysis
cat(paste0("Cases : ", length(event_ids), "\n"))
cat(paste0("Controls : ", length(doctor_ids)-length(event_ids), "\n"))
if (length(event_ids) < 500) {
    stop("Number of events (CHECK 1) is less than 500, SKIP ANALYSIS.")
}

outcomes = fread(outcomes_file)
covariates = fread(covariates_file)

# CHECK 2 : if doctor has less than 20 prescriptions for the outcome of interest, remove doctor from analysis
prescriptions_per_doctor <- outcomes[grepl(paste0("^", outcome_code), CODE), .N, by = DOCTOR_ID]
write.csv(prescriptions_per_doctor, file = file.path(outdir, "Outcomes.csv"), row.names = FALSE)
doctors_to_keep <- prescriptions_per_doctor[N >= 20, DOCTOR_ID]
event_ids <- intersect(intersect(unique(events$PATIENT_ID), doctors_to_keep), doctor_ids)
control_ids <- setdiff(intersect(doctor_ids, doctors_to_keep), event_ids)

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
events = events[PATIENT_ID %in% doctor_ids,] # QC : only selected doctors
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

# exclude events which happened before the first prescription of the outcome / or the last one
n_before = length(unique(df_merged$DOCTOR_ID))
df_merged = as.data.table(df_merged)
df_merged[, `:=`(
    first_Y_month = min(MONTH[!is.na(Y)], na.rm = TRUE),
    last_Y_month = max(MONTH[!is.na(Y)], na.rm = TRUE)
), by = DOCTOR_ID]
df_merged = df_merged[
    is.na(EVENT_MONTH) | (EVENT_MONTH >= first_Y_month & EVENT_MONTH <= last_Y_month)]
removed_ids = unique(df_merged[!(is.na(EVENT_MONTH) | (EVENT_MONTH >= first_Y_month & EVENT_MONTH <= last_Y_month)), DOCTOR_ID])
df_merged = df_merged[!(DOCTOR_ID %in% removed_ids)]
n_after = length(unique(df_merged$DOCTOR_ID))
cat(sprintf("Removed %d doctors with event outside prescription bounds\n", n_before - n_after))

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

# remove doctors with incomplete information about prescriptions (i.e., keep only those with complete monthly data)
months_per_doctor <- df_complete %>%
    group_by(DOCTOR_ID) %>%
    summarise(
        min_month = min(MONTH, na.rm = TRUE),
        max_month = max(MONTH, na.rm = TRUE),
        n_months = n_distinct(MONTH)
    ) %>%
    mutate(expected_months = max_month - min_month + 1) %>%
    filter(n_months == expected_months)
df_complete <- df_complete %>% filter(DOCTOR_ID %in% months_per_doctor$DOCTOR_ID)

# print new number of cases and controls\
n_cases <- df_complete[EVENT == 1, uniqueN(DOCTOR_ID)]
n_controls <- df_complete[EVENT == 0, uniqueN(DOCTOR_ID)]
cat(sprintf("Number of cases: %d\n", n_cases))
cat(sprintf("Number of controls: %d\n", n_controls))

# If N of doctor in specialty is <5 then put in Specialty "Other"
specialty_counts <- df_complete[, .(n = uniqueN(DOCTOR_ID)), by = SPECIALTY]
df_complete <- merge(df_complete, specialty_counts, by = "SPECIALTY", all.x = TRUE)
df_complete[, SPECIALTY := ifelse(n < 5, "Other", SPECIALTY)]
df_complete[, n := NULL]

# print specialties that have been removed
removed_specialties <- specialty_counts[SPECIALTY %in% df_complete$SPECIALTY & n < 5, SPECIALTY]
cat("Removed specialties:", paste(removed_specialties, collapse = ", "), "\n")

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


###############################################################################################
#                                                                                             #
#   =================   DIFFERENCE-IN-DIFFERENCES (DiD) ANALYSIS   =========================  #
#                                                                                             #
#   This section implements the core DiD modeling pipeline.                                   #
#   - It fits a generalized linear model (GLM) to estimate the effect of the event            #
#     on the outcome of interest, adjusting for relevant covariates such as                   #
#     age, sex, specialty, and calendar time.                                                 #
#   - The model includes interaction terms to capture heterogeneous effects                   #
#     across subgroups and time periods. The model is applied only to cases and               #
#     focuses on a 3 year period around the event.                                             #
#   - Model results are exported and visualized, including raw and adjusted                   #
#     prescription ratios centered on the event, as well as interaction effects.              #
#   - Additional advanced DiD analyses (e.g., staggered adoption/event-study)                 #
#     are performed using the 'did' package for robustness and dynamic effect                 #
#     estimation. The model also compares cases vs controls to assess the causal impact of    #
#     the event.                                                                              #
#                                                                                             #
###############################################################################################

# GLM Model: Comparing (average) prescription ratios Before and After Event 
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
write.csv(results, file = paste0(outdir, "/Coef_GLM_Model.csv"), row.names = TRUE)

# PLOT 1: Average prescription ratio (Y) centered on event
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
ggsave(filename = file.path(outdir, "Plot_GLM_Model_adjusted.png"), plot = p_centered_subset, width = 10, height = 12)

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

# Staggered DiD Analysis: 
# using the did package developed by Callaway and Sant'Anna
.libPaths("/shared-directory/sd-tools/apps/R/lib/")
library(did)

# Use data.table for much faster aggregation
df_model <- as.data.table(df_complete)[
    , .(
        Ni = sum(Ni, na.rm = TRUE),
        N = sum(N, na.rm = TRUE),
        Y = if (sum(N, na.rm = TRUE) == 0) NA_real_ else sum(Ni, na.rm = TRUE) / sum(N, na.rm = TRUE),
        AGE_AT_EVENT = tail(AGE_AT_EVENT, 1),
        AGE_IN_2023 = tail(AGE_IN_2023, 1),
        SEX = tail(SEX, 1),
        SPECIALTY = tail(SPECIALTY, 1),
        EVENT = tail(EVENT, 1),
        EVENT_YEAR = tail(EVENT_YEAR, 1),
        EVENT_MONTH = tail(EVENT_MONTH, 1)
    ),
    by = .(DOCTOR_ID, YEAR)
][
    , `:=`(
        SPECIALTY = factor(SPECIALTY, levels = c("", setdiff(unique(df_complete$SPECIALTY), ""))),
        SEX = factor(SEX, levels = c(1, 2), labels = c("Male", "Female"))
    )
]

# Analysis requires only individuals with non-missing Y for all years in the required window
years_required <- (min(df_model$YEAR, na.rm = TRUE)):(max(df_model$YEAR, na.rm = TRUE))
ids_with_all_years <- df_model %>%
    filter(YEAR %in% years_required & !is.na(Y)) %>%
    group_by(DOCTOR_ID) %>%
    summarise(n_years = n_distinct(YEAR)) %>%
    filter(n_years == length(years_required)) %>%
    pull(DOCTOR_ID)

# Step 1: prepare the model data
df_model <- df_model %>% filter(DOCTOR_ID %in% ids_with_all_years, YEAR %in% years_required)
df_model$ID <- as.integer(factor(df_model$DOCTOR_ID))                       # create a numeric ID variable
df_model$G <- ifelse(is.na(df_model$EVENT_YEAR), 0, df_model$EVENT_YEAR)  # G = group of first treatment year, 0 for never-treated
df_model$T <- df_model$YEAR                                                # T = time variable

# Step 2: estimate DiD 
# ATT adjusted for age, sex and specialty (calendar time and age at event will be automatically handled by the model)
att_gt_res <- att_gt(
    yname = "Y",
    tname = "T",
    idname = "ID",
    gname = "G",
    xformla = ~ AGE_IN_2023 + SEX + SPECIALTY,
    data = df_model,
    est_method = "dr",                  # doubly robust (for covariate adj.)
    control_group = "nevertreated",     # use never treated as control group
    clustervars = "ID",
    pl = TRUE,                           # parallel processing
    cores = N_THREADS
)

# Step 3: aggregate and plot

# ---- Dynamic effects (event-study) ----
agg_dynamic <- aggte(att_gt_res, type = "dynamic", na.rm = TRUE)
df_dynamic <- data.frame(
    time = agg_dynamic$egt,
    att = agg_dynamic$att.egt,
    se = agg_dynamic$se.egt
)
# Focus on 3 years before and after the event
df_dynamic_cut <- df_dynamic %>% filter(time >= -3 & time <= 3)
p_dynamic <- ggplot(df_dynamic_cut, aes(x = time, y = att)) +
        geom_line(color = "#1f77b4") +
        geom_point() +
        geom_errorbar(aes(ymin = att - 1.96 * se, ymax = att + 1.96 * se), width = 0.2, color = "#1f77b4") +
        geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
        labs(title = "Dynamic ATT (Event Study)",
                         x = "Years from Event",
                         y = "ATT (log-odds/probability)") +
        theme_minimal()

# ---- Calendar time ATT ----
agg_calendar <- aggte(att_gt_res, type = "calendar", na.rm = TRUE)
df_calendar <- data.frame(
    time = agg_calendar$egt,
    att = agg_calendar$att.egt,
    se = agg_calendar$se.egt
)
p_calendar <- ggplot(df_calendar, aes(x = time, y = att)) +
    geom_line(color = "#2ca02c") +
    geom_point() +
    geom_errorbar(aes(ymin = att - 1.96 * se, ymax = att + 1.96 * se),width = 0.2, color = "#2ca02c") +
    geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
    labs(title = "Calendar Time ATT",
             x = "Calendar Time",
             y = "ATT (log-odds/probability)") +
    theme_minimal()

# ---- Combine into 2x2 panel ----
p = (p_dynamic / p_calendar)
ggsave(filename = file.path(outdir, "did_ATT_plots.png"), plot = p, width = 10, height = 8)
