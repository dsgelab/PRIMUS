# ------------------------------------------------
#### Libraries
# ------------------------------------------------
.libPaths("/shared-directory/sd-tools/apps/R/lib/")
suppressPackageStartupMessages({
    library(data.table)
    library(dplyr)
    library(tidyr)
    library(lubridate)
    library(ggplot2)
    library(patchwork)
    library(arrow)
    library(stringr)
    library(did)
})

# ------------------------------------------------
##### File Paths and Global Variables
# ------------------------------------------------

doctor_list = "/media/volume/Projects/DSGELabProject1/doctors_20250424.csv"
pregnancies_file = "/media/volume/Projects/DSGELabProject1/ProcessedData/AllPregnanciesEvents_20251016.csv"
relatives_file = "/media/volume/Projects/DSGELabProject1/doctors_and_relative_20250521.csv"
outcomes_file = "/media/volume/Projects/DSGELabProject1/DiD_Experiments/Version2_Highthroughput_drop/ProcessedOutcomes_20251014/processed_outcomes.parquet"
covariates_file = "/media/volume/Projects/DSGELabProject1/doctor_characteristics_20250520.csv"

DATE <- format(Sys.time(), "%Y%m%d")
outdir = "/media/volume/Projects/DSGELabProject1/Plots/Supplements/"
if (!dir.exists(outdir)) {dir.create(outdir, recursive = TRUE)}

N_THREADS = 10
setDTthreads(N_THREADS) 

EVENT_CODE = "O80|O81|O82|O83|O84"

# ------------------------------------------------
##### Data Loading and Preprocessing
# ------------------------------------------------

# Load data
doctor_ids = fread(doctor_list, header = FALSE)$V1
outcomes = as.data.table(read_parquet(outcomes_file))
covariates = fread(covariates_file)
events = fread(pregnancies_file) 
relatives = fread(relatives_file)

# filter spouses of doctors
spouse_ids = relatives %>% filter(RELATIVE_TYPE == "SPOUSE") %>% pull(RELATIVE_ID) %>% unique()

# Filter events based on the event code
# Filter using regex (EVENT_CODE can be a regex like "AA|BB|CC" to match multiple codes)
pattern <- paste0("^(", EVENT_CODE, ")")
events <- events[grepl(pattern, as.character(ICD10_CODE), perl = TRUE), .(PATIENT_ID, ICD10_CODE, VISIT_DATE)]
events$VISIT_DATE <- as.Date(events$VISIT_DATE)
events = events[events[, .I[which.min(VISIT_DATE)], by = .(PATIENT_ID, ICD10_CODE)]$V1] # only use first event
events = events %>% 
    rename(
        # new = old
        EVENT_DATE = VISIT_DATE,
        EVENT_CODE = ICD10_CODE
    )

# Summary statistics:
# 1 Total number of pregnancies
unique_patient_ids <- length(unique(events$PATIENT_ID))
# 2. Number of IDs from doctor list with an event code
doctors_ids_with_event <- events %>%
    filter(PATIENT_ID %in% doctor_ids) %>%
    pull(PATIENT_ID) %>%
    unique()
# 3. Number of IDs from doctor spouse list with an event code
spouse_ids_with_event <- events %>%
    filter(PATIENT_ID %in% spouse_ids) %>%
    pull(PATIENT_ID) %>%
    unique()

# Generate list of IDS in the two groups
pregnancy_females = doctors_ids_with_event
pregnancy_males = relatives %>% 
    filter(RELATIVE_TYPE == "SPOUSE") %>%
    filter(RELATIVE_ID %in% spouse_ids_with_event) %>%
    pull(DOCTOR_ID) %>%
    unique()
pregnancy_all = c(pregnancy_females, pregnancy_males)

# extract spouse event years for later use
spouse_events <- relatives %>%
    filter(RELATIVE_TYPE == "SPOUSE") %>%
    select(DOCTOR_ID, RELATIVE_ID) %>%
    inner_join(events %>% select(PATIENT_ID, EVENT_DATE), by = c("RELATIVE_ID" = "PATIENT_ID")) %>%
    mutate(SPOUSE_EVENT_YEAR = as.numeric(format(EVENT_DATE, "%Y"))) %>%
    select(DOCTOR_ID, SPOUSE_EVENT_YEAR) %>%
    distinct()

# Merge events with outcomes
events = events %>% filter(PATIENT_ID %in% pregnancy_all) %>% rename(DOCTOR_ID = PATIENT_ID, DATE = EVENT_DATE)
df_merged = left_join(outcomes, events, by = "DOCTOR_ID")
df_merged = df_merged %>%
    mutate(
        EVENT = if_else(!is.na(DATE), 1, 0),
        EVENT_YEAR = if_else(!is.na(DATE), as.numeric(format(DATE, "%Y")), NA_real_),
    ) %>%
    select(-DATE)

# Prepare  covariates and specialty + merge them in the main dataframe
covariates_new = covariates %>%
    select(DOCTOR_ID, BIRTH_DATE, SEX, INTERPRETATION) %>%
    mutate(SPECIALTY = as.character(INTERPRETATION)) %>% #currently using interpretation of longest specialty
    mutate(BIRTH_YEAR = as.numeric(substr(BIRTH_DATE, 1, 4))) %>% # date format is YYYY-MM-DD
    select(-BIRTH_DATE, -INTERPRETATION)
df_complete = merge(df_merged, covariates_new, by = "DOCTOR_ID", how = "left")
df_complete = df_complete %>% 
    mutate(
        AGE = YEAR - BIRTH_YEAR,
        AGE_IN_2023 = 2023 - BIRTH_YEAR,
        AGE_AT_EVENT = if_else(is.na(EVENT_YEAR), NA_real_, EVENT_YEAR - BIRTH_YEAR)
    )
events_after60 = df_complete %>% filter(AGE_AT_EVENT > 60) %>% pull(DOCTOR_ID) %>% unique()
df_complete = df_complete %>% 
    filter(!(DOCTOR_ID %in% events_after60)) %>% # remove people which experiment the event after pension (age 60)
    filter(AGE <= 60) # remove all prescriptions done after pension (age 60)

df_model = df_complete %>%
    mutate(
        PERIOD = case_when(
            !is.na(EVENT_YEAR) & YEAR < EVENT_YEAR ~ "BEFORE",
            !is.na(EVENT_YEAR) & YEAR > EVENT_YEAR ~ "AFTER",
            is.na(EVENT_YEAR) ~ NA_character_),
        time = YEAR - EVENT_YEAR
    ) %>%
    mutate(
        PERIOD = factor(PERIOD, levels = c("BEFORE", "AFTER")), # set BEFORE as reference
        SPECIALTY = factor(SPECIALTY, levels = c("", setdiff(unique(df_complete$SPECIALTY), ""))), # set no specialty as reference
        SEX = factor(SEX, levels = c(1, 2), labels = c("Male", "Female")) # set male as reference
    )

# Replace missing N values with 0s 
df_model[is.na(N), N := 0]

# Step 1: prepare the model data
df_model$ID <- as.integer(factor(df_model$DOCTOR_ID))                       # create a numeric ID variable
df_model$G <- ifelse(is.na(df_model$EVENT_YEAR), 0, df_model$EVENT_YEAR)  # G = group of first treatment year, 0 for never-treated
df_model$T <- df_model$YEAR    


# ---------------------------------------------------------------------------
# Split analysis for males and females
# ---------------------------------------------------------------------------

# Females
# keep female doctors & ensure cases are limited to pregnancy_females list
df_model_female <- df_model %>%
    filter(SEX == "Female") %>%
    filter(EVENT == 0 | (EVENT == 1 & DOCTOR_ID %in% pregnancy_females))

n_cases_female <- df_model_female %>% filter(EVENT == 1) %>% pull(DOCTOR_ID) %>% unique() %>% length()
n_controls_female <- df_model_female %>% filter(EVENT == 0) %>% pull(DOCTOR_ID) %>% unique() %>% length()
events_per_year_female <- df_model_female[df_model_female$EVENT == 1, .(N = uniqueN(DOCTOR_ID)), by = EVENT_YEAR][order(EVENT_YEAR)]
events_year_str_female <- paste0(events_per_year_female$EVENT_YEAR, ":", events_per_year_female$N, collapse = ", ")

att_gt_res_female <- att_gt(
    yname = "N",
    tname = "T",
    idname = "ID",
    gname = "G",
    xformla = ~ BIRTH_YEAR + SPECIALTY,
    data = df_model_female,
    est_method = "dr",
    control_group = "notyettreated",
    clustervars = "ID",
    pl = TRUE,
    cores = N_THREADS
)

agg_dynamic_female <- aggte(att_gt_res_female, type = "dynamic", na.rm = TRUE)
results_female <- data.frame(
    time = agg_dynamic_female$egt,
    att = agg_dynamic_female$att.egt,
    se = agg_dynamic_female$se.egt
)

data_plot <- results_female %>% filter(time >= -5 & time <= 5)
p_dynamic_female <- ggplot(data_plot, aes(x = time, y = att)) +
    geom_line(color = "#1f77b4") +
    geom_point() +
    geom_errorbar(aes(ymin = att - 1.96 * se, ymax = att + 1.96 * se), width = 0.2, color = "#1f77b4") +
    geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
    labs(
        title = expression(bold(A)~". Effect of Pregnancy on Overall Prescriptions - Female Doctors"),
        subtitle = paste0("Cases: ", n_cases_female, ", Controls: ", n_controls_female),
        x = "Years from Event",
        y = "Change in total number of prescriptions"
    ) +
    scale_x_continuous(breaks = -5:5) +
    theme_minimal()

# Males
df_model_male <- df_model %>%
    filter(SEX == "Male") %>%
    left_join(spouse_events, by = "DOCTOR_ID") %>%
    mutate(
        EVENT = if_else(!is.na(SPOUSE_EVENT_YEAR), 1, 0),
        EVENT_YEAR = if_else(!is.na(SPOUSE_EVENT_YEAR), as.numeric(SPOUSE_EVENT_YEAR), EVENT_YEAR),
        G = ifelse(is.na(EVENT_YEAR), 0, EVENT_YEAR)
    ) %>%
    select(-SPOUSE_EVENT_YEAR)

n_cases_male <- df_model_male %>% filter(EVENT == 1) %>% pull(DOCTOR_ID) %>% unique() %>% length()
n_controls_male <- df_model_male %>% filter(EVENT == 0) %>% pull(DOCTOR_ID) %>% unique() %>% length()
events_per_year_male <- df_model_male[df_model_male$EVENT == 1, .(N = uniqueN(DOCTOR_ID)), by = EVENT_YEAR][order(EVENT_YEAR)]
events_year_str_male <- paste0(events_per_year_male$EVENT_YEAR, ":", events_per_year_male$N, collapse = ", ")

att_gt_res_male <- att_gt(
    yname = "N",
    tname = "T",
    idname = "ID",
    gname = "G",
    xformla = ~ BIRTH_YEAR + SPECIALTY,
    data = df_model_male,
    est_method = "dr",
    control_group = "notyettreated",
    clustervars = "ID",
    pl = TRUE,
    cores = N_THREADS
)

agg_dynamic_male <- aggte(att_gt_res_male, type = "dynamic", na.rm = TRUE)
results_male <- data.frame(
    time = agg_dynamic_male$egt,
    att = agg_dynamic_male$att.egt,
    se = agg_dynamic_male$se.egt
)

data_plot <- results_male %>% filter(time >= -5 & time <= 5)
p_dynamic_male <- ggplot(data_plot, aes(x = time, y = att)) +
    geom_line(color = "#1f77b4") +
    geom_point() +
    geom_errorbar(aes(ymin = att - 1.96 * se, ymax = att + 1.96 * se), width = 0.2, color = "#1f77b4") +
    geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
    labs(
        title = expression(bold(B)~". Effect of Pregnancy on Overall Prescriptions - Male Doctors (with pregnant spouses)"),
        subtitle = paste0("Cases: ", n_cases_male, ", Controls: ", n_controls_male),
        x = "Years from Event",
        y = "Change in total number of prescriptions"
    ) +
    scale_x_continuous(breaks = -5:5) +
    theme_minimal()

# Join plots and save
p_combined <- p_dynamic_female + p_dynamic_male + plot_layout(ncol = 1)

ggsave(
    filename = file.path(outdir, paste0("SupplementaryFigure_Pregnancy_", DATE, ".png")),
    plot = p_combined,
    width = 10, 
    height = 8, 
    dpi = 300
)

# ---------------------------------------------------------------------------
# Month zoom in for males
# ---------------------------------------------------------------------------

doctor_list = "/media/volume/Projects/DSGELabProject1/doctors_20250424.csv"
pregnancies_file = "/media/volume/Projects/DSGELabProject1/ProcessedData/AllPregnanciesEvents_20251016.csv"
relatives_file = "/media/volume/Projects/DSGELabProject1/doctors_and_relative_20250521.csv"
outcomes_file = "/media/volume/Projects/DSGELabProject1/DiD_Experiments/Version1_Highthroughput_drop/ProcessedOutcomes_20250926/processed_outcomes.parquet"
covariates_file = "/media/volume/Projects/DSGELabProject1/doctor_characteristics_20250520.csv"

# Load data
doctor_ids = fread(doctor_list, header = FALSE)$V1
outcomes = as.data.table(read_parquet(outcomes_file))
covariates = fread(covariates_file)
events = fread(pregnancies_file) 
relatives = fread(relatives_file)

# filter spouses of doctors
spouse_ids = relatives %>% filter(RELATIVE_TYPE == "SPOUSE") %>% pull(RELATIVE_ID) %>% unique()

# Filter events based on the event code
pattern <- paste0("^(", EVENT_CODE, ")")
events <- events[grepl(pattern, as.character(ICD10_CODE), perl = TRUE), .(PATIENT_ID, ICD10_CODE, VISIT_DATE)]
events$VISIT_DATE <- as.Date(events$VISIT_DATE)
events = events[events[, .I[which.min(VISIT_DATE)], by = .(PATIENT_ID, ICD10_CODE)]$V1] # only use first event
events = events %>% 
    rename(
        # new = old
        EVENT_DATE = VISIT_DATE,
        EVENT_CODE = ICD10_CODE
    )

# Summary statistics:
# 1 Total number of pregnancies
unique_patient_ids <- length(unique(events$PATIENT_ID))
# 2. Number of IDs from doctor list with an event code
doctors_ids_with_event <- events %>%
    filter(PATIENT_ID %in% doctor_ids) %>%
    pull(PATIENT_ID) %>%
    unique()
# 3. Number of IDs from doctor spouse list with an event code
spouse_ids_with_event <- events %>%
    filter(PATIENT_ID %in% spouse_ids) %>%
    pull(PATIENT_ID) %>%
    unique()

# Generate list of IDS in the two groups
pregnancy_females = doctors_ids_with_event
pregnancy_males = relatives %>% 
    filter(RELATIVE_TYPE == "SPOUSE") %>%
    filter(RELATIVE_ID %in% spouse_ids_with_event) %>%
    pull(DOCTOR_ID) %>%
    unique()
pregnancy_all = c(pregnancy_females, pregnancy_males)

# extract spouse event years for later use
spouse_events <- relatives %>%
    filter(RELATIVE_TYPE == "SPOUSE") %>%
    select(DOCTOR_ID, RELATIVE_ID) %>%
    inner_join(events %>% select(PATIENT_ID, EVENT_DATE), by = c("RELATIVE_ID" = "PATIENT_ID")) %>%
    rename(SPOUSE_EVENT_DATE = EVENT_DATE) %>%
    select(DOCTOR_ID, SPOUSE_EVENT_DATE) %>%
    distinct()

# Merge events with outcomes
events = events %>% filter(PATIENT_ID %in% pregnancy_all) %>% rename(DOCTOR_ID = PATIENT_ID, DATE = EVENT_DATE)
df_merged = left_join(outcomes, events, by = "DOCTOR_ID")
df_merged = df_merged %>%
    mutate(
        EVENT = if_else(!is.na(DATE), 1, 0),
        EVENT_YEAR = if_else(!is.na(DATE), as.numeric(format(DATE, "%Y")), NA_real_),
        EVENT_MONTH = if_else(!is.na(DATE), (as.numeric(format(DATE, "%Y")) - 1998) * 12 + as.numeric(format(DATE, "%m")), NA_real_),
    ) %>%
    select(-DATE)

# Prepare covariates and specialty + merge them in the main dataframe
covariates_new = covariates %>%
    select(DOCTOR_ID, BIRTH_DATE, SEX, INTERPRETATION) %>%
    mutate(SPECIALTY = as.character(INTERPRETATION)) %>% 
    mutate(BIRTH_YEAR = as.numeric(substr(BIRTH_DATE, 1, 4))) %>% # date format is YYYY-MM-DD
    select(-BIRTH_DATE, -INTERPRETATION)
df_complete = merge(df_merged, covariates_new, by = "DOCTOR_ID", how = "left")
df_complete = df_complete %>% 
    mutate(
        AGE = YEAR - BIRTH_YEAR,
        AGE_IN_2023 = 2023 - BIRTH_YEAR,
        AGE_AT_EVENT = if_else(is.na(EVENT_YEAR), NA_real_, EVENT_YEAR - BIRTH_YEAR)
    )
events_after60 = df_complete %>% filter(AGE_AT_EVENT > 60) %>% pull(DOCTOR_ID) %>% unique()
df_complete = df_complete %>% 
    filter(!(DOCTOR_ID %in% events_after60)) %>% # remove people which experiment the event after pension (age 60)
    filter(AGE <= 60) # remove all prescriptions done after pension (age 60)

df_model = df_complete %>%
    mutate(
        PERIOD = case_when(
            !is.na(EVENT_MONTH) & MONTH < EVENT_MONTH ~ "BEFORE",
            !is.na(EVENT_MONTH) & MONTH > EVENT_MONTH ~ "AFTER",
            is.na(EVENT_MONTH) ~ NA_character_),
        time_from_event = MONTH - EVENT_MONTH
    ) %>%
    mutate(
        PERIOD = factor(PERIOD, levels = c("BEFORE", "AFTER")), # set BEFORE as reference
        SPECIALTY = factor(SPECIALTY, levels = c("", setdiff(unique(df_complete$SPECIALTY), ""))), # set no specialty as reference
        SEX = factor(SEX, levels = c(1, 2), labels = c("Male", "Female")) # set male as reference
    )


# Prepare the model data (monthly)
df_model$ID <- as.integer(factor(df_model$DOCTOR_ID))                             # create a numeric ID variable
df_model$G <- ifelse(is.na(df_model$EVENT_MONTH), 0, df_model$EVENT_MONTH)        # G = month of first treatment, 0 for never-treated
df_model$T <- df_model$MONTH                                                      # time index in months

# Filter to males only and add spouse events
df_model <- df_model %>%
    filter(SEX == "Male") %>%
    left_join(spouse_events, by = "DOCTOR_ID") %>%
    mutate(
        EVENT = if_else(!is.na(SPOUSE_EVENT_DATE), 1, 0),
        EVENT_YEAR = if_else(!is.na(SPOUSE_EVENT_DATE), as.numeric(format(SPOUSE_EVENT_DATE, "%Y")), EVENT_YEAR),
        EVENT_MONTH = if_else(!is.na(SPOUSE_EVENT_DATE), (as.numeric(format(SPOUSE_EVENT_DATE, "%Y")) - 1998) * 12 + as.numeric(format(SPOUSE_EVENT_DATE, "%m")), EVENT_MONTH),
        G = ifelse(is.na(EVENT_MONTH), 0, EVENT_MONTH),
        SPECIALTY = factor(SPECIALTY)
    ) %>%
    select(-SPOUSE_EVENT_DATE)

# Count cases and controls
n_cases <- df_model %>% filter(EVENT == 1) %>% pull(DOCTOR_ID) %>% unique() %>% length()
n_controls <- df_model %>% filter(EVENT == 0) %>% pull(DOCTOR_ID) %>% unique() %>% length()

# Sample 100 cases per event year, then 2x that amount of controls
set.seed(123)
event_years <- df_model %>%filter(EVENT == 1) %>%pull(EVENT_YEAR) %>%unique() %>% na.omit()
n_event_years <- length(event_years)

# Sample case IDs per event year using a for loop
sampled_cases <- c()
for (year in event_years) {
    cases_in_year <- df_model %>%
        filter(EVENT == 1 & EVENT_YEAR == year) %>%
        pull(DOCTOR_ID) %>%
        unique()
    
    n_to_sample <- min(100, length(cases_in_year))
    sampled_year <- sample(cases_in_year, size = n_to_sample, replace = FALSE)
    sampled_cases <- c(sampled_cases, sampled_year)
}
sampled_cases <- unique(sampled_cases)

# Get unique control IDs, then sample 2 * n_event_years of them
control_ids <- df_model %>%
    filter(EVENT == 0) %>%
    pull(DOCTOR_ID) %>%
    unique()
sampled_controls <- sample(control_ids, size = min(2 * n_event_years, length(control_ids)), replace = FALSE)

# Filter df_model to only sampled IDs
df_model <- df_model %>% filter(DOCTOR_ID %in% c(sampled_cases, sampled_controls))
# Replace/Add missing monthly N values with 0s 
df_model[is.na(N), N := 0]

# Run DiD model
att_gt_res <- att_gt(
    yname = "N",
    tname = "T",
    idname = "ID",
    gname = "G",
    xformla = ~ BIRTH_YEAR, #removed specialty because of small sample size
    data = df_model,
    est_method = "dr",
    control_group = "notyettreated",
    clustervars = "ID",
    pl = TRUE,
    cores = N_THREADS
)

agg_dynamic <- aggte(att_gt_res, type = "dynamic", na.rm = TRUE)
results_df <- data.frame(
    time = agg_dynamic$egt,
    att = agg_dynamic$att.egt,
    se = agg_dynamic$se.egt
)

results_filtered <- results_df %>% filter(time >= -5*12 & time <= 5*12)
combined_plot <- ggplot(results_filtered, aes(x = time, y = att)) +
    geom_line(color = "#1f77b4") +
    geom_point() +
    geom_errorbar(aes(ymin = att - 1.96 * se, ymax = att + 1.96 * se), width = 0.2, color = "#1f77b4") +
    geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
    labs(
        title = "Effect of Pregnancy on Overall Prescriptions - Male doctors (ZOOM in Months)",
        subtitle = paste0("Cases: ", n_cases, ", Controls: ", n_controls),
        x = "Months from Event",
        y = "Change in total number of prescriptions"
    ) +
    scale_x_continuous(limits = c(-5*12, 5*12), breaks = seq(-5*12, 5*12, 12)) +
    theme_minimal()

ggsave(
    filename = file.path(outdir, "Supplements_Pregnancy_ZOOM_sampled.png"),
    plot = combined_plot,
    width = 10, 
    height = 8, 
    dpi = 300
)
