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
events_file = "/media/volume/Projects/DSGELabProject1/DiD_Experiments/Version2_Highthroughput_drop/ProcessedEvents_20251014/processed_events.parquet"
outcomes_file = "/media/volume/Projects/DSGELabProject1/DiD_Experiments/Version2_Highthroughput_drop/ProcessedOutcomes_20251014/processed_outcomes.parquet"
covariates_file = "/media/volume/Projects/DSGELabProject1/doctor_characteristics_20250520.csv"

DATE <- format(Sys.time(), "%Y%m%d")
outdir = "/media/volume/Projects/DSGELabProject1/Plots/Supplements/"
if (!dir.exists(outdir)) {dir.create(outdir, recursive = TRUE)}

N_THREADS = 10
setDTthreads(N_THREADS) 

event_code = "Diag_O02"

# ------------------------------------------------
##### Data Loading and Preprocessing
# ------------------------------------------------

doctor_ids = fread(doctor_list, header = FALSE)$V1

events = as.data.table(read_parquet(events_file))
event_code_parts = strsplit(event_code, "_")[[1]]
event_source = event_code_parts[1]
event_actual_code = event_code_parts[2]

# Filter events based on the event code
events = events[SOURCE == event_source & startsWith(as.character(CODE), event_actual_code), ]
event_ids = intersect(unique(events$PATIENT_ID), doctor_ids)
control_ids <- setdiff(doctor_ids, event_ids)

outcomes = as.data.table(read_parquet(outcomes_file))
covariates = fread(covariates_file)

# prepare outcomes for DiD analysis
outcomes = outcomes[DOCTOR_ID %in% doctor_ids,]

## prepare events for DiD analysis
# compared to previous scripts, here we all the event per patient
events = events[, .(PATIENT_ID, CODE, DATE)]
events = events[PATIENT_ID %in% doctor_ids,]
events$DATE <- as.Date(events$DATE)

# Merge with outcomes
events = events %>% rename("DOCTOR_ID" = "PATIENT_ID", "EVENT_DATE" = "DATE")
df_merged = left_join(outcomes, events, by = "DOCTOR_ID")
df_merged = df_merged %>%
    mutate(
        EVENT = if_else(!is.na(EVENT_DATE), 1, 0),
        EVENT_YEAR = if_else(!is.na(EVENT_DATE), as.numeric(format(EVENT_DATE, "%Y")), NA_real_),
        EVENT_MONTH = if_else(!is.na(EVENT_DATE), (as.numeric(format(EVENT_DATE, "%Y")) - 1998) * 12 + as.numeric(format(EVENT_DATE, "%m")), NA_real_),
    )

# Prepare covariates and merge
covariates_new = covariates %>%
    select(DOCTOR_ID, BIRTH_DATE, SEX, INTERPRETATION) %>%
    mutate(SPECIALTY = as.character(INTERPRETATION)) %>%
    mutate(BIRTH_YEAR = as.numeric(substr(BIRTH_DATE, 1, 4))) %>%
    select(-BIRTH_DATE, -INTERPRETATION)
df_complete = merge(df_merged, covariates_new, by = "DOCTOR_ID", how = "left") %>% as_tibble()
df_complete = df_complete %>% 
    mutate(
        AGE = YEAR - BIRTH_YEAR,
        AGE_IN_2023 = 2023 - BIRTH_YEAR,
        AGE_AT_EVENT = if_else(is.na(EVENT_YEAR), NA_real_, EVENT_YEAR - BIRTH_YEAR)
    )
events_after60 = df_complete %>% filter(AGE_AT_EVENT > 60) %>% pull(DOCTOR_ID) %>% unique()
df_complete = df_complete %>% 
    filter(!(DOCTOR_ID %in% events_after60)) %>%
    filter(AGE <= 60)

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
df_model <- df_model %>% mutate(N = replace_na(N, 0))
# Step 1: prepare the model data
df_model$ID <- as.integer(factor(df_model$DOCTOR_ID))                       # create a numeric ID variable
df_model$G <- ifelse(is.na(df_model$EVENT_YEAR), 0, df_model$EVENT_YEAR)  # G = group of first treatment year, 0 for never-treated
df_model$T <- df_model$YEAR    

# STEP 2: run staggered did model
# filter only women for correct controls
df_model <- df_model %>% filter(SEX == "Female")
n_cases <- df_model %>% filter(EVENT == 1) %>% pull(DOCTOR_ID) %>% unique() %>% length()
n_controls <- df_model %>% filter(EVENT == 0) %>% pull(DOCTOR_ID) %>% unique() %>% length()

att_gt_res <- att_gt(
    yname = "N",
    tname = "T",
    idname = "ID",
    gname = "G",
    xformla = ~ BIRTH_YEAR + SPECIALTY,
    data = df_model,
    est_method = "dr",
    control_group = "notyettreated",
    clustervars = "ID",
    pl = TRUE,
    cores = N_THREADS
)

agg_dynamic <- aggte(att_gt_res, type = "dynamic", na.rm = TRUE)
results <- data.frame(
    time = agg_dynamic$egt,
    att = agg_dynamic$att.egt,
    se = agg_dynamic$se.egt
)

data_plot <- results %>% filter(time >= -5 & time <= 5)
p_dynamic <- ggplot(data_plot, aes(x = time, y = att)) +
    geom_line(color = "#1f77b4") +
    geom_point() +
    geom_errorbar(aes(ymin = att - 1.96 * se, ymax = att + 1.96 * se), width = 0.2, color = "#1f77b4") +
    geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
    labs(
        title = "Effect of Other abnormal products of conception (ICD-10: O02) on Overall Prescriptions",
        subtitle = paste0("Cases: ", n_cases, ", Controls: ", n_controls),
        x = "Years from Event",
        y = "Change in total number of prescriptions"
    ) +
    scale_x_continuous(breaks = -5:5) +
    theme_minimal()

ggsave(
    filename = file.path(outdir, paste0("SupplementaryFigure_Abortion_", DATE, ".png")),
    plot = p_dynamic,
    width = 10, 
    height = 8, 
    dpi = 300
)


# ---------------------------------------------------------------------------
# Month zoom in 
# ---------------------------------------------------------------------------

doctor_list = "/media/volume/Projects/DSGELabProject1/doctors_20250424.csv"
events_file = "/media/volume/Projects/DSGELabProject1/DiD_Experiments/Version1_Highthroughput_drop/ProcessedEvents_20250926/processed_events.parquet"
outcomes_file = "/media/volume/Projects/DSGELabProject1/DiD_Experiments/Version1_Highthroughput_drop/ProcessedOutcomes_20250926/processed_outcomes.parquet"
covariates_file = "/media/volume/Projects/DSGELabProject1/doctor_characteristics_20250520.csv"

doctor_ids = fread(doctor_list, header = FALSE)$V1
events = as.data.table(read_parquet(events_file))
event_code_parts = strsplit(event_code, "_")[[1]]
event_source = event_code_parts[1]
event_actual_code = event_code_parts[2]

# Filter events based on the event code
events = events[SOURCE == event_source & startsWith(as.character(CODE), event_actual_code), ]
event_ids = intersect(unique(events$PATIENT_ID), doctor_ids)
control_ids <- setdiff(doctor_ids, event_ids)

outcomes = as.data.table(read_parquet(outcomes_file))
covariates = fread(covariates_file)

# prepare outcomes for DiD analysis
outcomes = outcomes[DOCTOR_ID %in% doctor_ids,]

## prepare events for DiD analysis
# compared to previous scripts, here we all the event per patient
events = events[, .(PATIENT_ID, CODE, DATE)]
events = events[PATIENT_ID %in% doctor_ids,]
events$DATE <- as.Date(events$DATE)

# Merge with outcomes
events = events %>% rename("DOCTOR_ID" = "PATIENT_ID", "EVENT_DATE" = "DATE")
df_merged = left_join(outcomes, events, by = "DOCTOR_ID")
df_merged = df_merged %>%
    mutate(
        EVENT = if_else(!is.na(EVENT_DATE), 1, 0),
        EVENT_YEAR = if_else(!is.na(EVENT_DATE), as.numeric(format(EVENT_DATE, "%Y")), NA_real_),
        EVENT_MONTH = if_else(!is.na(EVENT_DATE), (as.numeric(format(EVENT_DATE, "%Y")) - 1998) * 12 + as.numeric(format(EVENT_DATE, "%m")), NA_real_),
    )

# Prepare covariates and merge
covariates_new = covariates %>%
    select(DOCTOR_ID, BIRTH_DATE, SEX, INTERPRETATION) %>%
    mutate(SPECIALTY = as.character(INTERPRETATION)) %>%
    mutate(BIRTH_YEAR = as.numeric(substr(BIRTH_DATE, 1, 4))) %>%
    select(-BIRTH_DATE, -INTERPRETATION)
df_complete = merge(df_merged, covariates_new, by = "DOCTOR_ID", how = "left") %>% as_tibble()
df_complete = df_complete %>% 
    mutate(
        AGE = YEAR - BIRTH_YEAR,
        AGE_IN_2023 = 2023 - BIRTH_YEAR,
        AGE_AT_EVENT = if_else(is.na(EVENT_YEAR), NA_real_, EVENT_YEAR - BIRTH_YEAR)
    )
events_after60 = df_complete %>% filter(AGE_AT_EVENT > 60) %>% pull(DOCTOR_ID) %>% unique()
df_complete = df_complete %>% 
    filter(!(DOCTOR_ID %in% events_after60)) %>%
    filter(AGE <= 60)
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

# Replace missing N values with 0s 
df_model <- df_model %>% mutate(N = replace_na(N, 0))
# Prepare the model data (monthly)
df_model$ID <- as.integer(factor(df_model$DOCTOR_ID))                             # create a numeric ID variable
df_model$G <- ifelse(is.na(df_model$EVENT_MONTH), 0, df_model$EVENT_MONTH)        # G = month of first treatment, 0 for never-treated
df_model$T <- df_model$MONTH                                                      # time index in months

# Filter to 1998-2008 timeframe
df_model <- df_model %>%
    filter(YEAR >= 1998 & YEAR <= 2008) %>%
    filter(is.na(EVENT_YEAR) | (EVENT_YEAR >= 1998 & EVENT_YEAR <= 2008))

# Filter women and Count cases and controls
df_model <- df_model %>% filter(SEX == "Female")
n_cases <- df_model %>% filter(EVENT == 1) %>% pull(DOCTOR_ID) %>% unique() %>% length()
n_controls <- df_model %>% filter(EVENT == 0) %>% pull(DOCTOR_ID) %>% unique() %>% length()

# Run DiD model
att_gt_res <- att_gt(
    yname = "N",
    tname = "T",
    idname = "ID",
    gname = "G",
    xformla = ~ BIRTH_YEAR + SPECIALTY,
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

data_plot <- results_df %>% filter(time >= -5 & time <= 5)
p_dynamic <- ggplot(data_plot, aes(x = time, y = att)) +
    geom_line(color = "#1f77b4") +
    geom_point() +
    geom_errorbar(aes(ymin = att - 1.96 * se, ymax = att + 1.96 * se), width = 0.2, color = "#1f77b4") +
    geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
    labs(
        title = "Effect of Other abnormal products of conception (ICD-10: O02) on Overall Prescriptions - Month Zoom-In",
        subtitle = paste0("Cases: ", n_cases, ", Controls: ", n_controls),
        x = "Months from Event",
        y = "Change in total number of prescriptions"
    ) +
    scale_x_continuous(breaks = -5:5) +
    theme_minimal()

ggsave(
    filename = file.path(outdir, "Supplements_Abortion_ZOOM.png"),
    plot = p_dynamic,
    width = 10, 
    height = 8, 
    dpi = 300
)
