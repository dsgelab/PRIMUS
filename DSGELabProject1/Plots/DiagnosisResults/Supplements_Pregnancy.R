
# ============================================================
# 1. Libraries
# ============================================================
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


# ============================================================
# 2. Paths - ALL input/output paths declared here
# ============================================================

# --- Date stamps used to build input file paths ---
DATE_DATA_YEARLY  <- "20260219"   # yearly-resolution outcomes extraction date 
DATE_DATA_MONTHLY <- "20250926"   # month-resolution outcomes extraction date 
TODAY             <- format(Sys.time(), "%Y%m%d")   

# --- Input files shared across both the yearly and monthly analyses ---
PATH_DOCTOR_LIST        <- "/media/volume/Projects/DSGELabProject1/doctors_20250424.csv"
PATH_PREGNANCIES_FILE   <- "/media/volume/Projects/DSGELabProject1/ProcessedData/AllPregnanciesEvents_20251016.csv"
PATH_RELATIVES_FILE     <- "/media/volume/Projects/DSGELabProject1/doctors_and_relative_20250521.csv"
PATH_COVARIATES_FILE    <- "/media/volume/Projects/DSGELabProject1/doctor_characteristics_20250520.csv"
PATH_MARRIAGE_INFO_FILE <- "/media/volume/Data_20250430/DVV/FD_2698_Tulokset 2024-09-11 AVIOLIITOT.csv"

# --- Outcomes: two different resolutions/extractions are used in this script ---
PATH_OUTCOMES_FILE_YEARLY  <- paste0("/media/volume/Projects/DSGELabProject1/DiD_Experiments/DiD_Diagnosis_", DATE_DATA_YEARLY, "/ProcessedOutcomes_", DATE_DATA_YEARLY, "/processed_outcomes.parquet")
PATH_OUTCOMES_FILE_MONTHLY <- paste0("/media/volume/Projects/DSGELabProject1/DiD_Experiments/Archive/Version1_Highthroughput_drop/ProcessedOutcomes_", DATE_DATA_MONTHLY, "/processed_outcomes.parquet")

# --- Output directory ---
DIR_OUT <- "/media/volume/Projects/DSGELabProject1/Plots/ManuscriptFinal/"
if (!dir.exists(DIR_OUT)) dir.create(DIR_OUT, recursive = TRUE)

# --- Output file naming patterns ---
FILE_FEMALE_LONG        <- paste0("Supplements_Pregnancy_Female_Long_", TODAY, ".csv")
FILE_MALE_LONG          <- paste0("Supplements_Pregnancy_Male_Long_", TODAY, ".csv")
FILE_PLOT_DID_YEARS_V1  <- paste0("Plot_Supplements_Pregnancy_V1_", TODAY)
FILE_PLOT_DID_YEARS_V2  <- paste0("Plot_Supplements_Pregnancy_V2_", TODAY)
FILE_CSV_DID_YEARS      <- paste0("Supplements_Pregnancy_ByYears_", TODAY, ".csv")
FILE_PLOT_MALE_MONTHS   <- paste0("Plot_Supplements_Pregnancy_MaleByMonths_", TODAY)
FILE_CSV_MALE_MONTHS    <- paste0("Supplements_Pregnancy_MaleByMonths_", TODAY, ".csv")

# ============================================================
# 3. Plotting parameters - ALL plot settings declared here
# ============================================================

WIN <- 3   # yearly event-study window (years) shown plots
DODGE_WIDTH_CMP <- 0.3   # horizontal dodge for comparison

# -- Export settings (used for every ggsave call) --
PLOT_DPI                    <- 300
PLOT_WIDTH_DID_YEARS_V1     <- 10
PLOT_HEIGHT_DID_YEARS_V1    <- 8
PLOT_WIDTH_DID_YEARS_V2     <- 12
PLOT_HEIGHT_DID_YEARS_V2    <- 8
PLOT_WIDTH_MALE_MONTHS      <- 10
PLOT_HEIGHT_MALE_MONTHS     <- 12

# -- Colors --
COLOR_SINGLE_LINE <- "#1f77b4" 
COLOR_MALE_LINE   <- "#1f77b4"
COLOR_FEMALE_LINE <- "#ff7f0e"  
COLOR_ZERO_LINE   <- "red"       
THEME_BASE        <- theme_minimal()

# -- Month-resolution (male cases only) descriptive plot settings --
MONTH_WIN           <- 36                       # +/- months around event shown
MONTH_BREAKS_STEP   <- 12                       # x-axis tick spacing (months)
CASES_CENTERED_YLIM <- c(0, 150)                # fixed y-axis range across era panels
ERA_BREAKS   <- c("Before 2003", "2003-2012", "2013-2022") # parental leave era in Finland
PANEL_LABELS <- c("A", "B", "C")

# -- Helper: save a ggplot as both PNG and PDF using the same base filename --
save_plot_png_pdf <- function(plot, dir, basename, width, height, dpi = PLOT_DPI) {
    ggsave(filename = file.path(dir, paste0(basename, ".png")), 
        plot = plot,
        width = width, 
        height = height, 
        dpi = dpi
    )
    ggsave(filename = file.path(dir, paste0(basename, ".pdf")),
        plot = plot,
        width = width, 
        height = height
    )
}


# ============================================================
# 4. Global settings
# ============================================================

N_THREADS <- 10
setDTthreads(N_THREADS)

# Event definition: ICD-10 codes 
EVENT_CODE <- "O80|O81|O82|O83|O84"

# ============================================================
# 5. Load shared data (yearly analysis) and identify cohorts
# ============================================================

doctor_ids <- fread(PATH_DOCTOR_LIST, header = FALSE)$V1
outcomes   <- as.data.table(read_parquet(PATH_OUTCOMES_FILE_YEARLY))
covariates <- fread(PATH_COVARIATES_FILE)
events     <- fread(PATH_PREGNANCIES_FILE)
relatives  <- fread(PATH_RELATIVES_FILE)

# filter spouses of doctors
spouse_ids <- relatives %>% filter(RELATIVE_TYPE == "SPOUSE") %>% pull(RELATIVE_ID) %>% unique()

# Filter events based on the event code (EVENT_CODE can be a regex like "AA|BB|CC" to match multiple codes)
pattern <- paste0("^(", EVENT_CODE, ")")
events <- events[grepl(pattern, as.character(ICD10_CODE), perl = TRUE), .(PATIENT_ID, ICD10_CODE, VISIT_DATE)]
events$VISIT_DATE <- as.Date(events$VISIT_DATE)
events <- events[events[, .I[which.min(VISIT_DATE)], by = .(PATIENT_ID, ICD10_CODE)]$V1] # only use first event
events <- events %>%
    rename(
        # new = old
        EVENT_DATE = VISIT_DATE,
        EVENT_CODE = ICD10_CODE
    )

# Summary statistics:
# 1. Total number of pregnancies
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

# Generate list of IDs in the two groups
pregnancy_females <- doctors_ids_with_event
pregnancy_males <- relatives %>%
    filter(RELATIVE_TYPE == "SPOUSE") %>%
    filter(RELATIVE_ID %in% spouse_ids_with_event) %>%
    pull(DOCTOR_ID) %>%
    unique()
pregnancy_all <- c(pregnancy_females, pregnancy_males)

# extract spouse event years for later use (restricting male cohort to marriage window)
spouse_events <- relatives %>%
    filter(RELATIVE_TYPE == "SPOUSE") %>%
    select(DOCTOR_ID, RELATIVE_ID) %>%
    inner_join(events %>% select(PATIENT_ID, EVENT_DATE), by = c("RELATIVE_ID" = "PATIENT_ID")) %>%
    mutate(SPOUSE_EVENT_YEAR = as.numeric(format(EVENT_DATE, "%Y"))) %>%
    select(DOCTOR_ID, SPOUSE_EVENT_YEAR) %>%
    distinct()


# ============================================================
# 6. Merge events with outcomes + covariates, then QC
# ============================================================

# Merge events with outcomes
events <- events %>% filter(PATIENT_ID %in% pregnancy_all) %>% rename(DOCTOR_ID = PATIENT_ID, DATE = EVENT_DATE)
df_merged <- left_join(outcomes, events, by = "DOCTOR_ID")
df_merged <- df_merged %>%
    mutate(
        EVENT      = if_else(!is.na(DATE), 1, 0),
        EVENT_YEAR = if_else(!is.na(DATE), as.numeric(format(DATE, "%Y")), NA_real_),
    ) %>%
    select(-DATE)

# Prepare covariates and specialty, then merge into the main dataframe
covariates_new <- covariates %>%
    select(DOCTOR_ID, BIRTH_DATE, SEX, INTERPRETATION) %>%
    mutate(SPECIALTY = as.character(INTERPRETATION)) %>%   
    mutate(BIRTH_YEAR = as.numeric(substr(BIRTH_DATE, 1, 4))) %>%   # date format is YYYY-MM-DD
    select(-BIRTH_DATE, -INTERPRETATION)
df_complete <- merge(df_merged, covariates_new, by = "DOCTOR_ID", how = "left")
df_complete <- df_complete %>%
    mutate(
        AGE          = YEAR - BIRTH_YEAR,
        AGE_IN_2023  = 2023 - BIRTH_YEAR,
        AGE_AT_EVENT = if_else(is.na(EVENT_YEAR), NA_real_, EVENT_YEAR - BIRTH_YEAR)
    )
events_after60 <- df_complete %>% filter(AGE_AT_EVENT > 60) %>% pull(DOCTOR_ID) %>% unique()
df_complete <- df_complete %>%
    filter(!(DOCTOR_ID %in% events_after60)) %>%   # remove doctors who experienced the event after pension (age 60)
    filter(AGE <= 60)                              # remove all prescriptions logged after pension (age 60)

df_model <- df_complete %>%
    mutate(
        SPECIALTY = factor(SPECIALTY),  
        SEX       = factor(SEX, levels = c(1, 2), labels = c("Male", "Female"))   
    )

# Replace missing N values with 0s
df_model[is.na(N), N := 0]

# DiD variables: numeric ID, group (first treatment year), calendar year
df_model$ID <- as.integer(factor(df_model$DOCTOR_ID))
df_model$G  <- ifelse(is.na(df_model$EVENT_YEAR), 0, df_model$EVENT_YEAR)   # 0 = never-treated
df_model$T  <- df_model$YEAR


# ============================================================
# 7. Female DiD (own pregnancy, yearly resolution)
# ============================================================

# keep female doctors & ensure cases are limited to the pregnancy_females list
df_model_female <- df_model %>%
    filter(SEX == "Female") %>%
    filter(EVENT == 0 | (EVENT == 1 & DOCTOR_ID %in% pregnancy_females))

n_cases_female    <- df_model_female %>% filter(EVENT == 1) %>% pull(DOCTOR_ID) %>% unique() %>% length()
n_controls_female <- df_model_female %>% filter(EVENT == 0) %>% pull(DOCTOR_ID) %>% unique() %>% length()
events_per_year_female <- df_model_female[df_model_female$EVENT == 1, .(N = uniqueN(DOCTOR_ID)), by = EVENT_YEAR][order(EVENT_YEAR)]
events_year_str_female <- paste0(events_per_year_female$EVENT_YEAR, ":", events_per_year_female$N, collapse = ", ")

# Baseline prescription rate in controls (used to express effects as % change)
baseline_female <- mean(df_model_female$N[df_model_female$EVENT == 0], na.rm = TRUE)

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
    time     = agg_dynamic_female$egt,
    att      = agg_dynamic_female$att.egt,
    se       = agg_dynamic_female$se.egt,
    baseline = baseline_female
)

# Baseline & relative change estimates (effect size as % of control baseline)
results_female <- results_female %>%
    mutate(
        rel_att    = round(100 * att / baseline, 5),
        rel_att_se = round(100 * se / baseline, 5)
    )

# Save female long results
results_female_long <- results_female %>% mutate(group = "Female")
write.csv(results_female_long, file.path(DIR_OUT, FILE_FEMALE_LONG), row.names = FALSE)

data_plot <- results_female %>% filter(time >= -WIN & time <= WIN)
p_dynamic_female <- ggplot(data_plot, aes(x = time, y = att)) +
    geom_line(color = COLOR_SINGLE_LINE) +
    geom_point() +
    geom_errorbar(aes(ymin = att - 1.96 * se, ymax = att + 1.96 * se), width = 0.2, color = COLOR_SINGLE_LINE) +
    geom_hline(yintercept = 0, linetype = "dashed", color = COLOR_ZERO_LINE) +
    labs(
        title = expression(bold(A)~". Effect of Pregnancy on Overall Prescriptions - Female Doctors"),
        subtitle = paste0("Cases: ", n_cases_female, ", Controls: ", n_controls_female),
        x = "Years from Event",
        y = "Change in Total Number of Prescriptions \n(compared to controls)"
    ) +
    scale_x_continuous(breaks = -WIN:WIN) +
    THEME_BASE


# ============================================================
# 8. Male DiD (spouse pregnancy, restricted to marriage years) 
# ============================================================

# clean marriage info (year-level: restrict spouse events to during the marriage)
marriage_info <- fread(PATH_MARRIAGE_INFO_FILE)
marriage_info <- marriage_info %>%
    rename(
        DOCTOR_ID      = "FID",
        SPOUSE_ID      = "FID2",
        MARITAL_STATUS = "Tutkhenk_nykyinen_siviilisaaty"
    ) %>%
    mutate(
        START_DATE = as.Date(as.character(Alkupaiva), format = "%Y%m%d"),
        END_DATE   = as.Date(as.character(Paattymispaiva), format = "%Y%m%d")
    ) %>%
    mutate(
        START_YEAR = if_else(is.na(year(START_DATE)), 1998, year(START_DATE)),
        END_YEAR   = if_else(is.na(year(END_DATE)),   2023, year(END_DATE))
    ) %>%
    filter(
        DOCTOR_ID %in% doctor_ids,
        MARITAL_STATUS == 2,
        !is.na(SPOUSE_ID)
    ) %>%
    select(DOCTOR_ID, SPOUSE_ID, START_YEAR, END_YEAR)

# filter events that happened during marriage
spouse_events <- spouse_events %>%
    inner_join(marriage_info, by = "DOCTOR_ID") %>%
    filter(SPOUSE_EVENT_YEAR >= START_YEAR & SPOUSE_EVENT_YEAR <= END_YEAR)

df_model_male <- df_model %>%
    filter(SEX == "Male") %>%
    left_join(spouse_events, by = "DOCTOR_ID") %>%
    mutate(
        EVENT      = if_else(!is.na(SPOUSE_EVENT_YEAR), 1, 0),
        EVENT_YEAR = if_else(!is.na(SPOUSE_EVENT_YEAR), as.numeric(SPOUSE_EVENT_YEAR), EVENT_YEAR),
        G          = ifelse(is.na(EVENT_YEAR), 0, EVENT_YEAR)
    ) %>%
    select(-SPOUSE_EVENT_YEAR)

n_cases_male    <- df_model_male %>% filter(EVENT == 1) %>% pull(DOCTOR_ID) %>% unique() %>% length()
n_controls_male <- df_model_male %>% filter(EVENT == 0) %>% pull(DOCTOR_ID) %>% unique() %>% length()
events_per_year_male <- df_model_male[df_model_male$EVENT == 1, .(N = uniqueN(DOCTOR_ID)), by = EVENT_YEAR][order(EVENT_YEAR)]
events_year_str_male <- paste0(events_per_year_male$EVENT_YEAR, ":", events_per_year_male$N, collapse = ", ")

# Baseline prescription rate in controls (used to express effects as % change)
baseline_male <- mean(df_model_male$N[df_model_male$EVENT == 0], na.rm = TRUE)

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
    time     = agg_dynamic_male$egt,
    att      = agg_dynamic_male$att.egt,
    se       = agg_dynamic_male$se.egt,
    baseline = baseline_male
)

# Baseline & relative change estimates (effect size as % of control baseline)
results_male <- results_male %>%
    mutate(
        rel_att    = round(100 * att / baseline, 5),
        rel_att_se = round(100 * se / baseline, 5)
    )

# Save male long results
results_male_long <- results_male %>% mutate(group = "Male")
write.csv(results_male_long, file.path(DIR_OUT, FILE_MALE_LONG), row.names = FALSE)

data_plot <- results_male %>% filter(time >= -WIN & time <= WIN)
p_dynamic_male <- ggplot(data_plot, aes(x = time, y = att)) +
    geom_line(color = COLOR_SINGLE_LINE) +
    geom_point() +
    geom_errorbar(aes(ymin = att - 1.96 * se, ymax = att + 1.96 * se), width = 0.2, color = COLOR_SINGLE_LINE) +
    geom_hline(yintercept = 0, linetype = "dashed", color = COLOR_ZERO_LINE) +
    labs(
        title = expression(bold(B)~". Effect of Pregnancy on Overall Prescriptions - Male Doctors (with pregnant spouses)"),
        subtitle = paste0("Cases: ", n_cases_male, ", Controls: ", n_controls_male),
        x = "Years from Event",
        y = "Change in Total Number of Prescriptions \n(compared to controls)"
    ) +
    scale_x_continuous(breaks = -WIN:WIN) +
    THEME_BASE


# ============================================================
# 9. Combined figure (PNG + PDF)
# ============================================================

p_combined <- p_dynamic_female + p_dynamic_male + plot_layout(ncol = 1)
save_plot_png_pdf(p_combined, DIR_OUT, FILE_PLOT_DID_YEARS_V1, PLOT_WIDTH_DID_YEARS_V1, PLOT_HEIGHT_DID_YEARS_V1)

# Also do alternative version with both results
data_plot_combined <- rbind(
    results_female %>% mutate(group = "Female"),
    results_male %>% mutate(group = "Male")
)
data_plot_combined$group <- factor(data_plot_combined$group, levels = c("Female", "Male"))
data_plot_combined <- data_plot_combined %>% filter(time >= -WIN & time <= WIN)

# CHECKPOINT: Save combined DiD results to CSV
write.csv(data_plot_combined, file.path(DIR_OUT, FILE_CSV_DID_YEARS), row.names = FALSE)

p_combined_alt <- ggplot(data_plot_combined, aes(x = time, y = att, color = group, group = group)) +
    geom_line(linewidth = 0.8, position = position_dodge(width = DODGE_WIDTH_CMP)) +
    geom_point(size = 2, position = position_dodge(width = DODGE_WIDTH_CMP)) +
    geom_errorbar(
        aes(ymin = att - 1.96 * se, ymax = att + 1.96 * se),
        width = 0.2, position = position_dodge(width = DODGE_WIDTH_CMP)
    ) +
    geom_hline(yintercept = 0, linetype = "dashed", color = COLOR_ZERO_LINE) +
    labs(
        title = "Effect of Pregnancy on Overall Prescriptions - Female vs Male Doctors",
        subtitle = paste0("Female cases: ", n_cases_female, ", Female controls: ", n_controls_female, "\nMale cases: ", n_cases_male, ", Male controls: ", n_controls_male),
        x = "Years from Event",
        y = "Change in Total Number of Prescriptions \n(compared to controls)",
        color = "Group"
    ) +
    scale_color_manual(values = c("Female" = COLOR_FEMALE_LINE, "Male" = COLOR_MALE_LINE)) +
    scale_x_continuous(breaks = -WIN:WIN) +
    THEME_BASE

save_plot_png_pdf(p_combined_alt, DIR_OUT, FILE_PLOT_DID_YEARS_V2, PLOT_WIDTH_DID_YEARS_V2, PLOT_HEIGHT_DID_YEARS_V2)

# ============================================================
# 10. Descriptive month-resolution "cases centered on event" figure
#     males only, split by Finnish parental leave policy eras
# ============================================================

doctor_ids_m  <- fread(PATH_DOCTOR_LIST, header = FALSE)$V1
outcomes_m    <- as.data.table(read_parquet(PATH_OUTCOMES_FILE_MONTHLY))
covariates_m  <- fread(PATH_COVARIATES_FILE)
events_m      <- fread(PATH_PREGNANCIES_FILE)
relatives_m   <- fread(PATH_RELATIVES_FILE)

# filter spouses of doctors
spouse_ids_m <- relatives_m %>% filter(RELATIVE_TYPE == "SPOUSE") %>% pull(RELATIVE_ID) %>% unique()

# Filter events based on the event code
pattern_m <- paste0("^(", EVENT_CODE, ")")
events_m <- events_m[grepl(pattern_m, as.character(ICD10_CODE), perl = TRUE), .(PATIENT_ID, ICD10_CODE, VISIT_DATE)]
events_m$VISIT_DATE <- as.Date(events_m$VISIT_DATE)
events_m <- events_m %>% rename(EVENT_DATE = VISIT_DATE, EVENT_CODE = ICD10_CODE)

# Generate list of doctor IDs (males) for which the spouse has an event
spouse_ids_with_event_m <- events_m %>%
    filter(PATIENT_ID %in% spouse_ids_m) %>%
    pull(PATIENT_ID) %>%
    unique()
pregnancy_males_ids <- relatives_m %>%
    filter(RELATIVE_TYPE == "SPOUSE") %>%
    filter(RELATIVE_ID %in% spouse_ids_with_event_m) %>%
    pull(DOCTOR_ID) %>%
    unique()

# extract spouse event date for later use
spouse_events_m <- relatives_m %>%
    filter(RELATIVE_TYPE == "SPOUSE") %>%
    select(DOCTOR_ID, RELATIVE_ID) %>%
    inner_join(events_m %>% select(PATIENT_ID, EVENT_DATE), by = c("RELATIVE_ID" = "PATIENT_ID")) %>%
    rename(SPOUSE_EVENT_DATE = EVENT_DATE) %>%
    select(DOCTOR_ID, SPOUSE_EVENT_DATE) %>%
    distinct()

# clean marriage info (date-level, for month-resolution restriction to during marriage)
marriage_info_m <- fread(PATH_MARRIAGE_INFO_FILE)
marriage_info_m <- marriage_info_m %>%
    rename(
        DOCTOR_ID      = "FID",
        SPOUSE_ID      = "FID2",
        MARITAL_STATUS = "Tutkhenk_nykyinen_siviilisaaty"
    ) %>%
    mutate(
        START_DATE = as.Date(as.character(Alkupaiva), format = "%Y%m%d"),
        END_DATE   = as.Date(as.character(Paattymispaiva), format = "%Y%m%d")
    ) %>%
    mutate(
        START_DATE = if_else(is.na(START_DATE), as.Date("1998-01-01"), START_DATE),
        END_DATE   = if_else(is.na(END_DATE),   as.Date("2022-12-31"), END_DATE)
    ) %>%
    filter(
        DOCTOR_ID %in% doctor_ids_m,
        MARITAL_STATUS == 2,
        !is.na(SPOUSE_ID)
    ) %>%
    select(DOCTOR_ID, SPOUSE_ID, START_DATE, END_DATE)

# filter events that happened during marriage
spouse_events_m <- spouse_events_m %>%
    inner_join(marriage_info_m, by = "DOCTOR_ID") %>%
    filter(SPOUSE_EVENT_DATE >= START_DATE & SPOUSE_EVENT_DATE <= END_DATE) %>%
    select(DOCTOR_ID, SPOUSE_EVENT_DATE)

events_m <- spouse_events_m %>%
    filter(DOCTOR_ID %in% pregnancy_males_ids) %>%
    rename(DATE = SPOUSE_EVENT_DATE)
events_m <- events_m[events_m[, .I[which.min(DATE)], by = .(DOCTOR_ID)]$V1]   # only use first event

outcomes_m <- outcomes_m %>% filter(DOCTOR_ID %in% doctor_ids_m)

# Merge events with outcomes
df_merged_m <- left_join(outcomes_m, events_m, by = "DOCTOR_ID")
df_merged_m <- df_merged_m %>%
    mutate(
        EVENT       = if_else(!is.na(DATE), 1, 0),
        EVENT_YEAR  = if_else(!is.na(DATE), as.numeric(format(DATE, "%Y")), NA_real_),
        EVENT_MONTH = if_else(!is.na(DATE), (as.numeric(format(DATE, "%Y")) - 1998) * 12 + as.numeric(format(DATE, "%m")), NA_real_),
    ) %>%
    select(-DATE)

# Prepare covariates and specialty, then merge into the main dataframe
covariates_new_m <- covariates_m %>%
    select(DOCTOR_ID, BIRTH_DATE, SEX, INTERPRETATION) %>%
    mutate(SPECIALTY = as.character(INTERPRETATION)) %>%
    mutate(SPECIALTY = if_else(SPECIALTY == "", "No specialty", SPECIALTY)) %>%
    mutate(BIRTH_YEAR = as.numeric(substr(BIRTH_DATE, 1, 4))) %>%   # date format is YYYY-MM-DD
    select(-BIRTH_DATE, -INTERPRETATION)
df_complete_m <- merge(df_merged_m, covariates_new_m, by = "DOCTOR_ID", how = "left")
df_complete_m <- df_complete_m %>%
    mutate(
        AGE          = YEAR - BIRTH_YEAR,
        AGE_IN_2023  = 2023 - BIRTH_YEAR,
        AGE_AT_EVENT = if_else(is.na(EVENT_YEAR), NA_real_, EVENT_YEAR - BIRTH_YEAR)
    )
events_after60_m <- df_complete_m %>% filter(AGE_AT_EVENT > 60) %>% pull(DOCTOR_ID) %>% unique()
df_complete_m <- df_complete_m %>%
    filter(!(DOCTOR_ID %in% events_after60_m)) %>%   # remove doctors who experienced the event after pension (age 60)
    filter(AGE <= 60)                                # remove all prescriptions logged after pension (age 60)

df_model_m <- df_complete_m %>%
    mutate(
        SPECIALTY = factor(SPECIALTY),
        SEX       = factor(SEX, levels = c(1, 2), labels = c("Male", "Female"))   # set male as reference
    )

# Prepare the model data (monthly)
df_model_m$ID <- as.integer(factor(df_model_m$DOCTOR_ID))
df_model_m$G  <- ifelse(is.na(df_model_m$EVENT_MONTH), 0, df_model_m$EVENT_MONTH)
df_model_m$T  <- df_model_m$MONTH

# QC: Filter to males only
df_model_m <- df_model_m %>% filter(SEX == "Male")

# Count cases and controls
n_cases_m    <- df_model_m %>% filter(EVENT == 1) %>% pull(DOCTOR_ID) %>% unique() %>% length()
n_controls_m <- df_model_m %>% filter(EVENT == 0) %>% pull(DOCTOR_ID) %>% unique() %>% length()

# Replace/add missing monthly N values with 0s
df_model_m[is.na(N), N := 0]

# ---------------------------------------------------------------------------
# Plot case-only, centered on event month
cases_centered <- df_complete_m %>%
    filter(EVENT == 1) %>%
    mutate(
        event_group = case_when(
            EVENT_YEAR < 2003                       ~ ERA_BREAKS[1],
            EVENT_YEAR >= 2003 & EVENT_YEAR < 2013   ~ ERA_BREAKS[2],
            EVENT_YEAR >= 2013 & EVENT_YEAR <= 2022  ~ ERA_BREAKS[3],
            TRUE ~ NA_character_
        ),
        rel_month = MONTH - EVENT_MONTH
    ) %>%
    filter(!is.na(event_group), rel_month >= -MONTH_WIN, rel_month <= MONTH_WIN)

# Per-group n cases (for subtitles)
n_cases_by_group <- cases_centered %>%
    distinct(DOCTOR_ID, event_group) %>%
    count(event_group, name = "n_cases")

cases_centered_summary <- cases_centered %>%
    group_by(event_group, rel_month) %>%
    summarise(
        mean_n = mean(N, na.rm = TRUE),
        sd_n   = sd(N, na.rm = TRUE) / sqrt(n()),
        .groups = "drop"
    )

# CHECKPOINT: Save the cases-centered data to CSV for inspection
write.csv(cases_centered_summary, file.path(DIR_OUT, FILE_CSV_MALE_MONTHS), row.names = FALSE)

# Build one plot per event_group, each with its own title + subtitle (n cases)
plot_list <- lapply(seq_along(ERA_BREAKS), function(i) {
    grp   <- ERA_BREAKS[i]
    label <- PANEL_LABELS[i]
    n_grp <- n_cases_by_group %>% filter(event_group == grp) %>% pull(n_cases)
    n_grp <- if (length(n_grp) == 0) 0L else n_grp

    dat <- cases_centered_summary %>% filter(event_group == grp)

    ggplot(dat, aes(x = rel_month, y = mean_n)) +
        geom_ribbon(aes(ymin = mean_n - sd_n, ymax = mean_n + sd_n),
                    alpha = 0.15, color = NA) +
        geom_line(linewidth = 1) +
        geom_point(size = 1) +
        geom_vline(xintercept = 0, linetype = "dashed", color = COLOR_ZERO_LINE) +
        scale_x_continuous(limits = c(-MONTH_WIN, MONTH_WIN), breaks = seq(-MONTH_WIN, MONTH_WIN, MONTH_BREAKS_STEP)) +
        labs(
            title    = paste0(label, ". ", grp),
            subtitle = paste0("N cases: ", n_grp),
            x        = "Months from Event",
            y        = "Mean Number of Prescriptions"
        ) +
        THEME_BASE +
        ylim(CASES_CENTERED_YLIM[1], CASES_CENTERED_YLIM[2]) +
        theme(
            plot.title      = element_text(face = "bold"),
            plot.subtitle   = element_text(color = "grey40"),
            legend.position = "bottom"
        )
})

p_cases <- wrap_plots(plot_list, ncol = 1) +
    plot_layout(guides = "collect") &
    theme(legend.position = "bottom")

save_plot_png_pdf(p_cases, DIR_OUT, FILE_PLOT_MALE_MONTHS, PLOT_WIDTH_MALE_MONTHS, PLOT_HEIGHT_MALE_MONTHS)