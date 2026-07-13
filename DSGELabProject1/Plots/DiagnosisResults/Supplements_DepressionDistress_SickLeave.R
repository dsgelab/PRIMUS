
##
# This script is divided in two equal parts:
# 1. Recurrent depressive disorder phenotype
# 2. Distress (wide) phenotype

# ============================================================
# 1. Libraries
# ============================================================
.libPaths("/shared-directory/sd-tools/apps/R/lib/")
suppressPackageStartupMessages({
    library(data.table)
    library(arrow)
    library(dplyr)
    library(tidyr)
    library(lubridate)
    library(did)
    library(metafor)
    library(ggplot2)
})


# ============================================================
# 2. File paths and global settings
# ============================================================

DATE_DATA_1  <- "20260709"
DATE_DATA_2  <- "20260219"

doctor_list         <- "/media/volume/Projects/DSGELabProject1/doctors_20250424.csv"
events_file         <- paste0("/media/volume/Projects/DSGELabProject1/ProcessedData/AllDistressEvents_", DATE_DATA_1, ".parquet")
sick_leave_file     <- paste0("/media/volume/Projects/DSGELabProject1/ProcessedData/all_sickleaves_doctors_", DATE_DATA_1, ".parquet")
outcomes_file       <- paste0("/media/volume/Projects/DSGELabProject1/DiD_Experiments/DiD_Diagnosis_",DATE_DATA_2, "/ProcessedOutcomes_", DATE_DATA_2, "/processed_outcomes.parquet")
covariates_file     <- "/media/volume/Projects/DSGELabProject1/doctor_characteristics_20250520.csv"

# Window size for plot
WIN <- 3

PHENOTYPE <- list(
    name = "Recurrent depressive disorder",
    case_incl = c("F33"),
    case_excl = c("F33.4"),    # recurrent depressive disorder, currently in remission
    control_excl = c("F33", "F32", "F43", "Z73.0", "F41", "F51")
)

TODAY <- format(Sys.time(), "%Y%m%d")
outdir   <- paste0("/media/volume/Projects/DSGELabProject1/Plots/Supplements/Supplements_DepressionBurnout_SickLeaveScenarios_F33_", TODAY, "/")
if (!dir.exists(outdir)) dir.create(outdir, recursive = TRUE)

N_THREADS  <- 10
setDTthreads(N_THREADS)

# ============================================================
# 3. Load shared data
# ============================================================

doctor_ids <- fread(doctor_list, header = FALSE)$V1

# Covariates: keep specialty and birth year
covariates <- fread(covariates_file)
covariates[, `:=`(
    SPECIALTY  = as.character(INTERPRETATION),
    BIRTH_YEAR = as.numeric(substr(BIRTH_DATE, 1, 4)),
    BIRTH_DATE = NULL,
    INTERPRETATION = NULL
)]
covariates[SPECIALTY == "", SPECIALTY := "No specialty"]

# Outcomes: total number of prescriptions per doctor per year
outcomes <- as.data.table(read_parquet(outcomes_file, col_select = c("DOCTOR_ID", "YEAR", "N")))

# ============================================================
# 4. Extract events and define the three comparison scenarios
#
#   A. Diagnosis in care register, but NEVER took a sick leave
#   B. Diagnosis in care register AND took a sick leave
#      -> split by time distance between diagnosis and sick leave:
#         "before diagnosis" (distance < 0)
#         "immediate"         (0 <= distance <= 7)
#         "within a year"     (7 < distance <= 365)
#         "over a year later" (distance > 365)
#   C. NO diagnosis in care register, but a sick leave is recorded
#
#   For scenarios A and C, the FIRST available record is used.
#   For scenario B, the FIRST diagnosis and FIRST sick leave are used.
# ============================================================

# Load events and keep only Depression/Burnout codes
events_raw <- as.data.table(read_parquet(events_file))
events_raw[, DATE := as.Date(DATE)]
events_raw[, CODE := (CODE_ICD10)]

# Extract ids of doctors that will be included / excluded in the cohort
events_raw[, CODE := ifelse(
    nchar(CODE) >= 4 & substr(CODE, 4, 4) != ".", # QC: add dot after 3 char if not there
    paste0(substr(CODE, 1, 3), ".", substr(CODE, 4, nchar(CODE))),
    CODE
)]
case_incl_ids       <- events_raw[grepl(paste0("^(", paste(PHENOTYPE$case_incl, collapse = "|"), ")"), CODE), unique(DOCTOR_ID)]
case_excl_ids       <- events_raw[grepl(paste0("^(", paste(PHENOTYPE$case_excl, collapse = "|"), ")"), CODE), unique(DOCTOR_ID)]
control_excl_ids    <- events_raw[grepl(paste0("^(", paste(PHENOTYPE$control_excl, collapse = "|"), ")"), CODE), unique(DOCTOR_ID)]

# Extract cases for the phenotype
events_raw <- events_raw[DOCTOR_ID %in% case_incl_ids]
events_raw <- events_raw[!(DOCTOR_ID %in% case_excl_ids)]
events_raw <- events_raw[CODE_ICD10_3CHAR == "F33"]

# Take the FIRST record available per doctor, separately for each source
# (i.e. first diagnosis date in Care register, first sick leave date in Sick leave register)
events_raw <- events_raw[order(DOCTOR_ID, SOURCE, DATE)][, .SD[1], by = c("DOCTOR_ID", "SOURCE")]

# Go from long format to wide format: one row per doctor, with the first
# diagnosis date and the first sick leave date (NA if the doctor has none)
events_raw <- pivot_wider(
    as.data.frame(events_raw),
    id_cols = "DOCTOR_ID",
    names_from = "SOURCE",
    values_from = "DATE",
    values_fill = NA
) %>% as.data.table()

setnames(events_raw, c("CareRegister", "SickLeaveRegister"), c("DATE_CareRegister", "DATE_SickLeaveRegister"))

# Drop doctors with neither a diagnosis nor a sick leave record
events_raw <- events_raw[!(is.na(DATE_CareRegister) & is.na(DATE_SickLeaveRegister))]

# Merge in sick leave benefit-type information, for descriptive purposes only
sl <- as.data.table(read_parquet(sick_leave_file))
events_merged <- merge(
    events_raw,
    sl,
    by.x = c("DOCTOR_ID", "DATE_SickLeaveRegister"),
    by.y = c("DOCTOR_ID", "SVA_DATE"),
    all.x = TRUE
)
events_merged[, TYPE := fcase(
    BENEFIT_TYPE == 73, "partial",
    BENEFIT_TYPE == 74, "full",
    default = NA_character_
)]

# ------------------------------------------------------------
# Define scenario (A / B / C)
# ------------------------------------------------------------
events_merged[, SCENARIO := fcase(
    !is.na(DATE_CareRegister) &  is.na(DATE_SickLeaveRegister), "A",
    !is.na(DATE_CareRegister) & !is.na(DATE_SickLeaveRegister), "B",
     is.na(DATE_CareRegister) & !is.na(DATE_SickLeaveRegister), "C"
)]

# Distance (days) between sick leave and diagnosis - only meaningful for scenario B
events_merged[, distance_days := as.numeric(difftime(DATE_SickLeaveRegister, DATE_CareRegister, units = "days"))]

# Sub-group scenario B by time distance between diagnosis and sick leave
events_merged[, GROUP := fcase(
    SCENARIO == "A", "no sick leave",
    # SCENARIO == "B" & distance_days <  0,                          "before diagnosis",
    SCENARIO == "B" & distance_days >= 0   & distance_days <= 7,    "immediate",
    SCENARIO == "B" & distance_days >  7   & distance_days <= 365,  "within a year",
    # SCENARIO == "B" & distance_days >  365,                         "over a year later",
    SCENARIO == "C", "no diagnosis",
    default = NA_character_
)]

# Combined label used for stratification (keeps scenario A/B/C explicit)
events_merged[, STRATA_LABEL := fcase(
    SCENARIO == "A", "A: no sick leave",
    SCENARIO == "B", paste0("B: ", GROUP),
    SCENARIO == "C", "C: no diagnosis",
    default = NA_character_
)]

# Event date used for the DiD design:
#  - scenarios A & B: first diagnosis date (Care register)
#  - scenario C: first sick leave date (no diagnosis date is available)
events_merged[, EVENT_DATE := fifelse(SCENARIO == "C", DATE_SickLeaveRegister, DATE_CareRegister)]

# Count number of doctors in each scenario / group
group_counts <- events_merged[, .N, by = .(SCENARIO, GROUP)]
cat("Number of doctors in each scenario/group:\n")
print(group_counts)

# filter doctors in our cohort, then finalize data
events_doctors <- events_merged[DOCTOR_ID %in% doctor_ids]
events_doctors <- events_doctors[, .(DOCTOR_ID, EVENT_DATE, STRATA_LABEL)]

cat(sprintf("doctors with %s event: %d\n", PHENOTYPE$name, nrow(events_doctors)))

# ============================================================
# 5. Merge events, outcomes and covariates & QC steps
# ============================================================

# Left join: all outcome rows kept; controls get NA event date / strata
df <- left_join(outcomes, events_doctors, by = "DOCTOR_ID") %>%
    mutate(
        EVENT      = if_else(!is.na(EVENT_DATE), 1L, 0L),
        EVENT_YEAR = if_else(!is.na(EVENT_DATE), as.numeric(format(EVENT_DATE, "%Y")), NA_real_)
    ) %>%
    select(-EVENT_DATE) %>%
    as.data.table()

# Merge covariates
df <- covariates[df, on = "DOCTOR_ID"]
df[, `:=`(
    AGE          = YEAR - BIRTH_YEAR,
    AGE_AT_EVENT = fifelse(is.na(EVENT_YEAR), NA_real_, EVENT_YEAR - BIRTH_YEAR)
)]

# Remove doctors whose event occurred after pension age (60)
ids_post60 <- df[AGE_AT_EVENT > 60 & !is.na(AGE_AT_EVENT), unique(DOCTOR_ID)]
df <- df[!(DOCTOR_ID %in% ids_post60) & AGE <= 60]

# Replace missing prescription counts with 0
df[is.na(N), N := 0]

# Remove doctors from controls based on phenotype exclusion criteria
df <- df[!(EVENT == 0 & DOCTOR_ID %in% control_excl_ids),]

# --- DiD variables: numeric ID, group (first treatment year), calendar year ---
df[, ID := as.integer(factor(DOCTOR_ID))]
df[, G  := fifelse(is.na(EVENT_YEAR), 0, EVENT_YEAR)]
df[, T  := YEAR]

# STRATA is simply the scenario/group label already computed above (NA for controls)
df[, STRATA := STRATA_LABEL]


# ============================================================
# 6. Stratified DiD across scenarios A / B (sub-groups) / C
# ============================================================

group_results <- list()
group_results_long <- list()

# Every case doctor falls into exactly one of these six mutually-exclusive strata;
# controls (STRATA == NA) are shared as the comparison group for every stratum.
strata_values <- c(
    "A: no sick leave",
    #"B: before diagnosis",
    "B: immediate",
    "B: within a year",
    #"B: over a year later",
    "C: no diagnosis"
)

for (val in strata_values) {
    cat(sprintf("  Fitting: STRATA = '%s'\n", val))

    tryCatch({

        # Subset: this stratum's cases + all controls
        df_sub      <- df[STRATA == val | is.na(STRATA),]
        n_cases     <- df_sub[EVENT == 1, uniqueN(DOCTOR_ID)]
        n_controls  <- df_sub[EVENT == 0, uniqueN(DOCTOR_ID)]
        df_sub[, ID := as.integer(factor(DOCTOR_ID))]
        xformla <- ~ BIRTH_YEAR + SPECIALTY + SEX

        # att_gt
        set.seed(09152024)
        att_strata <- att_gt(
            yname         = "N",
            tname         = "T",
            idname        = "ID",
            gname         = "G",
            xformla       = xformla,
            data          = df_sub,
            est_method    = "dr",
            control_group = "notyettreated",
            clustervars   = "ID",
            pl            = TRUE,
            cores         = N_THREADS
        )

        # dynamic ATT(t)
        agg     <- aggte(att_strata, type = "dynamic", na.rm = TRUE)
        results <- data.frame(
            time = agg$egt,
            att = agg$att.egt,
            se = agg$se.egt
        )

        t0_row  <- results[results$time == 0, ]
        t0_att <- if (nrow(t0_row) > 0) t0_row$att[1] else NA_real_
        t0_se  <- if (nrow(t0_row) > 0) t0_row$se[1] else NA_real_

        stratum_result <- data.frame(
            stratum_dimension   = "Scenario",
            stratum_value       = as.character(val),
            n_cases             = n_cases,
            n_controls          = n_controls,
            drop                = round(t0_att, 5),
            se_drop             = round(t0_se, 5),
            stringsAsFactors    = FALSE
        )

        group_results[[length(group_results) + 1]] <- stratum_result

        # Save long results
        results_long <- data.frame(
            stratum_dimension = "Scenario",
            stratum_value     = as.character(val),
            time              = results$time,
            att               = results$att,
            se                = results$se,
            stringsAsFactors  = FALSE
        )
        group_results_long[[length(group_results_long) + 1]] <- results_long

    }, error = function(e) {
        cat(sprintf("    ERROR for STRATA = '%s': %s\n", val, conditionMessage(e)))

        df_sub <- df[STRATA == val]
        n_cases    <- df_sub[EVENT == 1, uniqueN(DOCTOR_ID)]
        n_controls <- df_sub[EVENT == 0, uniqueN(DOCTOR_ID)]

        stratum_result <- data.frame(
            stratum_dimension   = "Scenario",
            stratum_value       = as.character(val),
            n_cases             = n_cases,
            n_controls          = n_controls,
            drop                = NA_real_,
            se_drop             = NA_real_,
            stringsAsFactors    = FALSE
        )
        group_results[[length(group_results) + 1]] <- stratum_result
    })
}

# Save Scenario stratification results
if (length(group_results) > 0) {
    group_results_df <- do.call(rbind, group_results)
    rownames(group_results_df) <- NULL
    group_file <- file.path(outdir, paste0("Supplements_DepressionBurnout_Scenario_", TODAY, ".csv"))
    write.csv(group_results_df, group_file, row.names = FALSE)
}

# Save Scenario stratification long results
if (length(group_results_long) > 0) {
    group_results_long_df <- do.call(rbind, group_results_long)
    rownames(group_results_long_df) <- NULL
    group_long_file <- file.path(outdir, paste0("Supplements_DepressionBurnout_Scenario_Long_", TODAY, ".csv"))
    write.csv(group_results_long_df, group_long_file, row.names = FALSE)
}

# -- Plot --
# Reload the results to plot, if running this script in a separate session
results_plot <- read.csv(group_long_file)
data_plot <- results_plot %>% filter(time >= -WIN & time <= WIN)

# Build subtitle dynamically from however many strata were successfully fit
subtitle_text <- paste(
    sprintf(
        "%s | Cases: %d, Controls: %d",
        group_results_df$stratum_value,
        group_results_df$n_cases,
        group_results_df$n_controls
    ),
    collapse = "\n"
)

p <- ggplot(data_plot, aes(x = time, y = att, color = stratum_value, group = stratum_value)) +
    geom_line(linewidth = 0.8, position = position_dodge(width = 0.3)) +
    geom_point(size = 2, position = position_dodge(width = 0.3)) +
    geom_errorbar(
        aes(ymin = att - 1.96 * se, ymax = att + 1.96 * se),
        width = 0.2, position = position_dodge(width = 0.3)
    ) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "grey") +
    geom_vline(xintercept = 0, linetype = "dashed", color = "grey") +
    labs(
        title = paste0("Results for: ", PHENOTYPE$name),
        subtitle = subtitle_text,
        x = "Years from Event",
        y = "change in total number of prescriptions",
        color = "Scenario"
    ) +
    scale_x_continuous(breaks = -WIN:WIN) +
    theme_minimal()

out_plot_file <- file.path(outdir, paste0("Plot_Supplements_DepressionBurnout_Scenario_", TODAY, ".png"))
ggsave(filename = out_plot_file, plot = p, width = 12, height = 10, dpi = 300)


# --------------------------------------------------------------------------------------------
# Before was recurrent depression, after is distress analysis
# --------------------------------------------------------------------------------------------

# ============================================================
# 1. Libraries
# ============================================================
.libPaths("/shared-directory/sd-tools/apps/R/lib/")
suppressPackageStartupMessages({
    library(data.table)
    library(arrow)
    library(dplyr)
    library(tidyr)
    library(lubridate)
    library(did)
    library(metafor)
    library(ggplot2)
})


# ============================================================
# 2. File paths and global settings
# ============================================================

DATE_DATA_1  <- "20260709"
DATE_DATA_2  <- "20260219"

doctor_list         <- "/media/volume/Projects/DSGELabProject1/doctors_20250424.csv"
events_file         <- paste0("/media/volume/Projects/DSGELabProject1/ProcessedData/AllDistressEvents_", DATE_DATA_1, ".parquet")
sick_leave_file     <- paste0("/media/volume/Projects/DSGELabProject1/ProcessedData/all_sickleaves_doctors_", DATE_DATA_1, ".parquet")
outcomes_file       <- paste0("/media/volume/Projects/DSGELabProject1/DiD_Experiments/DiD_Diagnosis_",DATE_DATA_2, "/ProcessedOutcomes_", DATE_DATA_2, "/processed_outcomes.parquet")
covariates_file     <- "/media/volume/Projects/DSGELabProject1/doctor_characteristics_20250520.csv"

# Window size for plot
WIN <- 3

PHENOTYPE <- list(
    name = "Distress (Wide)",
    case_incl = c("F32 ", "F41", "F43", "F51", "Z73"),
    case_excl = c("F33"),
    control_excl = c("F32", "F33", "F41", "F43", "F51", "Z73")
)

TODAY <- format(Sys.time(), "%Y%m%d")
outdir   <- paste0("/media/volume/Projects/DSGELabProject1/Plots/Supplements/Supplements_DepressionBurnout_SickLeaveScenarios_Distress_", TODAY, "/")
if (!dir.exists(outdir)) dir.create(outdir, recursive = TRUE)

N_THREADS  <- 10
setDTthreads(N_THREADS)

# ============================================================
# 3. Load shared data
# ============================================================

doctor_ids <- fread(doctor_list, header = FALSE)$V1

# Covariates: keep specialty and birth year
covariates <- fread(covariates_file)
covariates[, `:=`(
    SPECIALTY  = as.character(INTERPRETATION),
    BIRTH_YEAR = as.numeric(substr(BIRTH_DATE, 1, 4)),
    BIRTH_DATE = NULL,
    INTERPRETATION = NULL
)]
covariates[SPECIALTY == "", SPECIALTY := "No specialty"]

# Outcomes: total number of prescriptions per doctor per year
outcomes <- as.data.table(read_parquet(outcomes_file, col_select = c("DOCTOR_ID", "YEAR", "N")))

# ============================================================
# 4. Extract events, pick the code of interest, and define the
#    three comparison scenarios
#
#   Because this phenotype spans several ICD-10 3-char codes
#   (F32, F41, F43, F51, Z73), a doctor may qualify through more
#   than one code. We first collapse to a single "code of interest"
#   per doctor (priority: both dates available > diagnosis only >
#   sick leave only, then earliest date), exactly as before.
#
#   On top of that single selected record we then define:
#   A. Diagnosis in care register, but NEVER took a sick leave
#   B. Diagnosis in care register AND took a sick leave
#      -> split by time distance between diagnosis and sick leave:
#         "before diagnosis" (distance < 0)
#         "immediate"         (0 <= distance <= 7)
#         "within a year"     (7 < distance <= 365)
#         "over a year later" (distance > 365)
#   C. NO diagnosis in care register, but a sick leave is recorded
# ============================================================

# Load events and keep only Depression/Burnout codes
events_raw <- as.data.table(read_parquet(events_file))
events_raw[, DATE := as.Date(DATE)]
events_raw[, CODE := (CODE_ICD10)]

# Extract ids of doctors that will be included / excluded in the cohort
events_raw[, CODE := ifelse(
    nchar(CODE) >= 4 & substr(CODE, 4, 4) != ".", # QC: add dot after 3 char if not there
    paste0(substr(CODE, 1, 3), ".", substr(CODE, 4, nchar(CODE))),
    CODE
)]
case_incl_ids       <- events_raw[grepl(paste0("^(", paste(PHENOTYPE$case_incl, collapse = "|"), ")"), CODE), unique(DOCTOR_ID)]
case_excl_ids       <- events_raw[grepl(paste0("^(", paste(PHENOTYPE$case_excl, collapse = "|"), ")"), CODE), unique(DOCTOR_ID)]
control_excl_ids    <- events_raw[grepl(paste0("^(", paste(PHENOTYPE$control_excl, collapse = "|"), ")"), CODE), unique(DOCTOR_ID)]

# Extract cases for the phenotype
events_raw <- events_raw[DOCTOR_ID %in% case_incl_ids]
events_raw <- events_raw[!(DOCTOR_ID %in% case_excl_ids)]

# Pick the first record for each code used to select cases for each doctor
events_raw <- events_raw[order(DOCTOR_ID, CODE_ICD10_3CHAR, SOURCE, DATE)][, .SD[1], by = c("DOCTOR_ID", "CODE_ICD10_3CHAR", "SOURCE")]

# Go from long format to wide format
# Include the CODE column in the values so we keep which code was matched for each source
events_raw <- pivot_wider(
    as.data.frame(events_raw),
    id_cols = c("DOCTOR_ID", "CODE_ICD10_3CHAR"),
    names_from = "SOURCE",
    values_from = "DATE",
    values_fill = NA
) %>% as.data.table()

setnames(events_raw, c("CareRegister", "SickLeaveRegister"), c("DATE_CareRegister", "DATE_SickLeaveRegister"))

# Drop rows where neither a diagnosis nor a sick leave date is available for that code
events_raw <- events_raw[!(is.na(DATE_CareRegister) & is.na(DATE_SickLeaveRegister))]

# ------------------------------------------------------------
# Extra step (specific to this wide phenotype): 
# pick a single "code of interest" per doctor across all candidate ICD codes.
# Priority: both dates available > diagnosis only > sick leave only;
# ties broken by earliest date.

# NOTE: 
# The priority order picked means that:
# if a doctor has an earlier F41 diagnosis with no sick leave and a later F43 diagnosis that does have a sick leave, the priority step will pick the F43 record,
# so that doctor lands in scenario B, not A, even though a diagnosis-only event happened first chronologically. 
# ------------------------------------------------------------
events_raw[, priority := fcase(
    !is.na(DATE_CareRegister) & !is.na(DATE_SickLeaveRegister), 1L,
    !is.na(DATE_CareRegister), 2L,
    default = 3L
)]
events_raw <- events_raw[order(DOCTOR_ID, priority, DATE_CareRegister, DATE_SickLeaveRegister)][, .SD[1], by = "DOCTOR_ID"]
events_raw[, priority := NULL]

# Merge in sick leave benefit-type information, for descriptive purposes only
sl <- as.data.table(read_parquet(sick_leave_file))
events_merged <- merge(
    events_raw,
    sl,
    by.x = c("DOCTOR_ID", "DATE_SickLeaveRegister"),
    by.y = c("DOCTOR_ID", "SVA_DATE"),
    all.x = TRUE
)
events_merged[, TYPE := fcase(
    BENEFIT_TYPE == 73, "partial",
    BENEFIT_TYPE == 74, "full",
    default = NA_character_
)]

# ------------------------------------------------------------
# Define scenario (A / B / C) based on the selected record
# ------------------------------------------------------------
events_merged[, SCENARIO := fcase(
    !is.na(DATE_CareRegister) &  is.na(DATE_SickLeaveRegister), "A",
    !is.na(DATE_CareRegister) & !is.na(DATE_SickLeaveRegister), "B",
     is.na(DATE_CareRegister) & !is.na(DATE_SickLeaveRegister), "C"
)]

# Distance (days) between sick leave and diagnosis - only meaningful for scenario B
events_merged[, distance_days := as.numeric(difftime(DATE_SickLeaveRegister, DATE_CareRegister, units = "days"))]

# Sub-group scenario B by time distance between diagnosis and sick leave
events_merged[, GROUP := fcase(
    SCENARIO == "A", "no sick leave",
    # SCENARIO == "B" & distance_days <  0,                          "before diagnosis",
    SCENARIO == "B" & distance_days >= 0   & distance_days <= 7,    "immediate",
    SCENARIO == "B" & distance_days >  7   & distance_days <= 365,  "within a year",
    # SCENARIO == "B" & distance_days >  365,                         "over a year later",
    SCENARIO == "C", "no diagnosis",
    default = NA_character_
)]

# Combined label used for stratification (keeps scenario A/B/C explicit)
events_merged[, STRATA_LABEL := fcase(
    SCENARIO == "A", "A: no sick leave",
    SCENARIO == "B", paste0("B: ", GROUP),
    SCENARIO == "C", "C: no diagnosis",
    default = NA_character_
)]

# Event date used for the DiD design:
#  - scenarios A & B: first diagnosis date (Care register)
#  - scenario C: first sick leave date (no diagnosis date is available)
events_merged[, EVENT_DATE := fifelse(SCENARIO == "C", DATE_SickLeaveRegister, DATE_CareRegister)]

# Count number of doctors in each scenario / group (and which code was selected)
group_counts <- events_merged[, .N, by = .(SCENARIO, GROUP, CODE_ICD10_3CHAR)]
cat("Number of doctors in each scenario/group (by selected code):\n")
print(group_counts)

# filter doctors in our cohort, then finalize data
events_doctors <- events_merged[DOCTOR_ID %in% doctor_ids]
events_doctors <- events_doctors[, .(DOCTOR_ID, EVENT_DATE, STRATA_LABEL)]

cat(sprintf("doctors with %s event: %d\n", PHENOTYPE$name, nrow(events_doctors)))

# ============================================================
# 5. Merge events, outcomes and covariates & QC steps
# ============================================================

# Left join: all outcome rows kept; controls get NA event date / strata
df <- left_join(outcomes, events_doctors, by = "DOCTOR_ID") %>%
    mutate(
        EVENT      = if_else(!is.na(EVENT_DATE), 1L, 0L),
        EVENT_YEAR = if_else(!is.na(EVENT_DATE), as.numeric(format(EVENT_DATE, "%Y")), NA_real_)
    ) %>%
    select(-EVENT_DATE) %>%
    as.data.table()

# Merge covariates
df <- covariates[df, on = "DOCTOR_ID"]
df[, `:=`(
    AGE          = YEAR - BIRTH_YEAR,
    AGE_AT_EVENT = fifelse(is.na(EVENT_YEAR), NA_real_, EVENT_YEAR - BIRTH_YEAR)
)]

# Remove doctors whose event occurred after pension age (60)
ids_post60 <- df[AGE_AT_EVENT > 60 & !is.na(AGE_AT_EVENT), unique(DOCTOR_ID)]
df <- df[!(DOCTOR_ID %in% ids_post60) & AGE <= 60]

# Replace missing prescription counts with 0
df[is.na(N), N := 0]

# Remove doctors from controls based on phenotype exclusion criteria
df <- df[!(EVENT == 0 & DOCTOR_ID %in% control_excl_ids),]

# --- DiD variables: numeric ID, group (first treatment year), calendar year ---
df[, ID := as.integer(factor(DOCTOR_ID))]
df[, G  := fifelse(is.na(EVENT_YEAR), 0, EVENT_YEAR)]
df[, T  := YEAR]

# STRATA is simply the scenario/group label already computed above (NA for controls)
df[, STRATA := STRATA_LABEL]


# ============================================================
# 6. Stratified DiD across scenarios A / B (sub-groups) / C
# ============================================================

group_results <- list()
group_results_long <- list()

# Every case doctor falls into exactly one of these six mutually-exclusive strata;
# controls (STRATA == NA) are shared as the comparison group for every stratum.
strata_values <- c(
    "A: no sick leave",
    #"B: before diagnosis",
    "B: immediate",
    "B: within a year",
    #"B: over a year later",
    "C: no diagnosis"
)

for (val in strata_values) {
    cat(sprintf("  Fitting: STRATA = '%s'\n", val))

    tryCatch({

        # Subset: this stratum's cases + all controls
        df_sub      <- df[STRATA == val | is.na(STRATA),]
        n_cases     <- df_sub[EVENT == 1, uniqueN(DOCTOR_ID)]
        n_controls  <- df_sub[EVENT == 0, uniqueN(DOCTOR_ID)]
        df_sub[, ID := as.integer(factor(DOCTOR_ID))]
        xformla <- ~ BIRTH_YEAR + SPECIALTY + SEX

        # att_gt
        set.seed(09152024)
        att_strata <- att_gt(
            yname         = "N",
            tname         = "T",
            idname        = "ID",
            gname         = "G",
            xformla       = xformla,
            data          = df_sub,
            est_method    = "dr",
            control_group = "notyettreated",
            clustervars   = "ID",
            pl            = TRUE,
            cores         = N_THREADS
        )

        # dynamic ATT(t)
        agg     <- aggte(att_strata, type = "dynamic", na.rm = TRUE)
        results <- data.frame(
            time = agg$egt,
            att = agg$att.egt,
            se = agg$se.egt
        )

        t0_row  <- results[results$time == 0, ]
        t0_att <- if (nrow(t0_row) > 0) t0_row$att[1] else NA_real_
        t0_se  <- if (nrow(t0_row) > 0) t0_row$se[1] else NA_real_

        stratum_result <- data.frame(
            stratum_dimension   = "Scenario",
            stratum_value       = as.character(val),
            n_cases             = n_cases,
            n_controls          = n_controls,
            drop                = round(t0_att, 5),
            se_drop             = round(t0_se, 5),
            stringsAsFactors    = FALSE
        )

        group_results[[length(group_results) + 1]] <- stratum_result

        # Save long results
        results_long <- data.frame(
            stratum_dimension = "Scenario",
            stratum_value     = as.character(val),
            time              = results$time,
            att               = results$att,
            se                = results$se,
            stringsAsFactors  = FALSE
        )
        group_results_long[[length(group_results_long) + 1]] <- results_long

    }, error = function(e) {
        cat(sprintf("    ERROR for STRATA = '%s': %s\n", val, conditionMessage(e)))

        df_sub <- df[STRATA == val]
        n_cases    <- df_sub[EVENT == 1, uniqueN(DOCTOR_ID)]
        n_controls <- df_sub[EVENT == 0, uniqueN(DOCTOR_ID)]

        stratum_result <- data.frame(
            stratum_dimension   = "Scenario",
            stratum_value       = as.character(val),
            n_cases             = n_cases,
            n_controls          = n_controls,
            drop                = NA_real_,
            se_drop             = NA_real_,
            stringsAsFactors    = FALSE
        )
        group_results[[length(group_results) + 1]] <- stratum_result
    })
}

# Save Scenario stratification results
if (length(group_results) > 0) {
    group_results_df <- do.call(rbind, group_results)
    rownames(group_results_df) <- NULL
    group_file <- file.path(outdir, paste0("Supplements_DepressionBurnout_Scenario_Distress_", TODAY, ".csv"))
    write.csv(group_results_df, group_file, row.names = FALSE)
}

# Save Scenario stratification long results
if (length(group_results_long) > 0) {
    group_results_long_df <- do.call(rbind, group_results_long)
    rownames(group_results_long_df) <- NULL
    group_long_file <- file.path(outdir, paste0("Supplements_DepressionBurnout_Scenario_Distress_Long_", TODAY, ".csv"))
    write.csv(group_results_long_df, group_long_file, row.names = FALSE)
}

# -- Plot --
# Reload the results to plot, if running this script in a separate session
results_plot <- read.csv(group_long_file)
data_plot <- results_plot %>% filter(time >= -WIN & time <= WIN)

# Build subtitle dynamically from however many strata were successfully fit
subtitle_text <- paste(
    sprintf(
        "%s | Cases: %d, Controls: %d",
        group_results_df$stratum_value,
        group_results_df$n_cases,
        group_results_df$n_controls
    ),
    collapse = "\n"
)

p <- ggplot(data_plot, aes(x = time, y = att, color = stratum_value, group = stratum_value)) +
    geom_line(linewidth = 0.8, position = position_dodge(width = 0.3)) +
    geom_point(size = 2, position = position_dodge(width = 0.3)) +
    geom_errorbar(
        aes(ymin = att - 1.96 * se, ymax = att + 1.96 * se),
        width = 0.2, position = position_dodge(width = 0.3)
    ) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "grey") +
    geom_vline(xintercept = 0, linetype = "dashed", color = "grey") +
    labs(
        title = paste0("Results for: ", PHENOTYPE$name),
        subtitle = subtitle_text,
        x = "Years from Event",
        y = "change in total number of prescriptions",
        color = "Scenario"
    ) +
    scale_x_continuous(breaks = -WIN:WIN) +
    theme_minimal()

out_plot_file <- file.path(outdir, paste0("Plot_Supplements_DepressionBurnout_Scenario_Distress_", TODAY, ".png"))
ggsave(filename = out_plot_file, plot = p, width = 12, height = 10, dpi = 300)