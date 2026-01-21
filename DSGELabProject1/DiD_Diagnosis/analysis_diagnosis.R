.libPaths("/shared-directory/sd-tools/apps/R/lib/")

#### Libraries:
suppressPackageStartupMessages({
    library(data.table)
    library(dplyr)
    library(tidyr)
    library(lubridate)
    library(arrow)
    library(did)
})

##### Arguments:
args = commandArgs(trailingOnly = TRUE)
doctor_list = args[1]
events_file = args[2]
event_code = args[3]
outcomes_file = args[4]
covariates_file = args[5]
outfile = args[6]

#### Main:
N_THREADS = 10
setDTthreads(N_THREADS) 

# STEP 1: Load data

# 1. list of doctors and covariates
doctor_ids = fread(doctor_list, header = FALSE)$V1
covariates = fread(covariates_file)
# 2. events
events = as.data.table(read_parquet(events_file))
event_code_parts = strsplit(event_code, "_")[[1]]
event_source = event_code_parts[1] # Diag or Purch
event_actual_code = event_code_parts[2]
# Filter events based on the event code
events = events[SOURCE == event_source & startsWith(as.character(CODE), event_actual_code), ]
event_ids = intersect(unique(events$PATIENT_ID), doctor_ids)
control_ids <- setdiff(doctor_ids, event_ids)
# 3. outcomes
outcomes = as.data.table(read_parquet(outcomes_file))
outcomes = outcomes[DOCTOR_ID %in% doctor_ids,] # QC : only selected doctors 

# STEP 2: Process and merge events, outcomes & covariates

events = events[, .(PATIENT_ID, CODE, DATE)]
setnames(events, "PATIENT_ID", "DOCTOR_ID")
# QC: Keep only the first event per DOCTOR_ID, in case multiple codes exist
events = events[order(DOCTOR_ID, DATE)]
events = events[, .SD[1], by = DOCTOR_ID]

df_merged = events[outcomes, on = "DOCTOR_ID", allow.cartesian = TRUE]
df_merged[, DATE := as.Date(DATE)]
df_merged[, EVENT := ifelse(!is.na(DATE), 1, 0)]
df_merged[, EVENT_YEAR := ifelse(!is.na(DATE), as.numeric(format(DATE, "%Y")), NA_real_)]
df_merged[, DATE := NULL]

# Prepare covariates 
covariates[, `:=`(
    SPECIALTY = as.character(INTERPRETATION),
    BIRTH_YEAR = as.numeric(substr(BIRTH_DATE, 1, 4))
)]
covariates[, `:=`(
    BIRTH_DATE = NULL, 
    INTERPRETATION = NULL
)]

# Merge covariates
df_complete = covariates[df_merged, on = "DOCTOR_ID"]
df_complete[, `:=`(
    AGE = YEAR - BIRTH_YEAR,
    AGE_IN_2023 = 2023 - BIRTH_YEAR,
    AGE_AT_EVENT = fifelse(is.na(EVENT_YEAR), NA_real_, EVENT_YEAR - BIRTH_YEAR)
)]

# STEP 3: Model Data Preparation

# Filter out events after pension, and prescriptions after pension
PENSION_AGE = 60
events_after_pension = df_complete[AGE_AT_EVENT > PENSION_AGE & !is.na(AGE_AT_EVENT), unique(DOCTOR_ID)]
df_complete = df_complete[!(DOCTOR_ID %in% events_after_pension) & AGE <= PENSION_AGE]
# final model data
df_model <- as.data.table(df_complete)[
    , `:=`(
        SPECIALTY = factor(SPECIALTY, levels = c("", setdiff(unique(df_complete$SPECIALTY), ""))),
        SEX = factor(SEX, levels = c(1, 2), labels = c("Male", "Female"))
    )
]
# Replace missing N values with 0s 
df_model[is.na(N), N := 0]
# prepare variables as requested by did package
df_model$ID <- as.integer(factor(df_model$DOCTOR_ID))                      
df_model$G <- ifelse(is.na(df_model$EVENT_YEAR), 0, df_model$EVENT_YEAR)    
df_model$T <- df_model$YEAR    

# Calculate number of cases and controls
n_cases <- length(unique(df_model[df_model$EVENT == 1, DOCTOR_ID]))
n_controls <- length(unique(df_model[df_model$EVENT == 0, DOCTOR_ID]))
cat(paste0("Cases: ", n_cases, "\n"))
cat(paste0("Controls: ", n_controls, "\n"))

# For cases, count number of unique doctors per event cohorts (i.e., number of events per year)
events_per_year <- df_model[df_model$EVENT == 1, .(N = uniqueN(DOCTOR_ID)), by = EVENT_YEAR][order(EVENT_YEAR)]
events_year_str <- paste0(events_per_year$EVENT_YEAR, ":", events_per_year$N, collapse = ", ")
cat("Events per year: [", events_year_str, "]\n")

# STEP 4: DiD Analysis using 'did' package

att_gt_res <- att_gt(
    yname = "N",
    tname = "T",
    idname = "ID",
    gname = "G",
    xformla = ~ BIRTH_YEAR + SEX + SPECIALTY,
    data = df_model,
    est_method = "dr",                      # doubly robust (for covariate adj.)
    control_group = "notyettreated",        # use not-yet-treated as control group
    clustervars = "ID",
    pl = TRUE,                              # parallel processing
    cores = N_THREADS
)

agg_dynamic <- aggte(att_gt_res, type = "dynamic", na.rm = TRUE)
results <- data.frame(
    time = agg_dynamic$egt,
    att = agg_dynamic$att.egt,
    se = agg_dynamic$se.egt
)

# For diagnosis results will only consider ATT and SE at event
effect_at_event <- results$att[results$time == 0]
se_at_event <- results$se[results$time == 0]

# ============================================================================
# 3. EXPORT RESULTS TO CSV
# ============================================================================

# Append summary row to outfile
summary_row <- data.frame(
        event_code = event_code,
        drop = effect_at_event,
        se = se_at_event,
        n_cases = n_cases,
        n_controls = n_controls
)

write.table(
        summary_row,
        file = outfile,
        sep = ",",
        row.names = FALSE,
        col.names = FALSE,
        append = TRUE
)
