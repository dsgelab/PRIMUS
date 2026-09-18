.libPaths("/shared-directory/sd-tools/apps/R/lib/")

#### Libraries:
suppressPackageStartupMessages({
    library(data.table)
    library(dplyr)
    library(tidyr)
    library(lubridate)
    library(arrow)
    library(did)
    library(readr)
})

##### Arguments:
args = commandArgs(trailingOnly = TRUE)
doctor_list = args[1]
events_file = args[2]
event_code = args[3]
outcomes_file = args[4]
covariates_file = args[5]
outfile = args[6]

#### Functions:
fill_gaps_with_0s <- function(dt) {
  
    # 1. Get each doctor's follow-up window
    ranges <- dt[, .(min_year = FOLLOW_UP_START_YEAR[1], max_year = FOLLOW_UP_END_YEAR[1]), by = DOCTOR_ID]
    # 2. Build the year skeleton for each doctor
    skeleton <- ranges[, .(YEAR = seq(min_year, max_year)), by = DOCTOR_ID]
    # 3. Join original data onto the skeleton
    setkey(dt, DOCTOR_ID, YEAR)
    setkey(skeleton, DOCTOR_ID, YEAR)
    filled <- dt[skeleton]

    # 4. Zero-fill missing values (N column only)
    filled[, N := fifelse(is.na(N), 0, N)]
    # 5. Carry forward "fixed" covariate columns, but not AGE
    fixed_cols <- setdiff(names(dt), c("DOCTOR_ID", "YEAR", "N", "AGE"))
    if (length(fixed_cols) > 0) {
      filled[, (fixed_cols) := lapply(.SD, function(x) {
          val <- x[!is.na(x)][1]
          fifelse(is.na(x), val, x)
      }), .SDcols = fixed_cols, by = DOCTOR_ID]
    }
    # 6. Recompute AGE for every row
    filled[, AGE := YEAR - BIRTH_YEAR]  

  return(filled)
}

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
    BIRTH_YEAR = as.numeric(substr(BIRTH_DATE, 1, 4)),
    LICENSE_START = as.Date(START_DATE),
    LICENSE_END   = as.Date(END_DATE),
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

# Replace missing values within follow-up with 0s 
df_complete[, `:=`(
    FOLLOW_UP_START = pmax(as.Date("1998-01-01"), LICENSE_START, na.rm = TRUE),
    FOLLOW_UP_END   = pmin(as.Date("2022-12-31"), LICENSE_END, BIRTH_DATE + 60 * 365.25, DEATH_DATE, na.rm = TRUE)
)]
df_complete[, `:=`(
    FOLLOW_UP_START_YEAR = as.integer(format(FOLLOW_UP_START, "%Y")),
    FOLLOW_UP_END_YEAR   = as.integer(format(FOLLOW_UP_END, "%Y"))
)]
df_complete = fill_gaps_with_0s(df_complete)

# final model data
df_model <- as.data.table(df_complete)[
    , `:=`(
        SPECIALTY = factor(SPECIALTY, levels = c("", setdiff(unique(df_complete$SPECIALTY), ""))),
        SEX = factor(SEX, levels = c(1, 2), labels = c("Male", "Female"))
    )
]

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

set.seed(09152024)
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
se_at_event     <- results$se[results$time == 0]

# Also compute (baseline) average prescription in controls, and relative drop
baseline    <- df_model[EVENT == 0, mean(N, na.rm = TRUE)]
rel_att     <- 100 * effect_at_event / baseline
rel_att_se  <- 100 * se_at_event / baseline

# ============================================================================
# 3. EXPORT RESULTS TO CSV
# ============================================================================

# Append summary row to output file
summary_row <- data.frame(
        event_code = sub("^Diag_", "", event_code),
        drop = effect_at_event,
        se = se_at_event,
        baseline = baseline,
        rel_drop = rel_att,
        rel_drop_se = rel_att_se,
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
