.libPaths("/shared-directory/sd-tools/apps/R/lib/")
options(width = 200)

#####################################################################
# 
# Starting from: 
# processed sick-leave data (Kela SVA) 
# and processed care-register data (Hilmo + Avohilmo)
#
# Generate a longitudinal file with all relevant information 
# about distress and depression in our cohort of doctors
#
# Extra
# - Remove dot from ICD10 code if present (e.g. F32.1 -> F321)
#####################################################################

#### Libraries
suppressPackageStartupMessages({
    library(data.table)
    library(arrow)
})

#### Helper functions

# A. Remove dot from ICD10 code 
strip_dot <- function(x) gsub(".", "", as.character(x), fixed = TRUE)

# B. check if ICD10 code is valid (first char is letter, second char is digit)
is_valid_icd10 <- function(x) grepl("^[A-Za-z][0-9]", x)

#### Arguments
TODAY  <- format(Sys.Date(), "%Y%m%d")

N_THREADS <- 10
setDTthreads(N_THREADS)

ICD10_CODES_OF_INTEREST <- c(
    "F32", "F33",         # depressive disorders
    "F41", "F43",         # anxiety / stress disorders
    "F51" ,                # sleep disorders
    "Z73"                 # problems related to life-management difficulty 
)

DATE_CARE <- "20260427"   
DATE_SVA  <- "20260709"

# Input
in_dir              <- "/media/volume/Projects/DSGELabProject1/"
events_file         <- paste0(in_dir, "DiD_Experiments/DiD_Diagnosis_", DATE_CARE, "/ProcessedEvents_", DATE_CARE, "/processed_events.parquet")
doctor_list         <- paste0(in_dir, "doctors_20250424.csv")
sickleaves_file     <- paste0(in_dir, "ProcessedData/all_sickleaves_doctors_", DATE_SVA, ".parquet")

# Output
out_dir             <- "/media/volume/Projects/DSGELabProject1/ProcessedData/"
log_dir             <- "/media/volume/Projects/DSGELabProject1/Logs/"
out_file_csv        <- paste0(out_dir, "AllDistressEvents_", DATE_SVA, ".csv")
out_file_parquet    <- paste0(out_dir, "AllDistressEvents_", DATE_SVA, ".parquet")
log_file            <- paste0(log_dir, "process_distress_events_", TODAY, ".log")

#### Logging
log_con  <- file(log_file, open = "w")
sink(log_con, split = TRUE)
on.exit({
    try(sink(),         silent = TRUE)
    try(close(log_con), silent = TRUE)
}, add = TRUE)

cat(sprintf("=== Extract All Distress Events ===\n"))
cat(sprintf("Start time: %s\n", Sys.time()))
cat(sprintf("Input (Care register events):  %s\n", events_file))
cat(sprintf("Input (Sick leaves events):    %s\n", sickleaves_file))
cat(sprintf("Input (Doctor list):           %s\n\n", doctor_list))


# ============================================================
# STEP 1: Doctor list
# ============================================================

doctor_ids <- fread(doctor_list, header = FALSE)$V1
cat(sprintf("Doctors in cohort: %d\n", length(doctor_ids)))

# ============================================================
# STEP 2: Care-register (Hilmo + Avohilmo)
# ============================================================

diag_dt <- as.data.table(read_parquet(events_file))
diag_dt[, DATE := as.Date(DATE)]
setnames(diag_dt, "PATIENT_ID", "DOCTOR_ID")

# QC: filter to doctors in cohort
diag_dt <- diag_dt[DOCTOR_ID %in% doctor_ids]

# keep only ICD10 codes of interest
diag_dt[, CODE_ICD10 := strip_dot(CODE)]
diag_dt <- diag_dt[grepl(paste0("^(", paste(ICD10_CODES_OF_INTEREST, collapse = "|"), ")"), CODE_ICD10, perl = TRUE)]

# ============================================================
# STEP 3: Sick-leave register (Kela SVA)
# ============================================================

sl <- as.data.table(read_parquet(sickleaves_file))

# QC: filter to doctors in cohort
sl <- sl[DOCTOR_ID %in% doctor_ids]

# keep only ICD10 codes of interest
sl[, CODE_ICD10 := strip_dot(SICK_LEAVE_DIAG)]
sl <- sl[grepl(paste0("^(", paste(ICD10_CODES_OF_INTEREST, collapse = "|"), ")"), CODE_ICD10, perl = TRUE)]

# ============================================================
# STEP 4: Prepare datasets
# ============================================================

# clean up ICD10 codes and standardize column names for both datasets
diag_dt <- diag_dt[is_valid_icd10(CODE_ICD10)]
diag_dt[, DATE := as.Date(DATE)]
diag_dt[, SOURCE := "CareRegister"]
diag_dt <- diag_dt[, .(DOCTOR_ID, SOURCE, CODE_ICD10, DATE)]

sl <- sl[is_valid_icd10(CODE_ICD10)]
sl[, DATE := as.Date(SVA_DATE)]
sl[, SOURCE := "SickLeaveRegister"]
sl <- sl[, .(DOCTOR_ID, SOURCE, CODE_ICD10, DATE = as.Date(DATE))]

# Combine both sources
base <- rbind(diag_dt, sl)
base[, CODE_ICD10_3CHAR := substr(CODE_ICD10, 1, 3)]

# drop duplicates
base <- unique(base)

# ---- Summary -----------------------------------------------
cat(sprintf("\nTotal distress events:           %d\n", nrow(base)))
cat(sprintf("  distinct doctors:                %d\n", uniqueN(base$DOCTOR_ID)))
cat(sprintf("  distinct ICD10 codes (3-char):   %d\n", uniqueN(base$CODE_ICD10_3CHAR)))
cat(sprintf("  info from care register:         %d\n", base[SOURCE == "CareRegister", .N]))
cat(sprintf("  info from sick-leave:            %d\n", base[SOURCE == "SickLeaveRegister", .N]))

# ============================================================
# STEP 5: Persist (tagged with the diag source date)
# ============================================================

write_parquet(base, out_file_parquet)   
fwrite(base, out_file_csv)              

cat(sprintf("\nOutputs:\n  %s\n  %s\n", out_file_parquet, out_file_csv))
cat(sprintf("\nLog written to: %s\n", log_file))
cat(sprintf("End time: %s\n", Sys.time()))

sink()
close(log_con)
