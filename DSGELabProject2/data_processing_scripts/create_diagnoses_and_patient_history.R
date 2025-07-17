library(data.table)
library(dplyr)
library(tidyr)
library(optparse)
library(stringr)
setwd("/media/volume/Projects/mikael/ProcessedData")
source("/media/volume/Projects/mikael/utils.R")

option_list <- list(
    make_option(c("-c", "--icd10_code"), type = "character",
                help = "ICD10 code to filter diagnoses [default %default]", metavar = "character")
)
opt_parser <- OptionParser(option_list = option_list)
opt <- parse_args(opt_parser)
icd10_code <- opt$icd10_code
icd10_code_no_dot <- gsub("\\.", "", icd10_code)

all_diagnoses_file <- get_latest_file("AllConnectedDiagnoses")
all_prescriptions_file <- "/media/volume/Projects/DSGELabProject1/ProcessedData/AllConnectedPrescriptions_20250506.csv"
current_date <- strftime(Sys.Date(), "%Y%m%d")

all_diagnoses <- fread(all_diagnoses_file)[grepl("^[A-Z]", ICD10_CODE)] # Filter out improper ICD10 codes
diagnosis <- all_diagnoses[startsWith(ICD10_CODE, icd10_code) | startsWith(ICD10_CODE, icd10_code_no_dot)]
# Only select the earliest instance of diagnosis for each patient
diagnosis <- diagnosis %>%
    as_tibble() %>%
    arrange(PATIENT_ID, VISIT_DATE) %>%
    group_by(PATIENT_ID) %>%
    slice(1) %>%
    ungroup()

outfile <- paste0("FirstConnected", icd10_code_no_dot, "Diagnoses_", current_date, ".csv")
write.csv(diagnosis, outfile, row.names = FALSE)
print(paste0("Patient diagnoses saved to ", outfile))

diag_history_pat <- diagnosis %>%
    inner_join(all_diagnoses, by = "PATIENT_ID", suffix = c("_DIAG", "_HIST")) %>%
    filter(VISIT_DATE_HIST < VISIT_DATE_DIAG) %>%
    mutate(first_letter = str_sub(ICD10_CODE_HIST, 1, 1)) %>%
    distinct(PATIENT_ID, first_letter) %>%
    mutate(present = 1) %>%
    pivot_wider(
        names_from = first_letter,
        values_from = present,
        names_prefix = "HAD_",
        values_fill = 0
    ) %>%
    right_join(
        diagnosis %>% distinct(PATIENT_ID),
        by = "PATIENT_ID"
    ) %>%
    mutate(across(starts_with("HAD_"), ~ replace_na(., 0)))
diag_history_pat_outfile <- paste0(icd10_code_no_dot, "PatientDiagnosisHistory_", current_date, ".csv")
write.csv(diag_history_pat, diag_history_pat_outfile, row.names = FALSE)
print(paste0("Patient diagnosis history saved to ", diag_history_pat_outfile))
rm(all_diagnoses)

all_prescriptions <- fread(all_prescriptions_file)[grepl("^[A-Z]", ATC_CODE)] # Filter out improper ATC codes
pres_history_pat <- diagnosis %>%
    inner_join(all_prescriptions, by = "PATIENT_ID") %>%
    filter(PRESCRIPTION_DATE < VISIT_DATE) %>%
    mutate(first_letter = str_sub(ATC_CODE, 1, 1)) %>%
    distinct(PATIENT_ID, first_letter) %>%
    mutate(present = 1) %>%
    pivot_wider(
        names_from = first_letter,
        values_from = present,
        names_prefix = "GOT_",
        values_fill = 0
    ) %>%
    right_join(
        diagnosis %>% distinct(PATIENT_ID),
        by = "PATIENT_ID"
    ) %>%
    mutate(across(starts_with("GOT_"), ~ replace_na(., 0)))
pres_history_pat_outfile <- paste0(icd10_code_no_dot, "PatientPrescriptionHistory_", current_date, ".csv")
write.csv(pres_history_pat, pres_history_pat_outfile, row.names = FALSE)
print(paste0("Patient prescription history saved to ", pres_history_pat_outfile))