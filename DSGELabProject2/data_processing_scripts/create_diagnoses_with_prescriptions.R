library(data.table)
library(dplyr)
library(tidyr)
library(lubridate)
library(optparse)
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

diagnosis_file <- get_latest_file(paste0("FirstConnected", icd10_code_no_dot, "Diagnoses")) # First diagnosis for each patient
prescription_file <- get_latest_file("J01Prescriptions")
pres_history_doc_file <- get_latest_file(paste0(icd10_code_no_dot, "DoctorPrescriptionHistory"))
diag_history_doc_file <- get_latest_file(paste0(icd10_code_no_dot, "DoctorDiagnosisHistory"))
pres_history_pat_file <- get_latest_file(paste0(icd10_code_no_dot, "PatientPrescriptionHistory"))
diag_history_pat_file <- get_latest_file(paste0(icd10_code_no_dot, "PatientDiagnosisHistory"))
doctor_file <- "/media/volume/Projects/DSGELabProject1/doctor_characteristics_20250520.csv"
patient_file <- "/media/volume/Data/Data_THL_2698_14.02.00_2023/DVV/FD_2698_Tulokset_2024-04-09_HY.csv"
city_file <- "cities.csv"

current_date <- strftime(Sys.Date(), "%Y%m%d")

prescription <- fread(prescription_file) %>%
    as_tibble() %>%
    select(PATIENT_ID, PRESCRIPTION_DATE, DOCTOR_ID)
print(paste("Number of J01 (antibiotics) prescriptions:", nrow(prescription)))

diagnosis <- fread(diagnosis_file) %>%
    as_tibble() %>%
    mutate(across(where(is.character), ~ na_if(., ""))) %>%
    select(PATIENT_ID, VISIT_DATE, ICD10_CODE, SOURCE, DOCTOR_ID) %>%
    filter(VISIT_DATE >= min(prescription$PRESCRIPTION_DATE)) # Only include patients from the same time period as prescriptions

count <- nrow(diagnosis)
print(paste("Number of patients (first diagnoses):", count))
count_with_doctor <- nrow(diagnosis %>% filter(!is.na(DOCTOR_ID)))
percentage_with_doctor <- sprintf("%.2f%%", count_with_doctor / count * 100)
print(paste0("Number of first diagnoses connected to a doctor: ", count_with_doctor, " (", percentage_with_doctor, ")"))

codes <- unique(diagnosis$ICD10_CODE)
print(paste("All ICD10 codes starting with", icd10_code, ":", paste(codes, collapse = ", ")))

pres_history_doc <- fread(pres_history_doc_file) %>% as_tibble()
diag_history_doc <- fread(diag_history_doc_file) %>% as_tibble()
pres_history_pat <- fread(pres_history_pat_file) %>% as_tibble()
diag_history_pat <- fread(diag_history_pat_file) %>% as_tibble()

city <- fread(city_file) %>% as_tibble()

patient <- fread(patient_file) %>%
    as_tibble() %>%
    rename(BIRTH_DATE = "Syntymä-päivä", SEX = "Suku-.puoli", PATIENT_ID = FID) %>%
    mutate(BIRTH_DATE = ymd(BIRTH_DATE)) %>%
    rename(CITY = "Kotikunnan.nimi") %>%
    left_join(city, by = "CITY") %>%
    rename(HOME_REGION = REGION) %>%
    mutate(BIRTH_YEAR = year(BIRTH_DATE)) %>%
    select(PATIENT_ID, BIRTH_YEAR, BIRTH_DATE, SEX, HOME_REGION)

doctor <- fread(doctor_file) %>%
    as_tibble() %>%
    rename(SPECIALTY = INTERPRETATION) %>%
    mutate(SPECIALTY = replace(SPECIALTY, SPECIALTY == "" | is.na(SPECIALTY), "No Specialty")) %>%
    rename(LANGUAGE_DOC = LANGUAGE) %>%
    select(DOCTOR_ID, PRACTICING_DAYS, SPECIALTY, LANGUAGE_DOC)


# Summarizes all diagnoses and whether a prescription was made after the diagnosis. The prescription information is
# imputed by assigning a prescription to a patient who received an antibiotic prescription from the same doctor as the
# diagnosis on the same day. Prescriptions within a week from any doctor are classified as "unclear" (excluding same-doctor
# same-day prescriptions).
prescription_rate_init <- diagnosis %>%
  left_join(prescription, by = "PATIENT_ID", suffix = c("_DIAG", "_PRES")) %>%
  # Indicator for prescriptions and unclear prescriptions
  mutate(
    UNCLEAR_OR_PRES = !is.na(PRESCRIPTION_DATE) & 
      (as.numeric(difftime(PRESCRIPTION_DATE, VISIT_DATE, units = "days")) < 7 & 
       as.numeric(difftime(PRESCRIPTION_DATE, VISIT_DATE, units = "days")) >= 0)
  ) %>%
  # Select only one row per patient, preferably the most recent prescription (if any)
  arrange(PATIENT_ID, desc(UNCLEAR_OR_PRES), PRESCRIPTION_DATE) %>%
  group_by(PATIENT_ID) %>%
  slice(1) %>%
  ungroup()

pr <- prescription_rate_init
 # Use if unknown diagnosing doctors are imputed from prescribing doctors
prescribed_condition <- pr$VISIT_DATE == pr$PRESCRIPTION_DATE & (is.na(pr$DOCTOR_ID_DIAG) | pr$DOCTOR_ID_DIAG == pr$DOCTOR_ID_PRES)
# Use if unknown diagnosing doctors are not imputed from prescribing doctors
prescribed_condition_strict <- pr$VISIT_DATE == pr$PRESCRIPTION_DATE & pr$DOCTOR_ID_DIAG == pr$DOCTOR_ID_PRES

calc_age <- function(birth_date, current_date) {
    as.numeric(difftime(current_date, birth_date, units = "days") / 365.25)
}

# Filter out unclear prescriptions from further analysis. Impute diagnosing doctor from prescribing doctor. Add
# doctor and patient characteristics.
prescription_rate <- prescription_rate_init %>%
    filter(prescribed_condition_strict | UNCLEAR_OR_PRES == 0) %>%
    rename(PRESCRIBED = UNCLEAR_OR_PRES) %>%
    mutate(PRESCRIBED = as.numeric(PRESCRIBED)) %>%
    #mutate(DOCTOR_ID = ifelse(PRESCRIBED == 1, DOCTOR_ID_PRES, DOCTOR_ID_DIAG)) %>%  # Use if unknown diagnosing doctors are imputed from prescribing doctors
    mutate(DOCTOR_ID = DOCTOR_ID_DIAG) %>% # Use if unknown diagnosing doctors are not imputed from prescribing doctors
    left_join(doctor, by = "DOCTOR_ID") %>%
    left_join(patient, by = c("DOCTOR_ID" = "PATIENT_ID")) %>%  # Doctors are also patients
    left_join(patient, by = "PATIENT_ID", suffix = c("_DOC", "_PAT")) %>%
    mutate(AGE_DOC = calc_age(BIRTH_DATE_DOC, VISIT_DATE)) %>%
    mutate(AGE_PAT = calc_age(BIRTH_DATE_PAT, VISIT_DATE)) %>%
    inner_join(diag_history_pat, by = "PATIENT_ID") %>%
    inner_join(pres_history_pat, by = "PATIENT_ID") %>%
    left_join(diag_history_doc, by = c("DOCTOR_ID", "VISIT_DATE"), suffix = c("_PAT", "_DOC")) %>%
    left_join(pres_history_doc, by = c("DOCTOR_ID", "VISIT_DATE"), suffix = c("_PAT", "_DOC"))

n_prescribed <- sum(prescription_rate$PRESCRIBED)
n_prescribed_percentage <- sprintf("%.2f%%", n_prescribed / nrow(prescription_rate) * 100)
print(paste0("Number of positive prescriptions: ", n_prescribed, " (", n_prescribed_percentage, ")"))

outfile <- paste0(icd10_code_no_dot, "DiagnosesWithPrescriptions_", current_date, ".csv")
write.csv(prescription_rate, outfile, row.names = FALSE)
print(paste0("Diagnoses with prescriptions saved to ", outfile))
