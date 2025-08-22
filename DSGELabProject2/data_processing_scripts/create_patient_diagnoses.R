library(data.table)
library(dplyr)
library(tidyr)
library(optparse)
library(stringr)
setwd("/media/volume/Projects/mikael/ProcessedData")
source("/media/volume/Projects/mikael/utils.R")

option_list <- list(
    make_option(c("-c", "--icd10_code"), type = "character",
                help = "ICD10 code to filter diagnoses [default %default]", metavar = "character", default = "J06.9")
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
    filter(!is.na(DOCTOR_ID)) %>%
    arrange(PATIENT_ID, VISIT_DATE) %>%
    group_by(PATIENT_ID) %>%
    slice(1) %>%
    ungroup()

outfile <- paste0("FirstConnected", icd10_code_no_dot, "Diagnoses_", current_date, ".csv")
write.csv(diagnosis, outfile, row.names = FALSE)
print(paste0("Patient diagnoses saved to ", outfile))