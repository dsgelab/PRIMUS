library(data.table)
library(dplyr)
library(stringr)
library(ggplot2)
library(lubridate)
setwd("/media/volume/Projects/mikael/ProcessedData")
source("/media/volume/Projects/mikael/utils.R")

diagnosis_file <- get_latest_file("AllConnectedDiagnosis")
prescription_file <- "/media/volume/Projects/DSGELabProject1/ProcessedData/AllConnectedPrescriptions_20250506.csv"
current_date <- strftime(Sys.Date(), "%Y%m%d")

diagnosis <- fread(diagnosis_file) %>% as_tibble() %>%
    mutate(across(where(is.character), ~ na_if(., "PUUTTUVA"))) %>%
    mutate(across(where(is.character), ~ na_if(., ""))) %>%
    mutate(VISIT_DATE = as.IDate(VISIT_DATE))

diagnosis <- diagnosis %>%
    filter(str_starts(DIAGNOSIS_CODE, "J06.9"))
print(paste("Number of total diagnoses", nrow(diagnosis)))
# Only select the earliest instance of diagnosis for each patient
diagnosis <- diagnosis %>%
    group_by(PATIENT_ID) %>%
    arrange(VISIT_DATE) %>%
    slice(1) %>%
    ungroup()

count <- nrow(diagnosis)
print(paste("Number of first diagnoses:", count))
count_with_doctor <- nrow(diagnosis %>% filter(!is.na(DOCTOR_ID)))
percentage_with_doctor <- sprintf("%.2f%%", count_with_doctor / count * 100)
print(paste0("Number of first diagnoses connected to a doctor: ", count_with_doctor, " (", percentage_with_doctor, ")"))
write.csv(diagnosis, paste0("J069Diagnoses_", current_date, ".csv"), row.names = FALSE)

codes <- unique(diagnosis$DIAGNOSIS_CODE)
print(paste("All ICD10 codes starting with J06.9:", codes))

diagnosis %>%
    mutate(DIAGNOSIS_YEAR = format(VISIT_DATE, "%Y")) %>%
    count(DIAGNOSIS_YEAR, SOURCE) %>%
    ggplot(aes(x = DIAGNOSIS_YEAR, y = n, fill = SOURCE)) +
    geom_bar(stat = "identity") +
    scale_x_discrete(breaks = ~ levels(factor(.x))[seq(1, nlevels(factor(.x)), by = 2)]) +
    labs(title = "Number of J06.9 Diagnoses by Year and Source Dataset",
         x = "Year",
         y = "Diagnoses",
         fill = "Source dataset") +
    theme(
      plot.title = element_text(size = 28),
      axis.title = element_text(size = 24),
      axis.text = element_text(size = 20),
      legend.title = element_text(size = 22),
      legend.text = element_text(size = 20)
    )

prescription <- fread(prescription_file) %>% as_tibble() %>%
    filter(str_starts(ATC_CODE, "J01"))

# Pair diagnoses with prescriptions that have same (patient, doctor) pair
pairs <- diagnosis %>%
    filter(!is.na(DOCTOR_ID)) %>% # Just to emphasize that we are only interested in events with doctor known
    inner_join(prescription, by = c("PATIENT_ID", "DOCTOR_ID"), suffix = c("_DIAG", "_PRES"))
print(paste("Number of doctor patient pairs between diagnosis and prescription tables:", nrow(pairs)))
# Only include pairs where the prescription is made after the diagnosis and
# only the first diagnosis of each doctor-patient pair
pairs <- pairs %>%
    filter(PRESCRIPTION_DATE >= VISIT_DATE)
print(paste("Number of doctor-patient pairs where prescription is after diagnosis:", nrow(pairs)))
pairs <- pairs %>%
    group_by(PATIENT_ID, DOCTOR_ID) %>%
    arrange(PRESCRIPTION_DATE) %>%
    slice(1) %>%
    ungroup()
print(paste("Number of doctor-patient pairs where the prescription is the first after the diagnosis", nrow(pairs)))
write.csv(pairs, paste0("DoctorPatientPairsWithJ069_", current_date, ".csv"), row.names = FALSE)

pairs <- pairs %>%
    mutate(PRES_DIAG_DIFF = as.integer(difftime(PRESCRIPTION_DATE, VISIT_DATE, units = "days")))

ggplot(data.frame(x = pairs$PRES_DIAG_DIFF), aes(x)) +
    geom_histogram(bins = 100) +
    scale_x_continuous(n.breaks = 20) +
    labs(title = "Histogram of Days from Diagnosis to Prescription",
         x = "Days",
         y = "Count") +
    xlim(0, 30) +
    ylim(0, 5000)

# Select prescriptions that were made later the same week than the diagnosis or the next week
diag_pres_pairs <- pairs %>%
    filter(
        PRESCRIPTION_DATE >= VISIT_DATE &
            as.numeric(difftime(PRESCRIPTION_DATE, VISIT_DATE, units = "days")) <=
                (7 - lubridate::wday(VISIT_DATE, week_start = 1)) + 7
    )
print(paste("Number of doctor patient pairs where diagnosis (probably) led to a prescription:", nrow(diag_pres_pairs)))
write.csv(diag_pres_pairs, paste0("DiagnosesConnectedtoPrescriptions_J069_", current_date, ".csv"), row.names = FALSE)

# Include pairs where there is no doctor information but there is a visit and prescription on the same day.
# In this case, we can impute the doctor_id from the prescription table.
pairs2 <- diagnosis %>%
  filter(is.na(DOCTOR_ID)) %>%
  inner_join(prescription, by = c("PATIENT_ID", "VISIT_DATE" = "PRESCRIPTION_DATE")) %>%
  group_by(PATIENT_ID, VISIT_DATE) %>%
  arrange(VISIT_DATE) %>%
  slice(1) %>%
  ungroup()
print(paste("Number of imputed doctor prescriptions where visit and prescription are on the same day:", nrow(pairs2)))
write.csv(pairs2, paste0("DiagnosesConnectedtoPrescriptionsImputed_J069_", current_date, ".csv"), row.names = FALSE)
