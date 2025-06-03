library(data.table)
library(dplyr)
library(stringr)
library(ggplot2)
library(lubridate)
library(tidyr)
setwd("/media/volume/Projects/mikael/ProcessedData")
source("/media/volume/Projects/mikael/utils.R")

diagnosis_file <- get_latest_file("AllConnectedDiagnoses")
prescription_file <- "/media/volume/Projects/DSGELabProject1/ProcessedData/AllConnectedPrescriptions_20250506.csv"
doctor_file <- "/media/volume/Projects/DSGELabProject1/doctor_characteristics_20250520.csv"
current_date <- strftime(Sys.Date(), "%Y%m%d")

diagnosis <- fread(diagnosis_file) %>% as_tibble() %>%
    mutate(across(where(is.character), ~ na_if(., ""))) %>%
    #mutate(VISIT_DATE = as.IDate(VISIT_DATE))

diagnosis <- diagnosis %>%
    filter(str_starts(ICD10_CODE, "J06.9"))
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

codes <- unique(diagnosis$ICD_CODE)
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

diag_pres <- diagnosis %>%
    inner_join(prescription, by = "PATIENT_ID", suffix = c("_DIAG", "_PRES")) %>%
    filter(PRESCRIPTION_DATE >= VISIT_DATE) %>%
    group_by(PATIENT_ID) %>%
    arrange(PRESCRIPTION_DATE) %>%
    slice(1) %>%
    ungroup()
print(paste("Number of patients with prescription after diagnosis:", nrow(diag_pres)))

# Statistics about number of prescription. Same doctor means that the prescription and diagnosis were made by the same doctor.
within_week <- diag_pres %>%
    filter(as.numeric(difftime(PRESCRIPTION_DATE, VISIT_DATE, units = "days")) < 7)
print(paste("Number of patients with prescription after diagnosis within 7 days:", nrow(diag_pres)))
same_day <- diag_pres %>%
    filter(PRESCRIPTION_DATE == VISIT_DATE)
prescription_dfs <- list(same_day=same_day, within_week=within_week)
# Prescription stats
pstats <- expand.grid(
    PRESCRIPTION_TIME = names(prescription_dfs),
    DOCTOR_MATCH = c("same", "different", "missing")
)
for (prescription_time in names(prescription_dfs)) {
    df = prescription_dfs[[prescription_time]]
    pstats[pstats$PRESCRIPTION_TIME == prescription_time & pstats$DOCTOR_MATCH == "same", "COUNT"] = nrow(df %>% filter(DOCTOR_ID_DIAG == DOCTOR_ID_PRES))
    pstats[pstats$PRESCRIPTION_TIME == prescription_time & pstats$DOCTOR_MATCH == "different", "COUNT"] = nrow(df %>% filter(DOCTOR_ID_DIAG != DOCTOR_ID_PRES))
    pstats[pstats$PRESCRIPTION_TIME == prescription_time & pstats$DOCTOR_MATCH == "missing", "COUNT"] = nrow(df %>% filter(is.na(DOCTOR_ID_PRES)))
}
# nrow(within_week) == sum(pstats[pstats$PRESCRIPTION_TIME == "within_week", "COUNT"])
# nrow(same_day) == sum(pstats[pstats$PRESCRIPTION_TIME == "same_day", "COUNT"])
ggplot(pstats, aes(x = DOCTOR_MATCH, y = COUNT, fill = PRESCRIPTION_TIME)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  labs(
    title = "Prescription Timing by Doctor Match",
    x = "Doctor Match",
    y = "Count",
    fill = "Prescription Time"
  ) +
  theme_minimal()


doctor <- fread(doctor_file) %>% as_tibble()
# Calculate prescription rates
prescription_rate <- diagnosis %>%
    left_join(doctor, by = "DOCTOR_ID") %>%
    left_join(prescription, by = "PATIENT_ID", suffix = c("_DIAG", "_PRES")) %>%
    mutate(PRESCRIBED = as.integer(as.numeric(difftime(PRESCRIPTION_DATE, VISIT_DATE, units = "days")) < 7))


diagnosis_t <- data.frame(
    VISIT_DATE = as.Date(c("2025-01-01", "2025-01-01", "2025-05-01", "2025-05-25", "2025-05-02")),
    DOCTOR_ID = c(1, 2, 3, 4, NA),
    PATIENT_ID = c(10, 11, 12, 13, 14)
) %>% as_tibble()
doctor_t <- data.frame(
    DOCTOR_ID = c(1, 2, 4, NA),
    INTERPRETATION = c("A", "B", "B", "D")
)
prescription_t <- data.frame(
    PRESCRIPTION_DATE = as.Date(c("2025-01-01", "2025-01-07", "2025-05-08", "2025-05-24", "2025-05-02")),
    DOCTOR_ID = c(1, 2, 3, 5, NA),
    PATIENT_ID = c(10, 11, 12, 13, 15)
)
rate_t <- diagnosis_t %>%
    left_join(doctor_t, by = "DOCTOR_ID") %>%
    left_join(prescription_t, by = "PATIENT_ID", suffix = c("_DIAG", "_PRES")) %>%
    mutate(
        PRESCRIBED = as.integer(
            !is.na(PRESCRIPTION_DATE) &
            difftime(PRESCRIPTION_DATE, VISIT_DATE, units = "days") >= 0 &
            difftime(PRESCRIPTION_DATE, VISIT_DATE, units = "days") < 7
        )
    ) %>%
    mutate(INTERPRETATION = replace_na(INTERPRETATION, "other"))

rate_t_summary <- rate_t %>%
    group_by(INTERPRETATION) %>%
    summarize(
        PRESCRIBED_RATE = mean(PRESCRIBED),
        SAMPLE_SIZE = n()
    )
