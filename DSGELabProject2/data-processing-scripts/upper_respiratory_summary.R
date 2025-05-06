library(data.table)
library(dplyr)
library(stringr)
library(ggplot2)

filename <- "/media/volume/Projects/DSGELabProject1/ProcessedData/AllConnectedDiagnosis_20250421.csv"
diagnosis <- fread(filename) %>% as_tibble()

# Only select the earliest instance of diagnosis for each patient
diagnosis <- diagnosis %>%
  filter(str_starts(ICD10_CODE, "J06.9")) %>%
  group_by(PATIENT_ID) %>%
  arrange(DIAGNOSIS_DATE) %>%
  slice(1) %>%
  ungroup()

count <- nrow(diagnosis)
count_with_doctor <- nrow(diagnosis %>% filter(!is.na(DOCTOR_ID)))
percentage_with_doctor <- sprintf("%.2f%%", count_with_doctor / count * 100)
print(paste("Number of upper respiratory diseases:", count))
print(paste0("Number of upper respiratory diagnoses connected to a doctor: ", count_with_doctor, " (", percentage_with_doctor, ")"))

codes <- unique(diagnosis$ICD10_CODE)
print(paste("All ICD10 codes starting with J06.9:", codes))

diagnosis %>%
  mutate(DIAGNOSIS_YEAR = format(DIAGNOSIS_DATE, "%Y")) %>%
  count(DIAGNOSIS_YEAR, SOURCE) %>%
  ggplot(aes(x = DIAGNOSIS_YEAR, y = n, fill = SOURCE)) +
  geom_bar(stat = "identity") +
  labs(title = "Number of J06.9 Diagnoses by Year and Source Dataset",
       x = "Year",
       y = "Diagnoses",
       fill = "Source dataset") +
  theme_minimal()

