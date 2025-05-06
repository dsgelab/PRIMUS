library(data.table)

filename <- "/media/volume/Projects/DSGELabProject1/ProcessedData/AllConnectedDiagnosis_20250421.csv"
diagnosis <- fread(filename)
diagnosis <- diagnosis[diagnosis$ICD10_CODE == "J06.9"]
count <- nrow(diagnosis)
count_with_doctor <- nrow(diagnosis[!is.na(diagnosis$DOCTOR_ID)])
print(paste("Number of upper respiratory diseases:", count))
print(paste("Number of upper respiratory diagnoses connected to a doctor", count_with_doctor))
