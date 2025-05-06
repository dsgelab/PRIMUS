library(data.table)

filename <- "/media/volume/Projects/DSGELabProject1/ProcessedData/AllConnectedDiagnosis_20250421.csv"
diagnosis <- fread(filename)
diagnosis <- diagnosis[startsWith(diagnosis$ICD10_CODE, "J06.9")]
count <- nrow(diagnosis)
count_with_doctor <- nrow(diagnosis[!is.na(diagnosis$DOCTOR_ID)])
percentage_with_doctor <- sprintf("%.2f%%", count_with_doctor / count * 100)
print(paste("Number of upper respiratory diseases:", count))
print(paste0("Number of upper respiratory diagnoses connected to a doctor: ", count_with_doctor, " (", percentage_with_doctor, ")"))

codes <- unique(diagnosis$ICD10_CODE)
print(paste("All ICD10 codes starting with J06.9:", codes))

year_counts <- table(as.integer(format(diagnosis$DIAGNOSIS_DATE, "%Y")))
barplot(year_counts,
        main = "Number of J06.9 diagnoses per year",
        xlab = "Year",
        ylab = "Diagnoses")