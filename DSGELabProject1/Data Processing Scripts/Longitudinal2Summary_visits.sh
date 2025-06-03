#### Info 
# This script extracts summary information about visits for each available doctor in the dataset,
# starting from the output from Doctor_Patient_Longitudinal.py

# Start timer
START_TIME=$(date +%s)

# Input and output files
INPUT_FILE="/media/volume/Projects/DSGELabProject1/ProcessedData/doctor_patient_longitudinal_VISIT_20250603.csv.gz"
OUTPUT_FILE="/media/volume/Projects/DSGELabProject1/doctor_patient_summary_VISIT_20250603.csv"

# Run AWK to process the file by year
zcat "$INPUT_FILE" | awk -F',' -v OUTFILE="$OUTPUT_FILE" '
{
    split($4, date_parts, "-")
    year = date_parts[1]  # Extract year from DATE (YY-mm-dd)
    key = $1 "_" year  # Composite key: DOCTOR_ID_YEAR

    total_patients[key]++
    unique_patients[key][$2] = 1

    if ($3 == "Prescription" || $3 == "Diagnosis Avohilmo" || $3 == "Diagnosis Hilmo") {
        register_counts[key][$3]++
    }

    if (($1 == $2) && ($3 == "Prescription")) {
        self_prescriptions[key]++
    }

    if (($1 == $2) && ($3 == "Diagnosis Avohilmo" || $3 == "Diagnosis Hilmo")) {
        self_diagnosis[key]++
    }
}
END {
    print "DOCTOR_ID,YEAR,TotalPatients,UniquePatients,Prescriptions,DiagnosisAvohilmo,DiagnosisHilmo,SelfPrescription,SelfDiagnosis" > OUTFILE

    for (key in total_patients) {
        split(key, arr, "_")
        doc = arr[1]
        year = arr[2]
        unique_count = length(unique_patients[key])
        printf "%s,%d,%d,%d,%d,%d,%d,%d,%d\n",
            doc,
            year,
            total_patients[key],
            unique_count,
            register_counts[key]["Prescription"]+0,
            register_counts[key]["Diagnosis Avohilmo"]+0,
            register_counts[key]["Diagnosis Hilmo"]+0,
            self_prescriptions[key]+0,
            self_diagnosis[key]+0 > OUTFILE
    }
}'

# End timer
END_TIME=$(date +%s)
ELAPSED_TIME=$((END_TIME - START_TIME))

echo "Summary saved to $OUTPUT_FILE"
echo "Execution time: $ELAPSED_TIME seconds"
