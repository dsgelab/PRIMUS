#### Info 
# This script extracts summary information about visits for each available doctor in the dataset,
# starting from the output from Doctor_Patient_Longitudinal.py

# Start timer
START_TIME=$(date +%s)

# Input and output files
INPUT_FILE="/media/volume/Projects/DSGELabProject1/ProcessedData/doctor_patient_longitudinal_VISIT_20250603.csv.gz"
OUTPUT_FILE="/media/volume/Projects/DSGELabProject1/doctor_patient_summary_VISIT_20250603.csv"

# RAM-efficient: Process one year at a time
TEMP_DIR=$(mktemp -d)
YEARS=$(seq 1998 2022)

# Initialize output file
echo "DOCTOR_ID,YEAR,TotalPatients,UniquePatients,TotalVisits,Prescriptions,DiagnosisAvohilmo,DiagnosisHilmo,SelfPrescription,SelfDiagnosis" > "$OUTPUT_FILE"

# Process each year separately
for YEAR in $YEARS; do
    echo "Processing year: $YEAR"
    
    # Extract data for this year only
    YEAR_DATA="$TEMP_DIR/year_${YEAR}.csv"
    zcat "$INPUT_FILE" | awk -F',' -v year="$YEAR" '
    {
        split($4, date_parts, "-")
        if (date_parts[1] == year) {
            print $0
        }
    }' > "$YEAR_DATA"
    
    # Process this year's data
    awk -F',' -v year="$YEAR" -v OUTFILE="$OUTPUT_FILE" '
    {
        doctor_id = $1
        patient_id = $2
        record_type = $3
        date = $4
        
        # Track total patients (all records)
        total_patients[doctor_id]++
        
        # Track unique patients
        unique_patients[doctor_id][patient_id] = 1
        
        # Track unique visits (patient + date combinations)
        visit_key = patient_id "_" date
        unique_visits[doctor_id][visit_key] = 1
        
        # Count different types of records
        if (record_type == "Prescription" || record_type == "Diagnosis Avohilmo" || record_type == "Diagnosis Hilmo") {
            register_counts[doctor_id][record_type]++
        }
        
        # Count self-prescriptions
        if ((doctor_id == patient_id) && (record_type == "Prescription")) {
            self_prescriptions[doctor_id]++
        }
        
        # Count self-diagnoses
        if ((doctor_id == patient_id) && (record_type == "Diagnosis Avohilmo" || record_type == "Diagnosis Hilmo")) {
            self_diagnosis[doctor_id]++
        }
    }
    END {
        for (doctor_id in total_patients) {
            unique_patient_count = length(unique_patients[doctor_id])
            total_visits = length(unique_visits[doctor_id])
            
            printf "%s,%s,%d,%d,%d,%d,%d,%d,%d,%d\n",
                doctor_id,
                year,
                total_patients[doctor_id],
                unique_patient_count,
                total_visits,
                register_counts[doctor_id]["Prescription"]+0,
                register_counts[doctor_id]["Diagnosis Avohilmo"]+0,
                register_counts[doctor_id]["Diagnosis Hilmo"]+0,
                self_prescriptions[doctor_id]+0,
                self_diagnosis[doctor_id]+0 >> OUTFILE
        }
    }' "$YEAR_DATA"
    
    # Clean up year file
    rm "$YEAR_DATA"
done

# Clean up temp directory
rm -rf "$TEMP_DIR"

# End timer
END_TIME=$(date +%s)
ELAPSED_TIME=$((END_TIME - START_TIME))

echo "Summary saved to $OUTPUT_FILE"
echo "Execution time: $ELAPSED_TIME seconds"
