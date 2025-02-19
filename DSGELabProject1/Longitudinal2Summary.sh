# Start timer
START_TIME=$(date +%s)

# Input and output files
INPUT_FILE="/media/volume/Projects/mattferr/doctor_patient_longitudinal_20250210.csv.gz"
OUTPUT_FILE="/media/volume/Projects/mattferr/doctor_patient_summary_20250210.csv"

# Run AWK to process the file
zcat "$INPUT_FILE" | awk -F',' '
{
    total_patients[$1]++                # Count total patient records per doctor
    unique_patients[$1][$2] = 1         # Track unique patients per doctor

    # Count REGISTER modalities
    if ($3 == "Purchase" || $3 == "Prescription" || $3 == "Diagnosis Avohilmo" || $3 == "Diagnosis Hilmo") {
        register_counts[$1][$3]++
    }

    # Count self-prescriptions (DOCTOR_ID == PATIENT_ID)
    if ($1 == $2) {
        self_prescriptions[$1]++
    }

    # Count visits by sector
    private_visits[$1] += $4  # Assuming column 4 represents PRIVATE visits
    public_visits[$1] += $5   # Assuming column 5 represents PUBLIC visits
}
END {
    print "DOCTOR_ID,TotalPatients,UniquePatients,Purchases,Prescriptions,DiagnosisAvohilmo,DiagnosisHilmo,SelfVisits,PrivatePrescriptions,PublicPrescriptions" > "'"$OUTPUT_FILE"'"  # Header

    for (doc in total_patients) {
        unique_count = length(unique_patients[doc])
        printf "%s,%d,%d", doc, total_patients[doc], unique_count > "'"$OUTPUT_FILE"'"

        # Print counts for each REGISTER type (default to 0 if missing)
        printf ",%d,%d,%d,%d,%d,%d,%d\n", 
            register_counts[doc]["Purchase"], 
            register_counts[doc]["Prescription"], 
            register_counts[doc]["Diagnosis Avohilmo"], 
            register_counts[doc]["Diagnosis Hilmo"], 
            self_prescriptions[doc], 
            private_visits[doc], 
            public_visits[doc] > "'"$OUTPUT_FILE"'"
    }
}' 

# End timer
END_TIME=$(date +%s)
ELAPSED_TIME=$((END_TIME - START_TIME))

echo "Summary saved to $OUTPUT_FILE"
echo "Execution time: $ELAPSED_TIME seconds"
