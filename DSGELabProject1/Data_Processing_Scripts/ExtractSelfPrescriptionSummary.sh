#!/bin/bash

# This script extracts a yearly prescription summary for each doctor in the dataset.
# For each doctor and year it counts:
#   PRESCRIPTION_OUT   - prescriptions the doctor issued (DOCTOR_ID == doctor)
#   PRESCRIPTION_IN    - prescriptions the doctor received as a patient (PATIENT_ID == doctor)
#   SELF_PRESCRIPTION  - prescriptions where the doctor prescribed to themselves (PATIENT_ID == DOCTOR_ID)
#   UNKNOWN_SOURCE     - prescriptions received as a patient but with a missing DOCTOR_ID
#
# Input columns (1-indexed):
#   $1 = PATIENT_ID
#   $2 = DOCTOR_ID
#   $3 = CODE
#   $4 = PRESCRIPTION_DATE  (YYYY-MM-DD)

# Start timer
START_TIME=$(date +%s)

# Input and output files
INPUT_FILE="/media/volume/Projects/DSGELabProject1/ProcessedData/imputed_prescriptions_20250501152849.csv.gz"
OUTPUT_FILE="/media/volume/Projects/DSGELabProject1/doctor_imputed_prescription_summary_20260618.csv"

echo "Started data processing at $(date)"
echo "Input file: $INPUT_FILE"
echo "Output file: $OUTPUT_FILE"

# Run AWK to process the file
echo "Processing input data..."

zcat "$INPUT_FILE" | gawk -F',' -v out_file="$OUTPUT_FILE" '
NR == 1 { next }   # Skip header row

{
    patient_id = $1
    doctor_id  = $2
    year       = substr($4, 1, 4)

    # Skip rows where year is not a 4-digit number (malformed dates)
    if (year !~ /^[0-9]{4}$/) next

    # Remember all doctor/patient IDs seen
    if (doctor_id != "")
        all_docs[doctor_id] = 1

    if (patient_id != "")
        all_docs[patient_id] = 1

    # PRESCRIPTION_OUT
    if (doctor_id != "") {
        key = doctor_id SUBSEP year
        rx_out[key]++
        years_seen[key] = 1
    }

    # PRESCRIPTION_IN / SELF_PRESCRIPTION / UNKNOWN_SOURCE
    if (patient_id != "") {

        key = patient_id SUBSEP year

        rx_in[key]++
        years_seen[key] = 1

        if (patient_id == doctor_id)
            self_rx[key]++

        if (doctor_id == "")
            unknown_src[key]++
    }
}
END {
    # Write header
    print "DOCTOR_ID,YEAR,PRESCRIPTION_OUT,PRESCRIPTION_IN,SELF_PRESCRIPTION,UNKNOWN_SOURCE" > out_file

    for (key in years_seen) {

        split(key, parts, SUBSEP)
        doc = parts[1]
        yr  = parts[2]

        printf "%s,%s,%d,%d,%d,%d\n",
            doc,
            yr,
            rx_out[key]      + 0,
            rx_in[key]       + 0,
            self_rx[key]     + 0,
            unknown_src[key] + 0 >> out_file
    }

    close(out_file)
}'

echo "Processing complete"

# End timer
END_TIME=$(date +%s)
ELAPSED_TIME=$((END_TIME - START_TIME))

echo "Execution time: $ELAPSED_TIME seconds"
