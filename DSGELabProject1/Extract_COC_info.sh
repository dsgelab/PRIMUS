#### Info
# This script extracts the Bice-Boxerman Continuity of Care (COC) information for each available patient in the dataset,
# starting from the output from Doctor_Patient_Longitudinal.py

# Start timer
START_TIME=$(date +%s)

# Input and output files
INPUT_FILE="/media/volume/Projects/DSGELabProject1/doctor_patient_longitudinal_20250220.csv.gz"
OUTPUT_FILE="/media/volume/Projects/DSGELabProject1/patient_COC_info_20250226.csv"

# Sort Input File by Patient ID and Pipe to AWK for extracting COC
zcat "$INPUT_FILE" | sort -t, -k2,2 -T /media/volume/Projects/DSGELabProject1/ | awk '
BEGIN {
    FS = ",";
    OFS = ",";
    print "PATIENT_ID", "TOTAL_DOCTORS", "TOTAL_VISITS", "COC";
}

{
    # Check if we switched to next patient
    if (current_patient != $2 && current_patient != "") {
        # Calculate COC for the current patient
        calculate_COC();
        
        # Reset for the next patient
        delete visits;
        total_visits = 0;
        total_doctors = 0;
    }
    
    # Accumulate data for the current patient
    current_patient = $2;
    visits[$1]++;
    total_visits++;
}

# Function to calculate and print COC for a given patient
function calculate_COC() {
    N = total_visits;
    if (N <= 1) {
        COC = 0;
        total_doctors = 1;
    } else {
        sum_sq = 0;
        for (doctor in visits) {
            sum_sq += visits[doctor] * visits[doctor];
            total_doctors++;
        }
        COC = (sum_sq - N) / (N * (N - 1));
    }
    
    # Output the result for the current patient
    print current_patient, total_doctors, total_visits, COC;
}

END {
    # Calculate COC for the last patient in the file
    calculate_COC();
}' > "$OUTPUT_FILE"

# End timer
END_TIME=$(date +%s)
ELAPSED_TIME=$((END_TIME - START_TIME))

echo "values for COC calculation saved to $OUTPUT_FILE"
echo "Execution time: $ELAPSED_TIME seconds"