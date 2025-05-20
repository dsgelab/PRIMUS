#!/bin/bash

# Define file paths (replace these with actual paths)
PATIENT_IDS_FILE="path_to_patient_ids_file.txt"
INPUT_FILE_1="path_to_input_file_1.csv"
INPUT_FILE_2="path_to_input_file_2.csv"

# Generate timestamp
TIMESTAMP=$(date +"%Y%m%d_%H%M%S")

# Define output file and log file with timestamp
OUTPUT_FILE="merged_output_file_$TIMESTAMP.csv"
LOG_FILE="script_log_$TIMESTAMP.log"

# Start logging
exec > >(tee -a "$LOG_FILE") 2>&1
echo "Script started at $(date)"

# Extract rows from INPUT_FILE_1 and INPUT_FILE_2 based on PATIENT_IDs
echo "Extracting rows based on PATIENT_IDs..."
# Determine the column index for PATIENT_ID in each file
PATIENT_ID_COLUMN_FILE_1=$(head -1 "$INPUT_FILE_1" | tr ',' '\n' | grep -nx "PATIENT_ID" | cut -d: -f1)
PATIENT_ID_COLUMN_FILE_2=$(head -1 "$INPUT_FILE_2" | tr ',' '\n' | grep -nx "PATIENT_ID" | cut -d: -f1)

# Check if column indices were found
if [[ -z "$PATIENT_ID_COLUMN_FILE_1" ]]; then
    echo "Error: PATIENT_ID column not found in INPUT_FILE_1" >&2
    exit 1
fi
if [[ -z "$PATIENT_ID_COLUMN_FILE_2" ]]; then
    echo "Error: PATIENT_ID column not found in INPUT_FILE_2" >&2
    exit 1
fi

# Log the determined column indices
echo "PATIENT_ID column index in INPUT_FILE_1: $PATIENT_ID_COLUMN_FILE_1"
echo "PATIENT_ID column index in INPUT_FILE_2: $PATIENT_ID_COLUMN_FILE_2"

# Extract rows based on PATIENT_IDs using the determined column index
awk -v col="$PATIENT_ID_COLUMN_FILE_1" -F',' 'NR==FNR {ids[$1]; next} FNR==1 || ($col in ids)' "$PATIENT_IDS_FILE" "$INPUT_FILE_1" > temp_file_1
awk -v col="$PATIENT_ID_COLUMN_FILE_2" -F',' 'NR==FNR {ids[$1]; next} FNR==1 || ($col in ids)' "$PATIENT_IDS_FILE" "$INPUT_FILE_2" > temp_file_2

# Add a source column to each file and merge them
echo "Adding source column to files..."
awk -F',' 'NR==1 {print $0, "Source"} NR>1 {print $0, "Purchase"}' OFS=',' temp_file_1 > temp_file_1_with_source
awk -F',' 'NR==1 {print $0, "Source"} NR>1 {print $0, "Prescription"}' OFS=',' temp_file_2 > temp_file_2_with_source

# Combine the files, aligning columns and filling missing values with NA
echo "Combining files and aligning columns..."
python3 <<EOF
import pandas as pd

# Read the two temp files (they already have the "Source" column)
df1 = pd.read_csv("temp_file_1_with_source")
df2 = pd.read_csv("temp_file_2_with_source")

# Stack them, unioning the columns (missing entries become NaN)
combined = pd.concat([df1, df2], ignore_index=True, sort=False)

# Write out, filling NaNs with empty string or “NA” if you prefer:
combined.fillna("", inplace=True)  # or .fillna("NA")
combined.to_csv("$OUTPUT_FILE", index=False)
EOF

# Clean up temporary files
echo "Cleaning up temporary files..."
rm temp_file_1 temp_file_2 temp_file_1_with_source temp_file_2_with_source

echo "Merging complete. Results saved to $OUTPUT_FILE."
echo "Script finished at $(date)"