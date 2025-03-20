#!/bin/bash

# Start the timer
START_TIME=$(date +%s)

# Define file names
TODAY=$(date +%Y-%m-%d)
INPUT_FILE="/media/volume/Projects/DSGELabProject1/filtered_dp_longitudinal_20250317.csv.gz"
TEMP_SORTED_FILE="/media/volume/Projects/jg/sorted_filtered_file_${TODAY}.csv"
OUTPUT_FILE="/media/volume/Projects/jg/filtered_file_first_occ_${TODAY}.csv.gz"
LOG_FILE="/media/volume/Projects/jg/Logs/filter_dp_longitudinal_first_occ_log_${TODAY}.log" 

# Ensure log directory exists
mkdir -p "$(dirname "$LOG_FILE")"


echo "[$(date)] Extracting first CODE per (DOCTOR_ID, PATIENT_ID, REGISTER), sorting by DATE..." | tee -a "$LOG_FILE"

# Extract header
zcat "$INPUT_FILE" | head -n 1 > header.txt

# Get column positions dynamically
HEADER_LINE=$(zcat "$INPUT_FILE" | head -n 1)
IFS=',' read -r -a COLUMNS <<< "$HEADER_LINE"

# Function to get column index
get_col_index() {
    for i in "${!COLUMNS[@]}"; do
        if [[ "${COLUMNS[$i]}" == "$1" ]]; then
            echo $((i + 1))
            return
        fi
    done
    echo ""
}

DATE_COL=$(get_col_index "DATE")
DOCTOR_COL=$(get_col_index "DOCTOR_ID")
PATIENT_COL=$(get_col_index "PATIENT_ID")
REGISTER_COL=$(get_col_index "REGISTER")
CODE_COL=$(get_col_index "CODE")

# Check if all required columns were found
if [[ -z "$DATE_COL" || -z "$DOCTOR_COL" || -z "$PATIENT_COL" || -z "$REGISTER_COL" || -z "$CODE_COL" ]]; then
    echo "[$(date)] Error: One or more required columns not found!" | tee -a "$LOG_FILE"
    exit 1
fi

echo "[$(date)] Found columns: DATE=$DATE_COL, DOCTOR_ID=$DOCTOR_COL, PATIENT_ID=$PATIENT_COL, REGISTER=$REGISTER_COL, CODE=$CODE_COL" | tee -a "$LOG_FILE"

# Step 1: Sort by DATE while keeping the header
(zcat "$INPUT_FILE" | tail -n +2 | sort -t ',' -k"$DATE_COL","$DATE_COL") > "$TEMP_SORTED_FILE"

echo "[$(date)] Sorted File: temporarily written to $TEMP_SORTED_FILE" | tee -a "$LOG_FILE"

# Step 2: Extract the first occurrence of CODE per (DOCTOR_ID, PATIENT_ID, REGISTER)
awk -F',' -v date_col="$DATE_COL" -v doc_col="$DOCTOR_COL" -v pat_col="$PATIENT_COL" -v reg_col="$REGISTER_COL" -v code_col="$CODE_COL" '
    BEGIN { OFS="," }
    NR==1 { next }  # Skip header
    {
        key = $doc_col "," $pat_col "," $reg_col;
        if (!(key in seen)) {
            seen[key] = 1;
            print $0;
        }
    }
' "$TEMP_SORTED_FILE" | cat header.txt - | gzip > "$OUTPUT_FILE"

# Cleanup
rm header.txt "$TEMP_SORTED_FILE"

# End the timer
END_TIME=$(date +%s)
DURATION=$((END_TIME - START_TIME))

# Log completion and duration
echo "[$(date)] Extraction complete. Output saved to $OUTPUT_FILE" | tee -a "$LOG_FILE"
echo "[$(date)] Script execution time: $DURATION seconds" | tee -a "$LOG_FILE"