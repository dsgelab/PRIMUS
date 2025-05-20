#!/bin/bash

# Start the timer
START_TIME=$(date +%s)

# Input and output file names
INPUT_FILE="/media/volume/Projects/DSGELabProject1/filtered_dp_longitudinal_20250317.csv.gz"  
TODAY=$(date +%Y-%m-%d)
OUTPUT_FILE="/media/volume/Projects/jg/filtered_dp_longitudinal_201418_${TODAY}.csv.gz"
LOG_FILE="/media/volume/Projects/jg/Logs/filter_dp_longitudinal_201418_log_${TODAY}.log"

# Ensure log directory exists
mkdir -p "$(dirname "$LOG_FILE")"

# Log start time
echo "[$(date)] Script started." | tee -a "$LOG_FILE"

# Extract header
zcat "$INPUT_FILE" | head -n 1 > header.txt

# Find column positions dynamically
DATE_COL=$(zcat "$INPUT_FILE" | head -n 1 | awk -F',' '{for (i=1; i<=NF; i++) if ($i == "DATE") print i}')
REGISTER_COL=$(zcat "$INPUT_FILE" | head -n 1 | awk -F',' '{for (i=1; i<=NF; i++) if ($i == "REGISTER") print i}')


# Check if columns were found
if [[ -z "$DATE_COL" || -z "$REGISTER_COL" ]]; then
    echo "[$(date)] Error: 'DATE' or 'REGISTER' column not found!" | tee -a "$LOG_FILE"
    exit 1
fi

echo "[$(date)] Found DATE column at position $DATE_COL and REGISTER column at position $REGISTER_COL" | tee -a "$LOG_FILE"


# Filter data based on identified columns
echo "[$(date)] Filtering data..." | tee -a "$LOG_FILE"

# Filter data based on identified columns
zcat "$INPUT_FILE" | awk -F',' -v date_col="$DATE_COL" -v reg_col="$REGISTER_COL" '
    NR==1 { next }  # Skip header
    $date_col != "" && $date_col >= "2014-01-01" && $date_col <= "2018-01-01" && ($reg_col == "Prescription" || $reg_col == "Purchase")
' | cat header.txt - | gzip > "$OUTPUT_FILE"

# Cleanup
rm header.txt

# End the timer
END_TIME=$(date +%s)
DURATION=$((END_TIME - START_TIME))


# Log completion and duration
echo "[$(date)] Filtering complete. Output saved to $OUTPUT_FILE" | tee -a "$LOG_FILE"
echo "[$(date)] Script execution time: $DURATION seconds" | tee -a "$LOG_FILE"

