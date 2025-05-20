#!/bin/bash
#------------------------------------------------------------------
# This script filters rows from a gzipped CSV file based on a list
# of patient IDs. It logs the process and outputs the filtered CSV.
#
# Requirements:
# - The input file: doctor_patient_prescpurch_20250324.csv.gz must be
#   a comma-separated CSV file with a header including "PATIENT_ID".
# - The ID file: doctor_and_spouses+children_20250305.csv must contain
#   a list of IDs (first column) to be used for filtering. If it has a
#   header, that line is skipped.
#------------------------------------------------------------------

# Set up date, log file and other key file paths
TODAY=$(date +%Y%m%d)
LOG_FILE="/media/volume/Projects/jg/Logs/filter_doctor_patient_prescpurch_relatives_${TODAY}.log"
OUT_FILE="/media/volume/Projects/jg/doctor_patient_prescpurch_relatives_${TODAY}.csv"
ID_FILE="/media/volume/Projects/DSGELabProject1/doctor_and_spouses+children_20250305.csv"
INPUT_FILE="/media/volume/Projects/DSGELabProject1/doctor_patient_prescpurch_20250324.csv.gz"

# Log start of process
echo "[$(date +"%Y-%m-%d %H:%M:%S")] Starting filter process." >> "${LOG_FILE}"
echo "ID file: ${ID_FILE}" >> "${LOG_FILE}"
echo "Input file: ${INPUT_FILE}" >> "${LOG_FILE}"
echo "Output file: ${OUT_FILE}" >> "${LOG_FILE}"

# Check if the ID file exists
if [ ! -f "${ID_FILE}" ]; then
    echo "[$(date +"%Y-%m-%d %H:%M:%S")] ERROR: ID file not found: ${ID_FILE}" >> "${LOG_FILE}"
    exit 1
fi

# Check if the input file exists
if [ ! -f "${INPUT_FILE}" ]; then
    echo "[$(date +"%Y-%m-%d %H:%M:%S")] ERROR: Input file not found: ${INPUT_FILE}" >> "${LOG_FILE}"
    exit 1
fi

# Filter the rows.
# We:
# 1. Load the list of valid patient IDs from the ID file (skipping a header if present).
# 2. Stream the gzipped CSV file via process substitution.
# 3. For the first line (header), detect the PATIENT_ID column index.
# 4. For subsequent lines, output the row if the PATIENT_ID is found in the list.
#
# Note: Adjust the FS (Field Separator) in awk if your CSV has a different format.
awk -F',' '
    BEGIN {
        # Load valid IDs from the ID file.
        id_file = "'"${ID_FILE}"'";
        while ((getline line < id_file) > 0) {
            # Skip header line if it contains a letter (e.g., "PATIENT_ID")
            if (line ~ /[A-Za-z]/ && NR==1) continue;
            split(line, fields, ",");
            valid_ids[fields[1]] = 1;
        }
        close(id_file);
    }
    NR==1 {
        # Process header to find PATIENT_ID column index
        for (i = 1; i <= NF; i++) {
            if ($i == "PATIENT_ID") {
                pid_idx = i;
                break;
            }
        }
        # If header is missing PATIENT_ID column, exit with an error.
        if (!pid_idx) {
            print "ERROR: PATIENT_ID column not found in header." > "/dev/stderr";
            exit 1;
        }
        print $0;  # Print the header line in the output.
        next;
    }
    {
        # If the PATIENT_ID in the current line is in our list, print the line.
        if ($pid_idx in valid_ids)
            print $0;
    }
' <(zcat "${INPUT_FILE}") > "${OUT_FILE}"

if [ $? -eq 0 ]; then
    echo "[$(date +"%Y-%m-%d %H:%M:%S")] Filtering completed successfully." >> "${LOG_FILE}"
    echo "Filtered data written to: ${OUT_FILE}" >> "${LOG_FILE}"
else
    echo "[$(date +"%Y-%m-%d %H:%M:%S")] ERROR: Filtering process encountered an error." >> "${LOG_FILE}"
    exit 1
fi
