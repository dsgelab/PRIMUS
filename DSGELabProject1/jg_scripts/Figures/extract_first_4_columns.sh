#!/usr/bin/env bash
# ----------------------------------------------------------------------------
# Script Name: extract_first_4_columns.sh
# Description: Extracts only the first 4 columns from a CSV file.
#              Memory-efficient streaming version.
# ----------------------------------------------------------------------------

set -euo pipefail

####### CONFIGURATION (EDIT THESE) #######
INPUT_FILE="/XXX/path/to/atc_filtered_prescriptions_TIMESTAMP.csv.gz"
OUTPUT_FILE="/XXX/path/to/atc_filtered_4cols_TIMESTAMP.csv.gz"
###########################################

echo "Extracting first 4 columns from: $INPUT_FILE"
echo "Output to: $OUTPUT_FILE"

# Check if input is gzipped and process accordingly
if [[ "$INPUT_FILE" == *.gz ]]; then
  gzip -cd "$INPUT_FILE" | awk -F',' -v OFS=',' '{print $1, $2, $3, $4}' | gzip -c > "$OUTPUT_FILE"
else
  awk -F',' -v OFS=',' '{print $1, $2, $3, $4}' "$INPUT_FILE" | gzip -c > "$OUTPUT_FILE"
fi

echo "Done! Output written to: $OUTPUT_FILE"