#!/usr/bin/env bash
# ----------------------------------------------------------------------------
# Script Name : filter_by_doctor_id.sh
# Description : Filters a (possibly gzipped) CSV by DOCTOR_ID, keeping only rows
#               whose DOCTOR_ID appears in a one‐column CSV list. 
#               Writes a timestamped gzipped CSV + a timestamped log file.
# Usage       : Simply edit the three "CONFIGURATION" variables below, then:
#               chmod +x filter_by_doctor_id.sh
#               ./filter_by_doctor_id.sh
# ----------------------------------------------------------------------------

set -euo pipefail
IFS=$'\n\t'

####### CONFIGURATION (EDIT ONLY THESE) #######
# 1) Path to the input CSV (or .csv.gz).
INPUT_FILE="/XXX/path/to/your/large_dataset.csv.gz"

# 2) Path to the doctor‐ID list (one column, no header):
#     e.g. a plain-text file where each line is a DOCTOR_ID to keep.
DOCTOR_LIST="/XXX/path/to/doctor_ids.csv"

# 3) Directories for logs and output. They will be created if they don't exist.
LOG_DIR="/XXX/path/to/log"
OUTPUT_DIR="/XXX/path/to/output"
###############################################

# Derived filenames / constants
BASE_NAME="filtered_by_doctor"                     # base name for intermediate files
TIMESTAMP=$(date +"%Y%m%d_%H%M%S")                  # e.g. 20250531_173045
TMP_CSV="${OUTPUT_DIR}/${BASE_NAME}_${TIMESTAMP}.csv"
OUTPUT_FILE="${TMP_CSV}.gz"
LOG_FILE="${LOG_DIR}/${BASE_NAME}_${TIMESTAMP}.log"

# Simple logger function
log(){
  echo "[$(date +"%Y-%m-%d %H:%M:%S")] $*" | tee -a "$LOG_FILE"
}

# Ensure directories exist
mkdir -p "$OUTPUT_DIR" "$LOG_DIR"

# Trap errors for logging
trap 'log "ERROR occurred at line $LINENO"; exit 1' ERR

log "START: INPUT='$INPUT_FILE' → OUTPUT='$OUTPUT_FILE'"

# Decide which reader to use (zcat if .gz, otherwise cat)
if [[ "$INPUT_FILE" == *.gz ]]; then
  reader="zcat"
else
  reader="cat"
fi

####### 1) PARSE HEADER & FIND DOCTOR_ID COLUMN INDEX #######
# Read the first line (header) of the input file to grab field names.
# We’ll split on commas, so fields must not be quoted with commas inside. 

# Use head to grab the header, then split into an array.
IFS=',' read -r -a headers < <($reader "$INPUT_FILE" | head -n1)

declare -A idx         # associative array: idx["COLUMN_NAME"]=position
for i in "${!headers[@]}"; do
  idx["${headers[$i]}"]=$((i + 1))   # store 1-based index
done

# Make sure DOCTOR_ID exists
if [[ -z "${idx[DOCTOR_ID]:-}" ]]; then
  echo "ERROR: 'DOCTOR_ID' column not found in header." >&2
  exit 1
fi

DOC_COL_INDEX=${idx[DOCTOR_ID]}
log "Found DOCTOR_ID column at index: $DOC_COL_INDEX"

####### 2) COUNT TOTAL ROWS BEFORE FILTER (OPTIONAL) #######
# Simply count lines minus one for header.
total_before=$($reader "$INPUT_FILE" | tail -n +2 | wc -l)
log "Rows before filter: $total_before"

####### 3) BUILD FILTERED CSV VIA STREAMING + AWK #######
# We’ll:
#   • print the header unchanged
#   • load the doctor list into an awk array (ids[...] = 1)
#   • for each subsequent row, check if $DOC_COL_INDEX is in ids, and if so, print the line.
#
# Note:
#   – We explicitly keep the field‐separator as comma (FS=","), which assumes no embedded commas in fields.
#   – If your CSV has quoted strings with commas inside, you’d need a full CSV parser instead.
#   – The doctor list is assumed small enough that awk’s in-memory array is fine.

log "Beginning filtering pass..."

# 3a) Print the header unchanged into TMP_CSV
set +o pipefail
$reader "$INPUT_FILE" | head -n1 > "$TMP_CSV"
set -o pipefail

# 3b) Stream the rest through awk:
$reader "$INPUT_FILE" \
  | tail -n +2 \
  | awk -F',' -v OFS=',' -v col="$DOC_COL_INDEX" -v listfile="$DOCTOR_LIST" '
    BEGIN {
      # Load doctor IDs into memory
      while ((getline line < listfile) > 0) {
        ids[line] = 1
      }
      close(listfile)
    }
    {
      # $col is DOCTOR_ID field (string)
      if ($col in ids) {
        print $0
      }
    }
  ' >> "$TMP_CSV"

log "Intermediate filtered CSV written → $TMP_CSV"

####### 4) COUNT TOTAL ROWS AFTER FILTER (OPTIONAL) #######
# Subtract 1 for header
total_after=$(wc -l < "$TMP_CSV")
rows_after_filter=$(( total_after - 1 ))
log "Rows after filter: $rows_after_filter"

####### 5) COMPRESS & CLEAN UP #######
log "Compressing to gzip…"
gzip -c "$TMP_CSV" > "$OUTPUT_FILE" && rm "$TMP_CSV"
log "Compression complete → $OUTPUT_FILE"

log "END: completed successfully."

exit 0
