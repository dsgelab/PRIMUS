#!/usr/bin/env bash
# ----------------------------------------------------------------------------
# Script Name: extract_atc_codes_f2.sh
# Description: Filters a gzipped prescriptions CSV file to include only rows
#              where the ATC CODE (column "CODE") starts with a prefix listed
#              in atc_list.txt. For each PATIENT_ID and CODE combination,
#              keeps only the FIRST occurrence by PRESCRIPTION_DATE.
#              Output is gzipped CSV + log.
#              Memory-efficient version using sorting and deduplication.
# ----------------------------------------------------------------------------

set -euo pipefail
IFS=$'\n\t'

####### CONFIGURATION (EDIT THESE) #######
INPUT_FILE="/XXX/path/to/composite_prescription.csv.gz"
LIST_FILE="/XXX/path/to/atc_list.txt"          # One ATC prefix per line (e.g., C10AA)
BASE_NAME="atc_filtered_prescriptions"
OUTPUT_DIR="/XXX/path/to/output"
LOG_DIR="/XXX/path/to/log"
TEMP_DIR="${OUTPUT_DIR}/temp"  # Temporary directory for sorting
###########################################

mkdir -p "$OUTPUT_DIR" "$LOG_DIR" "$TEMP_DIR"

TIMESTAMP=$(date +"%Y%m%d%H%M%S")
TMP_FILTERED="${TEMP_DIR}/${BASE_NAME}_filtered_${TIMESTAMP}.csv"
TMP_SORTED="${TEMP_DIR}/${BASE_NAME}_sorted_${TIMESTAMP}.csv"
TMP_DEDUPED="${TEMP_DIR}/${BASE_NAME}_deduped_${TIMESTAMP}.csv"
OUTPUT_FILE="${OUTPUT_DIR}/${BASE_NAME}_${TIMESTAMP}.csv.gz"
LOG_FILE="${LOG_DIR}/${BASE_NAME}_${TIMESTAMP}.log"

log() {
  echo "[$(date +"%Y-%m-%d %H:%M:%S")] $*" | tee -a "$LOG_FILE"
}
trap 'log "ERROR at line $LINENO"; rm -rf "$TEMP_DIR"; exit 1' ERR

log "START: INPUT='$INPUT_FILE', ATC_LIST='$LIST_FILE' → OUTPUT='$OUTPUT_FILE'"

####### 1) BUILD PREFIX REGEX #######
log "Building ATC prefix regex from $LIST_FILE"
PREFIX_REGEX=$(awk '{gsub(/[\r\n]+/, ""); if ($1) printf "^%s|", $1}' "$LIST_FILE" | sed 's/|$//')
log "Compiled prefix regex: $PREFIX_REGEX"

####### 2) FIND COLUMN INDICES #######
set +o pipefail
if [[ "$INPUT_FILE" == *.gz ]]; then
  HEADER_LINE=$(gzip -cd "$INPUT_FILE" | head -n1)
else
  HEADER_LINE=$(head -n1 "$INPUT_FILE")
fi
set -o pipefail

CODE_INDEX=$(echo "$HEADER_LINE" | awk -F',' '{
  for (i=1; i<=NF; i++) {
    if ($i == "CODE") {
      print i;
      exit
    }
  }
}')

PATIENT_INDEX=$(echo "$HEADER_LINE" | awk -F',' '{
  for (i=1; i<=NF; i++) {
    if ($i == "PATIENT_ID") {
      print i;
      exit
    }
  }
}')

DATE_INDEX=$(echo "$HEADER_LINE" | awk -F',' '{
  for (i=1; i<=NF; i++) {
    if ($i == "PRESCRIPTION_DATE") {
      print i;
      exit
    }
  }
}')

if [[ -z "$CODE_INDEX" || -z "$PATIENT_INDEX" || -z "$DATE_INDEX" ]]; then
  log "ERROR: Required columns not found. CODE: $CODE_INDEX, PATIENT_ID: $PATIENT_INDEX, PRESCRIPTION_DATE: $DATE_INDEX"
  exit 1
fi

log "Detected columns - CODE: $CODE_INDEX, PATIENT_ID: $PATIENT_INDEX, PRESCRIPTION_DATE: $DATE_INDEX"

####### 3) FILTER ROWS BY PREFIX MATCH (streaming) #######
log "Filtering rows where CODE matches given ATC prefixes..."
if [[ "$INPUT_FILE" == *.gz ]]; then
  gzip -cd "$INPUT_FILE" | awk -F',' -v OFS=',' -v code_col="$CODE_INDEX" -v regex="$PREFIX_REGEX" '
    NR==1 { print; next }
    $code_col ~ regex { print }
  ' > "$TMP_FILTERED"
else
  awk -F',' -v OFS=',' -v code_col="$CODE_INDEX" -v regex="$PREFIX_REGEX" '
    NR==1 { print; next }
    $code_col ~ regex { print }
  ' "$INPUT_FILE" > "$TMP_FILTERED"
fi
log "Filtered CSV written to $TMP_FILTERED"

####### 4) SORT BY PATIENT_ID, CODE, DATE (disk-based sort) #######
log "Sorting by PATIENT_ID, CODE, and PRESCRIPTION_DATE (using disk-based sort)..."
# Extract header
head -n1 "$TMP_FILTERED" > "$TMP_SORTED"

# Sort data rows (skip header), using temporary directory for large sorts
tail -n +2 "$TMP_FILTERED" | \
  sort -t',' -k${PATIENT_INDEX},${PATIENT_INDEX} -k${CODE_INDEX},${CODE_INDEX} -k${DATE_INDEX},${DATE_INDEX} \
  -T "$TEMP_DIR" --parallel=4 -S 2G >> "$TMP_SORTED"

log "Sorted CSV written to $TMP_SORTED"
rm "$TMP_FILTERED"

####### 5) DEDUPLICATE: KEEP FIRST OCCURRENCE PER PATIENT+CODE (streaming) #######
log "Keeping only first occurrence per PATIENT_ID + CODE combination..."
awk -F',' -v OFS=',' \
  -v code_col="$CODE_INDEX" \
  -v patient_col="$PATIENT_INDEX" '
  NR==1 { 
    print
    next 
  }
  {
    # Create unique key from patient ID and CODE
    key = $patient_col "||" $code_col
    
    # Only print if this is the first time we see this combination
    if (!(key in seen)) {
      seen[key] = 1
      print
    }
  }
' "$TMP_SORTED" > "$TMP_DEDUPED"

log "Deduplicated CSV written to $TMP_DEDUPED"
rm "$TMP_SORTED"

####### 6) COMPRESS OUTPUT #######
log "Compressing final CSV..."
gzip -c "$TMP_DEDUPED" > "$OUTPUT_FILE"
rm "$TMP_DEDUPED"

log "Compression complete → $OUTPUT_FILE"

# Cleanup temp directory
rm -rf "$TEMP_DIR"
log "Temporary files cleaned up"
log "END."