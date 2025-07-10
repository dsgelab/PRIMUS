#!/usr/bin/env bash
# ----------------------------------------------------------------------------
# Script Name: extract_icd_codes.sh
# Description: Filters a gzipped prescriptions CSV file to include only rows
#              where the ICD CODE (column "ICD10_CODE") starts with a prefix listed
#              in icd_list.txt. Output is gzipped CSV + log.
# ----------------------------------------------------------------------------

set -euo pipefail
IFS=$'\n\t'

####### CONFIGURATION (EDIT THESE) #######
INPUT_FILE="/XXX/path/to/AllConnected_Diagnoses.csv.gz"
LIST_FILE="/XXX/path/to/icd_list.txt"          # One ICD prefix per line (e.g., J10)
BASE_NAME="icd_filtered_diagnoses"
OUTPUT_DIR="/XXX/path/to/output"
LOG_DIR="/XXX/path/to/log"
###########################################

mkdir -p "$OUTPUT_DIR" "$LOG_DIR"

TIMESTAMP=$(date +"%Y%m%d%H%M%S")
TMP_CSV="${OUTPUT_DIR}/${BASE_NAME}_${TIMESTAMP}.csv"
OUTPUT_FILE="${TMP_CSV}.gz"
LOG_FILE="${LOG_DIR}/${BASE_NAME}_${TIMESTAMP}.log"

log() {
  echo "[$(date +"%Y-%m-%d %H:%M:%S")] $*" | tee -a "$LOG_FILE"
}
trap 'log "ERROR at line $LINENO"; exit 1' ERR

log "START: INPUT='$INPUT_FILE', ICD_LIST='$LIST_FILE' → OUTPUT='$OUTPUT_FILE'"

####### 1) DETERMINE READER FOR INPUT #######
if [[ "$INPUT_FILE" == *.gz ]]; then
  READER="gzip -cd"
else
  READER="cat"
fi

####### 2) COMPILE ICD PREFIXES INTO AWK REGEX #######
log "Building ICD prefix regex from $LIST_FILE"
PREFIX_REGEX=$(awk '{gsub(/[\r\n]+/, ""); if ($1) printf "^%s|", $1}' "$LIST_FILE" | sed 's/|$//')
log "Compiled prefix regex: $PREFIX_REGEX"

####### 3) FIND COLUMN INDEX FOR CODE #######
set +o pipefail  # Allow failure in this command to handle missing CODE column
CODE_INDEX=$($READER "$INPUT_FILE" | head -n1 | awk -F',' '{
  for (i=1; i<=NF; i++) {
    if ($i == "ICD10_CODE") {
      print i;
      exit
    }
  }
}')
set -o pipefail  # Restore strict error handling
if [[ -z "$CODE_INDEX" ]]; then
  log "ERROR: ICD10_CODE column not found in header."
  exit 1
fi
log "Detected ICD10_CODE column at position $CODE_INDEX"

####### 4) FILTER LINES BASED ON PREFIX MATCH #######
log "Filtering rows where ICD10_CODE matches given ICD prefixes..."
$READER "$INPUT_FILE" | awk -F',' -v OFS=',' -v code_col="$CODE_INDEX" -v regex="$PREFIX_REGEX" '
  NR==1 { print; next }
  $code_col ~ regex { print }
' > "$TMP_CSV"
log "Filtered CSV written to $TMP_CSV"

####### 5) COMPRESS OUTPUT #######
log "Compressing filtered CSV..."
gzip -c "$TMP_CSV" > "$OUTPUT_FILE" && rm "$TMP_CSV"
log "Compression complete → $OUTPUT_FILE"
log "END."
