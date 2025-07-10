#!/usr/bin/env bash
# ----------------------------------------------------------------------------
# Script Name: extract_atc_codes.sh
# Description: Filters a gzipped prescriptions CSV file to include only rows
#              where the ATC CODE (column "CODE") starts with a prefix listed
#              in atc_list.txt. Output is gzipped CSV + log.
# ----------------------------------------------------------------------------

set -euo pipefail
IFS=$'\n\t'

####### CONFIGURATION (EDIT THESE) #######
INPUT_FILE="/XXX/path/to/composite_prescription.csv.gz"
LIST_FILE="/XXX/path/to/atc_list.txt"          # One ATC prefix per line (e.g., C10AA)
BASE_NAME="atc_filtered_prescriptions"
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

log "START: INPUT='$INPUT_FILE', ATC_LIST='$LIST_FILE' → OUTPUT='$OUTPUT_FILE'"

####### 1) DETERMINE READER FOR INPUT #######
if [[ "$INPUT_FILE" == *.gz ]]; then
  READER="gzip -cd"
else
  READER="cat"
fi

####### 2) COMPILE ATC PREFIXES INTO AWK REGEX #######
log "Building ATC prefix regex from $LIST_FILE"
PREFIX_REGEX=$(awk '{gsub(/[\r\n]+/, ""); if ($1) printf "^%s|", $1}' "$LIST_FILE" | sed 's/|$//')
log "Compiled prefix regex: $PREFIX_REGEX"

####### 3) FIND COLUMN INDEX FOR CODE #######
set +o pipefail  # Allow failure in this command to handle missing CODE column
CODE_INDEX=$($READER "$INPUT_FILE" | head -n1 | awk -F',' '{
  for (i=1; i<=NF; i++) {
    if ($i == "CODE") {
      print i;
      exit
    }
  }
}')
set -o pipefail  # Restore strict error handling
if [[ -z "$CODE_INDEX" ]]; then
  log "ERROR: CODE column not found in header."
  exit 1
fi
log "Detected CODE column at position $CODE_INDEX"

####### 4) FILTER LINES BASED ON PREFIX MATCH #######
log "Filtering rows where CODE matches given ATC prefixes..."
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
