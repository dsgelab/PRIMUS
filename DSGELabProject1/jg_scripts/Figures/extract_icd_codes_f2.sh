#!/usr/bin/env bash
# ----------------------------------------------------------------------------
# Script Name: extract_icd_codes_f2.sh
# Description: Filters a (gzipped or plain) diagnoses CSV to rows where
#              ICD10_CODE starts with any prefix in icd_list.txt.
#              Then, for each (PATIENT_ID, ICD10_CODE) pair, keeps only the
#              first (earliest) row by PRESCRIPTION_DATE.
#              Outputs a gzipped CSV and a timestamped log.
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

####### 3) FIND COLUMN INDICES #######
set +o pipefail  # Allow failure to handle missing columns
HEADER_LINE=$($READER "$INPUT_FILE" | head -n1)
CODE_INDEX=$(echo "$HEADER_LINE" | awk -F',' '{
  for (i=1; i<=NF; i++) {
    if ($i == "ICD10_CODE") {
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
    if ($i == "VISIT_DATE") {
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
if [[ -z "$PATIENT_INDEX" ]]; then
  log "ERROR: PATIENT_ID column not found in header."
  exit 1
fi
if [[ -z "$DATE_INDEX" ]]; then
  log "ERROR: VISIT_DATE column not found in header."
  exit 1
fi

log "Detected columns - ICD10_CODE: $CODE_INDEX, PATIENT_ID: $PATIENT_INDEX, VISIT_DATE: $DATE_INDEX"

####### 4) FILTER AND DEDUPLICATE #######
log "Filtering rows and keeping only first occurrence per PATIENT_ID + ICD10_CODE by VISIT_DATE..."
$READER "$INPUT_FILE" | awk -F',' -v OFS=',' \
  -v code_col="$CODE_INDEX" \
  -v patient_col="$PATIENT_INDEX" \
  -v date_col="$DATE_INDEX" \
  -v regex="$PREFIX_REGEX" '
  NR==1 { 
    print
    next 
  }
  $code_col ~ regex {
    # Create unique key from patient ID and ICD code
    key = $patient_col "||" $code_col
    
    # If we haven'\''t seen this combination, or this date is earlier
    if (!(key in seen) || $date_col < seen[key]) {
      seen[key] = $date_col
      rows[key] = $0
    }
  }
  END {
    # Output all unique first occurrences
    for (key in rows) {
      print rows[key]
    }
  }
' | sort -t',' -k${PATIENT_INDEX},${PATIENT_INDEX} -k${CODE_INDEX},${CODE_INDEX} > "$TMP_CSV"

log "Filtered and deduplicated CSV written to $TMP_CSV"

####### 5) COMPRESS OUTPUT #######
log "Compressing filtered CSV..."
gzip -c "$TMP_CSV" > "$OUTPUT_FILE" && rm "$TMP_CSV"
log "Compression complete → $OUTPUT_FILE"
log "END."