#!/usr/bin/env bash
# ----------------------------------------------------------------------------
# Script Name: extract_icd_codes_f2_20.sh
# Description: Filters a (gzipped or plain) diagnoses CSV to rows where
#              ICD10_CODE starts with any prefix in icd_list.txt.
#              Then, for each (PATIENT_ID, ICD10_CODE) pair, keeps only the
#              first (earliest) row by VISIT_DATE where the patient is ≥20 years old.
#              Outputs only PATIENT_ID, VISIT_DATE, ICD10_CODE columns.
#              Outputs a gzipped CSV and a timestamped log.
# ----------------------------------------------------------------------------

set -euo pipefail
IFS=$'\n\t'

####### CONFIGURATION (EDIT THESE) #######
INPUT_FILE="/XXX/path/to/AllConnected_Diagnoses.csv.gz"
BIRTH_DATE_FILE="/XXX/path/to/DVV_ID_BD.csv"  # Columns: ID, BIRTH_DATE
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

log "START: INPUT='$INPUT_FILE', BIRTH_DATES='$BIRTH_DATE_FILE', ICD_LIST='$LIST_FILE' → OUTPUT='$OUTPUT_FILE'"

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

####### 3) FIND COLUMN INDICES IN DIAGNOSES FILE #######
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

####### 4) LOAD BIRTH DATES INTO MEMORY #######
log "Loading birth dates from $BIRTH_DATE_FILE..."
declare -A BIRTH_DATES
while IFS=',' read -r patient_id birth_date; do
  # Skip header and clean whitespace
  if [[ "$patient_id" != "ID" ]]; then
    BIRTH_DATES["$patient_id"]="$birth_date"
  fi
done < "$BIRTH_DATE_FILE"
log "Loaded ${#BIRTH_DATES[@]} birth dates"

####### 5) EXPORT BIRTH DATES FOR AWK #######
# Create a temporary file with birth dates for AWK to read
TMP_BIRTH_FILE="${OUTPUT_DIR}/tmp_birth_dates_${TIMESTAMP}.txt"
for patient_id in "${!BIRTH_DATES[@]}"; do
  echo "${patient_id},${BIRTH_DATES[$patient_id]}"
done > "$TMP_BIRTH_FILE"

####### 6) FILTER AND DEDUPLICATE #######
log "Filtering rows (age ≥20, ICD codes trimmed to 3 chars) and keeping only first occurrence per PATIENT_ID + ICD10_CODE by VISIT_DATE..."
$READER "$INPUT_FILE" | awk -F',' -v OFS=',' \
  -v code_col="$CODE_INDEX" \
  -v patient_col="$PATIENT_INDEX" \
  -v date_col="$DATE_INDEX" \
  -v regex="$PREFIX_REGEX" \
  -v birth_file="$TMP_BIRTH_FILE" '
  BEGIN {
    # Load birth dates into associative array
    while ((getline < birth_file) > 0) {
      split($0, arr, ",")
      birth_dates[arr[1]] = arr[2]
    }
    close(birth_file)
  }
  
  # Function to calculate age in years from two dates (YYYY-MM-DD format)
  function calc_age(birth, visit,    b_year, b_month, b_day, v_year, v_month, v_day, age) {
    split(birth, b, "-")
    split(visit, v, "-")
    b_year = b[1]; b_month = b[2]; b_day = b[3]
    v_year = v[1]; v_month = v[2]; v_day = v[3]
    
    age = v_year - b_year
    if (v_month < b_month || (v_month == b_month && v_day < b_day)) {
      age--
    }
    return age
  }
  
  NR==1 { 
    # Output header with only the 3 columns we want
    print "PATIENT_ID,VISIT_DATE,ICD10_CODE"
    next 
  }
  
  {
    # Trim ICD code to first 3 characters
    icd_full = $code_col
    icd_code = substr(icd_full, 1, 3)
    
    # Check if trimmed code matches our prefix regex
    if (icd_code !~ regex) {
      next
    }
    
    patient_id = $patient_col
    visit_date = $date_col
    
    # Skip if no birth date available
    if (!(patient_id in birth_dates)) {
      next
    }
    
    # Calculate age at visit
    age = calc_age(birth_dates[patient_id], visit_date)
    
    # Only process if age >= 20
    if (age < 20) {
      next
    }
    
    # Create unique key from patient ID and trimmed ICD code
    key = patient_id "||" icd_code
    
    # If we haven'\''t seen this combination, or this date is earlier
    if (!(key in seen) || visit_date < seen[key]) {
      seen[key] = visit_date
      # Store only the 3 columns we need (with 3-char ICD code)
      rows[key] = patient_id "," visit_date "," icd_code
    }
  }
  END {
    # Output all unique first occurrences
    for (key in rows) {
      print rows[key]
    }
  }
' | sort -t',' -k1,1 -k3,3 > "$TMP_CSV"

# Clean up temporary birth date file
rm -f "$TMP_BIRTH_FILE"

log "Filtered and deduplicated CSV written to $TMP_CSV"

####### 7) COMPRESS OUTPUT #######
log "Compressing filtered CSV..."
gzip -c "$TMP_CSV" > "$OUTPUT_FILE" && rm "$TMP_CSV"
log "Compression complete → $OUTPUT_FILE"
log "END."