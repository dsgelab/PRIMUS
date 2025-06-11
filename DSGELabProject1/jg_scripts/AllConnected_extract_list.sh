#!/usr/bin/env bash
# ----------------------------------------------------------------------------
# Script Name: filter_by_patient_list.sh
# Description: Filters a prescriptions CSV file to include only those
#              PATIENT_IDs listed in a separate file. Supports both .csv and .csv.gz
#              input, preserves all columns, outputs timestamped gzipped CSV + log.
# ----------------------------------------------------------------------------

set -euo pipefail
IFS=$'\n\t'

####### CONFIGURATION (EDIT THESE) #######
INPUT_FILE="/XXX/path/to/merged_prescriptions_20250521130352.csv.gz"
LIST_FILE="/XXX/path/to/doctors_20250424.csv"
BASE_NAME="doctors_prescriptions"
OUTPUT_DIR="/XXX/path/to/output"
LOG_DIR="/XXX/path/to/log"
#########################################

mkdir -p "$OUTPUT_DIR" "$LOG_DIR"

# derived filenames\ nTIMESTAMP=$(date +"%Y%m%d%H%M%S")
TMP_CSV="${OUTPUT_DIR}/${BASE_NAME}_${TIMESTAMP}.csv"
OUTPUT_FILE="${TMP_CSV}.gz"
LOG_FILE="${LOG_DIR}/${BASE_NAME}_${TIMESTAMP}.log"

# simple logger\ nlog(){
  echo "[$(date +"%Y-%m-%d %H:%M:%S")] $*" | tee -a "$LOG_FILE"
}
trap 'log "ERROR at line $LINENO"; exit 1' ERR

log "START: INPUT='$INPUT_FILE', LIST='$LIST_FILE' → OUTPUT='$OUTPUT_FILE'"

####### 1) DETERMINE READER FOR INPUT #######
if [[ "$INPUT_FILE" == *.gz ]]; then
  READER="gzip -cd"
else
  READER="cat"
fi

####### 2) LOAD PATIENT_IDs INTO MEMORY #######
log "Loading PATIENT_ID whitelist from $LIST_FILE"
declare -A keep
while IFS= read -r line; do
  # extract first column as ID, skip header if present
  id=${line%%,*}
  [[ "$id" == "PATIENT_ID" || -z "$id" ]] && continue
  keep["$id"]=1
done < "$LIST_FILE"
log "Total IDs loaded: ${#keep[@]}"

####### 3) FILTER INPUT CSV #######
log "Filtering input by whitelist..."
# stream header + filtered body
$READER "$INPUT_FILE" | awk -F',' -v OFS=',' -v cols_keep="${!keep[*]}" \
  'BEGIN {
     # load the keep list into an awk array for O(1) lookups
     split(cols_keep, arr, " ");
     for (i in arr) whitelist[arr[i]] = 1;
   }
   NR==1 { print; next }
   whitelist[$1] { print }
' > "$TMP_CSV"

log "Filtered CSV written → $TMP_CSV"

####### 4) COMPRESS & CLEAN UP #######
log "Compressing to gzip..."
gzip -c "$TMP_CSV" > "$OUTPUT_FILE" \
  && rm "$TMP_CSV"
log "Compression complete → $OUTPUT_FILE"
log "END."
