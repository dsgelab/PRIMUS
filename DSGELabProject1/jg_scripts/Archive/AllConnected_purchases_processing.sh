#!/usr/bin/env bash
# ----------------------------------------------------------------------------
# Script Name: dedupe_and_reorder.sh
# Description: Deduplicates a CSV by (PATIENT_ID, ATC_CODE, PRESCRIPTION_DATE),
#              renames ATC_CODE → CODE, reorders columns to
#              PATIENT_ID,DOCTOR_ID,CODE,PRESCRIPTION_DATE,PURCHASE_DATE,FD_HASH_CODE,
#              then outputs a timestamped gzipped CSV and a detailed log.
# ----------------------------------------------------------------------------
set -euo pipefail
IFS=$'\n\t'

####### HARD-CODED SETTINGS #######
INPUT_FILE="/XXX/path/to/your_input.csv"
BASE_NAME="your_input"            
LOG_DIR="/XXX/path/to/log"
OUTPUT_DIR="/XXX/path/to/output"  
###################################

TIMESTAMP=$(date +"%Y%m%d%H%M%S")
TMP_CSV="${OUTPUT_DIR}/${BASE_NAME}_${TIMESTAMP}.csv"
OUTPUT_FILE="${OUTPUT_DIR}/${BASE_NAME}_${TIMESTAMP}.csv.gz"
LOG_FILE="${LOG_DIR}/${BASE_NAME}_${TIMESTAMP}.log"

log() {
  echo "[$(date +"%Y-%m-%d %H:%M:%S")] $*" | tee -a "$LOG_FILE"
}
trap 'log "ERROR at line $LINENO"; exit 1' ERR

# Log script path & description
SCRIPT_PATH=$(readlink -f "$0")
SCRIPT_DESC="Deduplicate CSV by PATIENT_ID, ATC_CODE, PRESCRIPTION_DATE; rename ATC_CODE→CODE; reorder columns; output timestamped gzipped CSV + log"
log "Script path: $SCRIPT_PATH"
log "Description: $SCRIPT_DESC"
log "START: INPUT='${INPUT_FILE}' → OUTPUT='${OUTPUT_FILE}'"

####### 1) READ HEADER & FIND INDICES #######
IFS=',' read -r -a headers < <(head -n1 "$INPUT_FILE")
declare -A idx
for i in "${!headers[@]}"; do
  idx["${headers[$i]}"]=$((i+1))
done

# Ensure all required columns exist
for col in PATIENT_ID DOCTOR_ID ATC_CODE PRESCRIPTION_DATE PURCHASE_DATE FD_HASH_CODE; do
  if [[ -z "${idx[$col]:-}" ]]; then
    echo "ERROR: missing column '$col'" >&2
    exit 1
  fi
done

# Capture numeric indices (rename DOCTOR_ID index to 'did' to avoid awk conflicts)
ip=${idx[PATIENT_ID]}
did=${idx[DOCTOR_ID]}
ic=${idx[ATC_CODE]}
ipd=${idx[PRESCRIPTION_DATE]}
iup=${idx[PURCHASE_DATE]}
ih=${idx[FD_HASH_CODE]}

log "Column indices: PATIENT_ID=$ip, DOCTOR_ID=$did, ATC_CODE=$ic, PRESCRIPTION_DATE=$ipd, PURCHASE_DATE=$iup, FD_HASH_CODE=$ih"

####### 2) GENERATE INTERMEDIATE CSV #######
log "Generating intermediate CSV…"

# 2a) print new header
printf 'PATIENT_ID,DOCTOR_ID,CODE,PRESCRIPTION_DATE,PURCHASE_DATE,FD_HASH_CODE\n' > "$TMP_CSV"

# 2b) tail → sort by the 3-key → awk to keep first of each
tail -n +2 "$INPUT_FILE" \
  | sort --stable -t, \
        -k${ip},${ip} -k${ic},${ic} -k${ipd},${ipd} \
  | awk -F',' -v OFS=',' \
        -v ip="$ip" -v did="$did" -v ic="$ic" \
        -v ipd="$ipd" -v iup="$iup" -v ih="$ih" '
    BEGIN { prev = "" }
    {
      key = $ip FS $ic FS $ipd
      if (key != prev) {
        print $ip, $did, $ic, $ipd, $iup, $ih
        prev = key
      }
    }
  ' >> "$TMP_CSV"

####### 3) COMPRESS TO GZIP #######
log "Compressing to gzip…"
gzip -c "$TMP_CSV" > "$OUTPUT_FILE" \
  && rm "$TMP_CSV"

log "Compression complete → $OUTPUT_FILE"
log "END."