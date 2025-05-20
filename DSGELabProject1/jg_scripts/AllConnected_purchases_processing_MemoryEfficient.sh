#!/usr/bin/env bash
# ----------------------------------------------------------------------------
# Script Name: dedupe_and_reorder.sh
# Description: Deduplicates a CSV by (PATIENT_ID, ATC_CODE, PRESCRIPTION_DATE),
#              renames ATC_CODE→CODE, reorders columns to
#                PATIENT_ID,DOCTOR_ID,CODE,PRESCRIPTION_DATE,
#                PURCHASE_DATE,FD_HASH_CODE
#              Uses disk-based external sort (--buffer-size, --parallel),
#              then a tiny awk pass (O(1) memory). Outputs a timestamped
#              gzipped CSV + detailed log.
# ----------------------------------------------------------------------------

set -euo pipefail
IFS=$'\n\t'

####### CONFIGURATION (EDIT THESE) #######
INPUT_FILE="/XXX/path/to/your_input.csv"
BASE_NAME="your_input"            
LOG_DIR="/XXX/path/to/log"
OUTPUT_DIR="/XXX/path/to/output"  
# where sort can spill its temporary files:
SORT_TMPDIR="/XXX/path/to/output/sort_tmp"  
# max RAM for sort (e.g. "200M", "1G")
SORT_BUFFER="200M"                   
#########################################

mkdir -p "$OUTPUT_DIR" "$SORT_TMPDIR"

# derived filenames
TIMESTAMP=$(date +"%Y%m%d%H%M%S")
TMP_CSV="${OUTPUT_DIR}/${BASE_NAME}_${TIMESTAMP}.csv"
OUTPUT_FILE="${OUTPUT_DIR}/${BASE_NAME}_${TIMESTAMP}.csv.gz"
LOG_FILE="${LOG_DIR}/${BASE_NAME}_${TIMESTAMP}.log"

# simple logger
log(){
  echo "[$(date +"%Y-%m-%d %H:%M:%S")] $*" | tee -a "$LOG_FILE"
}
trap 'log "ERROR at line $LINENO"; exit 1' ERR


# start timer
START_EPOCH=$(date +%s)

# record script meta
SCRIPT_PATH=$(readlink -f "$0")
SCRIPT_DESC="Disk-backed sort+awk dedupe; rename ATC_CODE→CODE, reorder columns; gzip+log"
log "Script path: $SCRIPT_PATH"
log "Description: $SCRIPT_DESC"
log "START: INPUT='$INPUT_FILE' → OUTPUT='$OUTPUT_FILE'"

####### 1) READ HEADER & FIND COLUMN INDICES #######
# split on commas (Linux only)
IFS=',' read -r -a headers < <(head -n1 "$INPUT_FILE")
declare -A idx
for i in "${!headers[@]}"; do
  idx["${headers[$i]}"]=$((i+1))
done

# ensure all needed columns exist
for col in PATIENT_ID DOCTOR_ID ATC_CODE PRESCRIPTION_DATE PURCHASE_DATE FD_HASH_CODE; do
  [[ -n "${idx[$col]:-}" ]] \
    || { echo "ERROR: missing column $col" >&2; exit 1; }
done

# assign to bash vars (use did for DOCTOR_ID)
ip=${idx[PATIENT_ID]}; did=${idx[DOCTOR_ID]}; ic=${idx[ATC_CODE]}
ipd=${idx[PRESCRIPTION_DATE]}; iup=${idx[PURCHASE_DATE]}; ih=${idx[FD_HASH_CODE]}
log "Column idx: PATIENT_ID=$ip, DOCTOR_ID=$did, ATC_CODE=$ic, PRESC_DATE=$ipd, PURCHASE_DATE=$iup, FD_HASH_CODE=$ih"

####### 2) EXTERNAL SORT & DEDUPE TO INTERMEDIATE CSV #######
log "Building intermediate CSV via external sort (buffer=$SORT_BUFFER, tmp=$SORT_TMPDIR)…"

# 2a) emit header
printf 'PATIENT_ID,DOCTOR_ID,CODE,PRESCRIPTION_DATE,PURCHASE_DATE,FD_HASH_CODE\n' > "$TMP_CSV"

# 2b) tail → sort → awk → append
tail -n +2 "$INPUT_FILE" \
  | sort \
      --temporary-directory="$SORT_TMPDIR" \
      --parallel="$(nproc)" \
      --buffer-size="$SORT_BUFFER" \
      --stable \
      -t, \
      -k${ip},${ip} \
      -k${ic},${ic} \
      -k${ipd},${ipd} \
  | awk -F',' -v OFS=',' \
        -v ip="$ip" -v did="$did" -v ic="$ic" \
        -v ipd="$ipd" -v iup="$iup" -v ih="$ih" '
    BEGIN { prev="" }
    {
      key = $ip FS $ic FS $ipd
      if (key != prev) {
        print $ip, $did, $ic, $ipd, $iup, $ih
        prev = key
      }
    }
  ' >> "$TMP_CSV"

log "Intermediate CSV complete → $TMP_CSV"

####### 3) GZIP AT THE VERY END #######
log "Compressing to gzip…"
gzip -c "$TMP_CSV" > "$OUTPUT_FILE" \
  && rm "$TMP_CSV"

log "Compression complete → $OUTPUT_FILE"


####### 4) TOTAL RUNTIME #######
END_EPOCH=$(date +%s)
ELAPSED=$((END_EPOCH - START_EPOCH))
ELAPSED_MIN=$(awk "BEGIN { printf \"%.2f\", ${ELAPSED}/60 }")
log "Total runtime: ${ELAPSED_MIN} minutes"
log "END."
