#!/usr/bin/env bash
# ----------------------------------------------------------------------------
# Script Name: merge_prescriptions.sh
# Description: Merge imputed and original prescriptions into one CSV—
#              rename ATC_CODE→CODE, union headers, add SOURCE column,
#              output timestamped gzipped CSV + log.
# ----------------------------------------------------------------------------

set -euo pipefail
IFS=$'\n\t'

####### HARD-CODED SETTINGS (EDIT THESE) #######
IMPUTED_FILE="/XXX/path/to/imputed_prescriptions.csv.gz"
ORIGINAL_FILE="/XXX/path/to/original_prescriptions.csv"
BASE_NAME="all_prescriptions"
OUTPUT_DIR="/XXX/path/to/output"
LOG_DIR="/XXX/path/to/log"
###############################################

mkdir -p "$OUTPUT_DIR"

# Derived filenames
TIMESTAMP=$(date +"%Y%m%d%_H%M%S")
TMP_CSV="${OUTPUT_DIR}/${BASE_NAME}_${TIMESTAMP}.csv"
OUTPUT_FILE="${TMP_CSV}.gz"
LOG_FILE="${LOG_DIR}/${BASE_NAME}_${TIMESTAMP}.log"

# Simple logger
log(){
  echo "[$(date +"%Y-%m-%d %H:%M:%S")] $*" | tee -a "$LOG_FILE"
}
trap 'log "ERROR at line $LINENO"; exit 1' ERR

# Script metadata
SCRIPT_PATH=$(readlink -f "$0")
SCRIPT_DESC="Merge imputed & original prescriptions; rename ATC_CODE→CODE; union headers; add SOURCE; gzip+log"
log "Script path: $SCRIPT_PATH"
log "Description: $SCRIPT_DESC"
log "Start: IMPUTED='$IMPUTED_FILE', ORIGINAL='$ORIGINAL_FILE', OUTPUT='$OUTPUT_FILE'"

####### 1) READ HEADER & FIND INDICES IN IMPUTED FILE #######
# imputed file already has CODE (not ATC_CODE)
# temporarily turn off pipefail so the SIGPIPE isn’t fatal
set +o pipefail
im_header=$(gunzip -c "$IMPUTED_FILE" | head -n1)
set -o pipefail

# strip any stray null bytes (just in case)
im_header=${im_header//$'\000'/}

IFS=',' read -r -a im_cols <<< "$im_header"
declare -A im_idx
for i in "${!im_cols[@]}"; do
  im_idx["${im_cols[$i]}"]=$((i+1))
done

# Required columns in imputed
for col in PATIENT_ID DOCTOR_ID CODE PRESCRIPTION_DATE PURCHASE_DATE FD_HASH_CODE; do
  [[ -n "${im_idx[$col]:-}" ]] \
    || { echo "ERROR: column '$col' missing in imputed file" >&2; exit 1; }
done

# Capture their positions
ip1=${im_idx[PATIENT_ID]}; did1=${im_idx[DOCTOR_ID]}; ic1=${im_idx[CODE]}
ipd1=${im_idx[PRESCRIPTION_DATE]}; iup1=${im_idx[PURCHASE_DATE]}; ih1=${im_idx[FD_HASH_CODE]}

log "Imputed indices: PATIENT_ID=$ip1, DOCTOR_ID=$did1, CODE=$ic1, PRESC_DATE=$ipd1, PURCHASE_DATE=$iup1, FD_HASH=$ih1"

####### 2) READ HEADER & FIND INDICES IN ORIGINAL FILE #######
orig_header=$(head -n1 "$ORIGINAL_FILE")
IFS=',' read -r -a orig_cols <<< "$orig_header"
declare -A orig_idx
for i in "${!orig_cols[@]}"; do
  orig_idx["${orig_cols[$i]}"]=$((i+1))
done

# Required columns in original
for col in PATIENT_ID PRESCRIPTION_DATE ATC_CODE SECTOR CITY FD_HASH_CODE DOCTOR_ID; do
  [[ -n "${orig_idx[$col]:-}" ]] \
    || { echo "ERROR: column '$col' missing in original file" >&2; exit 1; }
done

# Capture their positions
ip2=${orig_idx[PATIENT_ID]}; ipd2=${orig_idx[PRESCRIPTION_DATE]}
atc2=${orig_idx[ATC_CODE]}; sec2=${orig_idx[SECTOR]}; city2=${orig_idx[CITY]}
ih2=${orig_idx[FD_HASH_CODE]}; did2=${orig_idx[DOCTOR_ID]}

log "Original indices: PATIENT_ID=$ip2, PRESC_DATE=$ipd2, ATC_CODE=$atc2, SECTOR=$sec2, CITY=$city2, FD_HASH=$ih2, DOCTOR_ID=$did2"

####### 3) BUILD INTERMEDIATE MERGED CSV #######
log "Building merged CSV → $TMP_CSV"
# Write unified header
printf 'PATIENT_ID,DOCTOR_ID,CODE,PRESCRIPTION_DATE,PURCHASE_DATE,FD_HASH_CODE,SECTOR,CITY,SOURCE\n' \
  > "$TMP_CSV"

# 3a) Append imputed rows (fill SECTOR,CITY with NA)
gunzip -c "$IMPUTED_FILE" | tail -n +2 | \
awk -F',' -v OFS=',' \
    -v ip="$ip1" -v did="$did1" -v ic="$ic1" \
    -v ipd="$ipd1" -v iup="$iup1" -v ih="$ih1" \
'{
  print $ip, $did, $ic, $ipd, $iup, $ih, "NA", "NA", "Imputed_Prescription"
}' >> "$TMP_CSV"

# 3b) Append original rows (fill PURCHASE_DATE with NA; rename ATC_CODE→CODE)
tail -n +2 "$ORIGINAL_FILE" | \
awk -F',' -v OFS=',' \
    -v ip="$ip2" -v did="$did2" -v atc="$atc2" \
    -v ipd="$ipd2" -v ih="$ih2" \
    -v sec="$sec2" -v city="$city2" \
'{
  print $ip, $did, $atc, $ipd, "NA", $ih, $sec, $city, "Prescription"
}' >> "$TMP_CSV"

log "Merged CSV written → $TMP_CSV"

####### 4) COMPRESS AT THE VERY END #######
log "Compressing merged CSV…"
gzip -c "$TMP_CSV" > "$OUTPUT_FILE" && rm "$TMP_CSV"
log "Compression complete → $OUTPUT_FILE"
log "END."
