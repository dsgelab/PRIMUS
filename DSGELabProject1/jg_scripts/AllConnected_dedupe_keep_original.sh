#!/usr/bin/env bash
# ----------------------------------------------------------------------------
# Script Name: dedupe_keep_original.sh
# Description: Deduplicates a merged prescriptions CSV by (PATIENT_ID, CODE, PRESCRIPTION_DATE),
#              keeping only rows where SOURCE="Prescription" (original) if both imputed and original
#              prescriptions exist. Preserves all columns.
#              Uses external disk-backed sort then an awk pass. Outputs a timestamped gzipped CSV + log.
# ----------------------------------------------------------------------------

set -euo pipefail
IFS=$'\n\t'

####### CONFIGURATION (EDIT THESE) #######
INPUT_FILE="/XXX/path/to/all_prescriptions_20250506184349.csv.gz"
BASE_NAME="composite_prescriptions"  # Base name for output files
LOG_DIR="/XXX/path/to/log"
OUTPUT_DIR="/XXX/path/to/output"
# Directory for sort temporary spills
SORT_TMPDIR="/XXX/path/to/output/sort_tmp"
# Max RAM for sort (e.g. "200M", "1G")
SORT_BUFFER="200M"
#########################################

mkdir -p "$OUTPUT_DIR" "$SORT_TMPDIR" "$LOG_DIR"

# Derived filenames
TIMESTAMP=$(date +"%Y%m%d%H%M%S")
TMP_CSV="${OUTPUT_DIR}/${BASE_NAME}_${TIMESTAMP}.csv"
OUTPUT_FILE="${TMP_CSV}.gz"
LOG_FILE="${LOG_DIR}/${BASE_NAME}_${TIMESTAMP}.log"

# Simple logger
log(){
  echo "[$(date +"%Y-%m-%d %H:%M:%S")] $*" | tee -a "$LOG_FILE"
}
trap 'log "ERROR at line $LINENO"; exit 1' ERR

log "START: INPUT='$INPUT_FILE' → OUTPUT='$OUTPUT_FILE'"

# Decide how to read the input file
if [[ "$INPUT_FILE" == *.gz ]]; then
  reader="zcat"
else
  reader="cat"
fi

####### 1) PARSE HEADER & FIND INDICES #######
# Read header line into array
IFS=',' read -r -a headers < <($reader "$INPUT_FILE" | head -n1)
declare -A idx
for i in "${!headers[@]}"; do
  idx["${headers[$i]}"]=$((i+1))
done

# Ensure required columns exist
for col in PATIENT_ID CODE PRESCRIPTION_DATE SOURCE; do
  [[ -n "${idx[$col]:-}" ]] || {
    echo "ERROR: missing column '$col' in input file" >&2
    exit 1
  }
done

# Assign index vars
ip=${idx[PATIENT_ID]}
ic=${idx[CODE]}
ipd=${idx[PRESCRIPTION_DATE]}
isrc=${idx[SOURCE]}

log "Column indices: PATIENT_ID=$ip, CODE=$ic, PRESCRIPTION_DATE=$ipd, SOURCE=$isrc"

####### 2) OPTIONAL SANITY COUNTERS #######
total_before=$($reader "$INPUT_FILE" | tail -n +2 | wc -l)
log "Rows before dedupe: $total_before"

####### 3) EXTERNAL SORT & DEDUPE #######
log "Building deduplicated CSV via external sort (buffer=$SORT_BUFFER, tmp=$SORT_TMPDIR)…"

# 3a) Emit header unchanged
set +o pipefail
$reader "$INPUT_FILE" | head -n1 > "$TMP_CSV"
set -o pipefail

# 3b) Sort+awk pipeline
$reader "$INPUT_FILE" \
  | tail -n +2 \
  | sort \
      --temporary-directory="$SORT_TMPDIR" \
      --parallel="$(nproc)" \
      --buffer-size="$SORT_BUFFER" \
      --stable \
      -t, \
      -k${ip},${ip} \
      -k${ic},${ic} \
      -k${ipd},${ipd} \
      -k${isrc},${isrc}r \
  | awk -F',' -v OFS=',' -v ip="$ip" -v ic="$ic" -v ipd="$ipd" \
      'BEGIN { prev="" }
       {
         key = $ip FS $ic FS $ipd
         if (key != prev) {
           print $0
           prev = key
         }
       }' \
  >> "$TMP_CSV"

log "Intermediate deduped CSV written → $TMP_CSV"

####### 4) SANITY COUNT AFTER #######
total_after=$(wc -l < "$TMP_CSV")
# subtract 1 for the header line
log "Rows after dedupe: $(( total_after - 1 ))"

####### 5) COMPRESS & CLEAN UP #######
log "Compressing to gzip…"
gzip -c "$TMP_CSV" > "$OUTPUT_FILE" && rm "$TMP_CSV"
log "Compression complete → $OUTPUT_FILE"

log "END."