#!/usr/bin/env bash
# ----------------------------------------------------------------------------
# Script Name: count_prescriptions_by_doctor_year.sh
# Description: Groups a CSV file by DOCTOR_ID and year of PRESCRIPTION_DATE,
#              counting the number of rows per group.
#              Outputs a CSV with columns: DOCTOR_ID,YEAR,COUNT.
# ----------------------------------------------------------------------------

set -euo pipefail
IFS=$'\n\t'

####### CONFIGURATION (EDIT THESE) #######
INPUT_FILE="/XXX/path/to/composite_prescriptions.csv.gz"
BASE_NAME="prescription_counts"  # Base name for output files
LOG_DIR="/XXX/path/to/log"
OUTPUT_DIR="/XXX/path/to/output"
SORT_TMPDIR="/XXX/path/to/output/sort_tmp"
SORT_BUFFER="200M"
#########################################

mkdir -p "$OUTPUT_DIR" "$SORT_TMPDIR" "$LOG_DIR"

# Derived filenames
TIMESTAMP=$(date +"%Y%m%d%H%M%S")
TMP_OUT="${OUTPUT_DIR}/${BASE_NAME}_${TIMESTAMP}.csv"
OUTPUT_FILE="${TMP_OUT}.gz"
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
IFS=',' read -r -a headers < <($reader "$INPUT_FILE" | head -n1)
declare -A idx
for i in "${!headers[@]}"; do
  idx["${headers[$i]}"]=$((i+1))
done

for col in DOCTOR_ID PRESCRIPTION_DATE; do
  [[ -n "${idx[$col]:-}" ]] || {
    echo "ERROR: missing column '$col' in input file" >&2
    exit 1
  }
done

idoc=${idx[DOCTOR_ID]}
idate=${idx[PRESCRIPTION_DATE]}

log "Column indices: DOCTOR_ID=$idoc, PRESCRIPTION_DATE=$idate"

####### 2) COUNT PRESCRIPTIONS BY YEAR #######
log "Counting prescriptions per doctor and year…"

{
  echo "DOCTOR_ID,YEAR,COUNT"
  $reader "$INPUT_FILE" \
    | tail -n +2 \
    | awk -F',' -v OFS=',' -v idoc="$idoc" -v idate="$idate" '
        {
          split($idate, d, "-");
          year = d[1];
          key = $idoc FS year;
          counts[key]++;
        }
        END {
          for (k in counts) {
            split(k, parts, FS);
            print parts[1], parts[2], counts[k];
          }
        }
      ' \
    | sort -t',' -k1,1 -k2,2
} > "$TMP_OUT"

log "Count output written → $TMP_OUT"

####### 3) COMPRESS #######
log "Compressing output…"
gzip -c "$TMP_OUT" > "$OUTPUT_FILE" && rm "$TMP_OUT"
log "Final output: $OUTPUT_FILE"

log "END."
