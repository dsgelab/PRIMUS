#!/usr/bin/env bash
# ----------------------------------------------------------------------------
# Script Name: count_prescriptions_vs_imputed.sh
# Description: From all_prescriptions CSV, filters rows from 2016 onward and
#              counts rows by DOCTOR_ID and year, separately for
#              SOURCE=Prescription and SOURCE=Imputed_Prescription.
#              Outputs a CSV with: DOCTOR_ID,YEAR,COUNT_PRESCRIPTIONS,COUNT_IMPUTED
# ----------------------------------------------------------------------------

set -euo pipefail
IFS=$'\n\t'

####### CONFIGURATION (EDIT THESE) #######
INPUT_FILE="/XXX/path/to/all_prescriptions.csv.gz"
BASE_NAME="prescription_counts_by_source"
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
log "Parsing header..."
declare -A idx
mapfile -t headers < <($reader "$INPUT_FILE" | head -n1 | tr ',' '\n')
for i in "${!headers[@]}"; do
  header=$(echo "${headers[$i]}" | tr -d '"')  # Strip quotes if present
  idx["$header"]=$((i+1))
  echo "Column $((i+1)): '$header'"
done

for col in DOCTOR_ID PRESCRIPTION_DATE SOURCE; do
  [[ -n "${idx[$col]:-}" ]] || {
    echo "ERROR: missing column '$col' in input file" >&2
    exit 1
  }
done

idoc=${idx[DOCTOR_ID]}
idate=${idx[PRESCRIPTION_DATE]}
isrc=${idx[SOURCE]}

log "Column indices: DOCTOR_ID=$idoc, PRESCRIPTION_DATE=$idate, SOURCE=$isrc"

####### 2) COUNT PRESCRIPTIONS PER DOCTOR/YEAR/SOURCE #######
log "Filtering from 2016 onward and counting prescriptions vs imputed…"

{
  echo "DOCTOR_ID,YEAR,COUNT_PRESCRIPTIONS,COUNT_IMPUTED"
  $reader "$INPUT_FILE" \
    | tail -n +2 \
    | awk -F',' -v OFS=',' -v idoc="$idoc" -v idate="$idate" -v isrc="$isrc" '
        {
          split($idate, d, "-");
          year = d[1] + 0;
          if (year < 2016) next;

          doc = $idoc;
          gsub(/^"|"$/, "", doc);  # Remove quotes if present
          source = $isrc;
          key = doc FS year;

          if (source == "Prescription") {
            p[key]++;
          } else if (source == "Imputed_Prescription") {
            i[key]++;
          }
        }
        END {
          for (k in p) { seen[k] = 1 }
          for (k in i) { seen[k] = 1 }
          for (k in seen) {
            split(k, parts, FS);
            doc = parts[1];
            year = parts[2];
            cp = (k in p ? p[k] : 0);
            ci = (k in i ? i[k] : 0);
            print doc, year, cp, ci;
          }
        }
      ' \
    | sort -t',' -k1,1 -k2,2n
} > "$TMP_OUT"

log "Output CSV written → $TMP_OUT"

####### 3) COMPRESS #######
log "Compressing result…"
gzip -c "$TMP_OUT" > "$OUTPUT_FILE" && rm "$TMP_OUT"
log "Final output: $OUTPUT_FILE"

log "END."