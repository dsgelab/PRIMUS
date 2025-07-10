#!/usr/bin/env bash
# ----------------------------------------------------------------------------
# Script Name: patient_summary_by_year.sh
# Description: From a longitudinal doctor–patient visits CSV (gzipped),
#              filter to a given set of doctors, then build a per-
#              patient, per-year summary of:
#                TotalDoctors, UniqueDoctors, TotalVisits,
#                Prescriptions, DiagnosisAvohilmo, DiagnosisHilmo
#              Processes 1998–2022 year-by-year to keep memory use low.
# ----------------------------------------------------------------------------

set -euo pipefail
IFS=$'\n\t'

####### CONFIGURATION (EDIT THESE) #######
INPUT_FILE="/media/volume/Projects/DSGELabProject1/ProcessedData/doctor_patient_longitudinal_VISIT_20250603.csv.gz"
DOCTOR_IDS_FILE="/media/volume/Projects/DSGELabProject1/doctor_IDs.csv"
OUTPUT_DIR="/media/volume/Projects/jg"
LOG_DIR="/media/volume/Projects/jg/Logs"
TEMP_DIR_ROOT="/media/volume/Projects/jg/sort_tmp"
#########################################

mkdir -p "$OUTPUT_DIR"
mkdir -p "$TEMP_DIR_ROOT"

BASE_NAME="patient_summary_VISIT"
TIMESTAMP=$(date +"%Y%m%d%H%M%S")
OUTPUT_FILE="${OUTPUT_DIR}/${BASE_NAME}_${TIMESTAMP}.csv"
LOG_FILE="${LOG_DIR}/${BASE_NAME}_${TIMESTAMP}.log"

# Simple logger
log(){
    echo "[$(date +"%Y-%m-%d %H:%M:%S")] $*" | tee -a "$LOG_FILE"
}
trap 'log "ERROR at line $LINENO"; exit 1' ERR

START_EPOCH=$(date +%s)
log "START: INPUT='$INPUT_FILE'  DOCTOR_IDS='$DOCTOR_IDS_FILE' → OUTPUT='$OUTPUT_FILE'"

# Years to process
YEARS=$(seq 1998 2022)

# Write CSV header
echo "PATIENT_ID,YEAR,TotalDoctors,UniqueDoctors,TotalVisits,Prescriptions,DiagnosisAvohilmo,DiagnosisHilmo" > "$OUTPUT_FILE"

# Create a working temp dir
WORK_TMP=$(mktemp -d "${TEMP_DIR_ROOT}/patient_summary_tmp.XXXXXX")
log "Using temp dir: $WORK_TMP"

for YEAR in $YEARS; do
    log "Processing year $YEAR..."

    YEAR_DATA="$WORK_TMP/year_${YEAR}.csv"

    # 1) Extract only this year's records AND only for doctors in DOCTOR_IDS_FILE
    zcat "$INPUT_FILE" \
      | awk -F',' -v year="$YEAR" -v DID_FILE="$DOCTOR_IDS_FILE" '
        BEGIN {
          # load allowed doctor IDs (one per line)
          while ((getline line < DID_FILE) > 0) {
            allowed[line] = 1
          }
          close(DID_FILE)
        }
        {
          doc = $1; date = $4
          split(date, D, "-")
          if (D[1] == year && (doc in allowed)) {
            print
          }
        }' \
      > "$YEAR_DATA"

    # 2) Summarize per patient
    awk -F',' -v year="$YEAR" -v OUT="$OUTPUT_FILE" '
      {
        doc = $1; pat = $2; rec = $3; date = $4

        # total doctor references (all records)
        total_docs[pat]++

        # unique doctors seen
        uniq_docs[pat][doc] = 1

        # unique visits per patient (doctor + date)
        visit_key = doc "_" date
        uniq_visits[pat][visit_key] = 1

        # count record types
        if (rec == "Prescription")          presc[pat]++
        else if (rec == "Diagnosis Avohilmo") diagA[pat]++
        else if (rec == "Diagnosis Hilmo")    diagH[pat]++
      }
      END {
        for (pat in total_docs) {
          ud = length(uniq_docs[pat])
          tv = length(uniq_visits[pat])
          printf "%s,%s,%d,%d,%d,%d,%d,%d\n",
            pat, year,
            total_docs[pat],
            ud, tv,
            presc[pat]+0,
            diagA[pat]+0,
            diagH[pat]+0 >> OUT
        }
      }' "$YEAR_DATA"

    # clean up
    rm "$YEAR_DATA"
done

# tear down temp dir
rm -rf "$WORK_TMP"

END_EPOCH=$(date +%s)
ELAPSED=$((END_EPOCH - START_EPOCH))
log "Completed in ${ELAPSED} seconds"
log "Summary saved to $OUTPUT_FILE"
