#!/usr/bin/env bash
# ----------------------------------------------------------------------------
# Script Name: filter_prescriptions.sh
# Description: From all_prescriptions.csv.gz, keep:
#   - every original ("Prescription") row
#   - any imputed ("Imputed_Prescription") row for which no original exists
# Outputs a filtered CSV.GZ and logs the final row count.
# ----------------------------------------------------------------------------

set -euo pipefail
IFS=$'\n\t'

####### CONFIGURATION (EDIT THESE) #######
INPUT_FILE="/media/volume/Projects/jg/all_prescriptions_20250506184349.csv.gz"
BASE_NAME="composite_prescriptions"      # base name for all outputs
LOG_DIR="/media/volume/Projects/jg/Logs"
OUTPUT_DIR="/media/volume/Projects/jg"
SORT_TMPDIR="/media/volume/Projects/jg/sort_tmp"  # for sort spills
SORT_BUFFER="200M"                                # e.g. "200M" or "1G"
###############################################

# make sure our dirs exist
mkdir -p "$LOG_DIR" "$OUTPUT_DIR" "$SORT_TMPDIR"

# timestamp for filenames
TIMESTAMP=$(date +'%Y%m%d_%H%M%S')

# derived filenames
OUTPUT_FILE="${OUTPUT_DIR}/${BASE_NAME}_filtered_${TIMESTAMP}.csv.gz"
LOG_FILE="${LOG_DIR}/${BASE_NAME}_filtered_${TIMESTAMP}.log"

log(){ echo "[$(date +'%F %T')] $*" | tee -a "$LOG_FILE"; }
trap 'log "ERROR at line $LINENO"; exit 1' ERR

log "Starting filter of '$INPUT_FILE' → '$OUTPUT_FILE'"

# ———————— 1) Read & parse header ————————
# turn off pipefail so head won't kill us on SIGPIPE
set +o pipefail
raw1=$(zcat "$INPUT_FILE" | head -n1)
set -o pipefail

if [[ -z "$raw1" ]]; then
  log "ERROR: couldn’t read header from $INPUT_FILE"
  exit 1
fi

# handle wrapped header (if SOURCE spilled to next line)
if ! grep -q 'SOURCE' <<<"$raw1"; then
  raw2=$(zcat "$INPUT_FILE" | sed -n '2p')
  header="${raw1}${raw2}"
else
  header="$raw1"
fi

log "Header read: $header"

# map col names → positions
IFS=',' read -r -a cols <<<"$header"
declare -A idx
for i in "${!cols[@]}"; do
  idx["${cols[$i]}"]=$((i+1))
done
for col in PATIENT_ID CODE PRESCRIPTION_DATE SOURCE; do
  [[ -n "${idx[$col]:-}" ]] || { log "ERROR: missing column '$col'"; exit 1; }
done

pid_col=${idx[PATIENT_ID]}
code_col=${idx[CODE]}
date_col=${idx[PRESCRIPTION_DATE]}
src_col=${idx[SOURCE]}

log "Using columns: PATIENT_ID=$pid_col, CODE=$code_col, PRESC_DATE=$date_col, SOURCE=$src_col"

# helper to mktemp in our sort tmp
mktemp_sort(){ mktemp --tmpdir="$SORT_TMPDIR" filter.XXXXXX; }

# ———————— 2) Build set of original keys ————————
tmp_orig_keys=$(mktemp_sort)
log "Extracting unique original keys…"

zcat "$INPUT_FILE" \
  | awk -F',' -v pid="$pid_col" -v code="$code_col" -v date="$date_col" -v src="$src_col" '
      NR>1 && $src=="Prescription" {
        print $pid FS $code FS $date
      }
    ' \
  | sort -u \
      -T "$SORT_TMPDIR" \
      -S "$SORT_BUFFER" \
      --parallel="$(nproc)" \
  > "$tmp_orig_keys"

orig_count=$(wc -l < "$tmp_orig_keys")
log "Found $orig_count unique original prescription keys"

# ———————— 3) Filter the file ————————
log "Filtering out redundant imputed rows…"

zcat "$INPUT_FILE" \
  | awk -F',' -v OFS=',' \
        -v pid="$pid_col" -v code="$code_col" -v date="$date_col" -v src="$src_col" \
        -v KEYFILE="$tmp_orig_keys" '
    BEGIN {
      # load original-keys into memory (one string per key)
      while ((getline line < KEYFILE) > 0) orig[line]=1
      close(KEYFILE)
    }
    NR==1 {
      print  # always keep header
      next
    }
    {
      key = $pid FS $code FS $date
      if ($src=="Prescription") {
        print      # keep every original
      }
      else if ($src=="Imputed_Prescription" && !(key in orig)) {
        print      # keep imputed only when no original exists
      }
    }
  ' \
  | gzip > "$OUTPUT_FILE"

log "Filtered file written to $OUTPUT_FILE"

# ———————— 4) Count result rows ————————
data_rows=$(zcat "$OUTPUT_FILE" | tail -n +2 | wc -l)
log "Total data rows after filtering: $data_rows"

# clean up
rm "$tmp_orig_keys"

log "Filtering complete."
