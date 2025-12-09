#!/usr/bin/env bash
# ----------------------------------------------------------------------------
# Script Name: id_date_code_to_wide_join_dvv.sh
# Description:
#   Takes a long file (ID,DATE,CODE; no header) and dvv_all.csv and:
#     1) Removes rows where DATE is NA or outside [1998-01-01, 2023-12-31].
#     2) Pivots to wide: one row per ID; for each CODE:
#         - CODE_DATE = DATE or NA
#         - CODE = 1 if DATE present, else 0
#     3) Left-joins the wide table to dvv_all.csv by ID (memory-efficient).
#     4) Writes result + log file.
#   Completely streaming & sort-based → memory-efficient.
# ----------------------------------------------------------------------------

set -euo pipefail
IFS=$'\n\t'

####### CONFIGURATION (EDIT THESE) #######
INPUT_FILE="/XXX/path/to/id_date_code_long.csv"   # No header, columns: ID,DATE,CODE
DVV_FILE="/XXX/path/to/dvv_all.csv"               # Header, first column = ID
BASE_NAME="id_code_wide_join_dvv"
OUTPUT_DIR="/XXX/path/to/output"
LOG_DIR="/XXX/path/to/log"
###########################################

mkdir -p "$OUTPUT_DIR" "$LOG_DIR"

TIMESTAMP=$(date +"%Y%m%d%H%M%S")
OUT_FILE="${OUTPUT_DIR}/${BASE_NAME}_${TIMESTAMP}.csv"
LOG_FILE="${LOG_DIR}/${BASE_NAME}_${TIMESTAMP}.log"

log() {
  echo "[$(date +"%Y-%m-%d %H:%M:%S")] $*" | tee -a "$LOG_FILE"
}

TMP_DIR=$(mktemp -d "${OUTPUT_DIR}/tmp_${BASE_NAME}_${TIMESTAMP}_XXXX")
trap 'log "ERROR at line $LINENO"; rm -rf "$TMP_DIR"; exit 1' ERR

log "START: INPUT='$INPUT_FILE', DVV='$DVV_FILE' → OUTPUT='$OUT_FILE'"
log "Temporary directory: $TMP_DIR"

FILTERED_LONG="${TMP_DIR}/filtered_long.csv"
FILTERED_SORTED="${TMP_DIR}/filtered_sorted.csv"
CODES_FILE="${TMP_DIR}/codes.txt"
WIDE_FILE="${TMP_DIR}/wide.csv"
WIDE_SORTED="${TMP_DIR}/wide_sorted.csv"
DVV_HEADER_FILE="${TMP_DIR}/dvv_header.txt"
DVV_BODY_FILE="${TMP_DIR}/dvv_body.csv"
DVV_BODY_SORTED="${TMP_DIR}/dvv_body_sorted.csv"
JOINED_BODY="${TMP_DIR}/joined_body.csv"

########################################
# 1) FILTER INVALID DATES
########################################
log "Step 1: Filtering rows with DATE=NA or outside [1998-01-01, 2023-12-31]..."

if [[ "$INPUT_FILE" == *.gz ]]; then
  gzip -cd -- "$INPUT_FILE" | awk -F',' '
    $2 != "NA" && $2 != "" && $2 >= "1998-01-01" && $2 <= "2023-12-31" {
      print $0
    }
  ' > "$FILTERED_LONG"
else
  awk -F',' '
    $2 != "NA" && $2 != "" && $2 >= "1998-01-01" && $2 <= "2023-12-31" {
      print $0
    }
  ' "$INPUT_FILE" > "$FILTERED_LONG"
fi

log "Filtered rows saved to $FILTERED_LONG"

########################################
# 2) BUILD WIDE TABLE (NO DVV INVOLVED)
########################################
log "Step 2: Building wide table (pivot + binary flags)..."

# 2a) get unique codes
cut -d',' -f3 "$FILTERED_LONG" | sort -u > "$CODES_FILE"
NUM_CODES=$(wc -l < "$CODES_FILE" | tr -d ' ')
log "Found $NUM_CODES unique codes."

# 2b) sort by ID, CODE so we can stream by ID
sort -t',' -k1,1 -k3,3 "$FILTERED_LONG" > "$FILTERED_SORTED"

# 2c) streaming pivot: ID row-by-row
awk -F',' -v OFS=',' -v CODES_FILE="$CODES_FILE" '
  BEGIN {
    # Load codes into array + index
    while ((getline code < CODES_FILE) > 0) {
      if (code != "") {
        n_codes++
        codes[n_codes]   = code
        code_idx[code]   = n_codes
      }
    }
    close(CODES_FILE)

    # Header: ID, CODE1_DATE,CODE1, CODE2_DATE,CODE2, ...
    printf "ID"
    for (i = 1; i <= n_codes; i++) {
      c = codes[i]
      printf ",%s_DATE,%s", c, c
    }
    printf "\n"

    have_id = 0
  }

  {
    id   = $1
    date = $2
    code = $3

    if (!have_id) {
      current_id = id
      for (i = 1; i <= n_codes; i++) { date_arr[i] = "NA"; bin_arr[i] = 0 }
      have_id = 1
    }

    if (id != current_id) {
      # flush previous ID
      printf "%s", current_id
      for (i = 1; i <= n_codes; i++) {
        printf ",%s,%d", date_arr[i], bin_arr[i]
      }
      printf "\n"

      current_id = id
      for (i = 1; i <= n_codes; i++) { date_arr[i] = "NA"; bin_arr[i] = 0 }
    }

    idx = code_idx[code]
    if (idx != "") {
      date_arr[idx] = date
      bin_arr[idx]  = 1
    }
  }

  END {
    if (have_id) {
      printf "%s", current_id
      for (i = 1; i <= n_codes; i++) {
        printf ",%s,%d", date_arr[i], bin_arr[i]
      }
      printf "\n"
    }
  }
' "$FILTERED_SORTED" > "$WIDE_FILE"

log "Wide table written to $WIDE_FILE"

########################################
# 3) LEFT JOIN WIDE WITH DVV
########################################
log "Step 3: Left-joining DVV with wide table by ID..."

# Split DVV into header + body and sort body by ID
head -n1 "$DVV_FILE" > "$DVV_HEADER_FILE"
tail -n +2 "$DVV_FILE" > "$DVV_BODY_FILE"
sort -t',' -k1,1 "$DVV_BODY_FILE" > "$DVV_BODY_SORTED"

# Drop wide header and sort wide body by ID
tail -n +2 "$WIDE_FILE" | sort -t',' -k1,1 > "$WIDE_SORTED"

# Construct final header: dvv header + extra columns (CODE_DATE,CODE)
DVV_HDR=$(cat "$DVV_HEADER_FILE")
EXTRA_HDR=""
while read -r code; do
  [[ -z "$code" ]] && continue
  EXTRA_HDR="${EXTRA_HDR},${code}_DATE,${code}"
done < "$CODES_FILE"

echo "${DVV_HDR}${EXTRA_HDR}" > "$OUT_FILE"

# Streaming merge DVV ⟕ WIDE by ID
awk -F',' -v OFS=',' -v WIDE="$WIDE_SORTED" -v CODES_FILE="$CODES_FILE" '
  BEGIN {
    # How many NA,0 pairs for a missing wide row?
    while ((getline c < CODES_FILE) > 0) {
      if (c != "") n_codes++
    }
    close(CODES_FILE)

    # Prime wide iterator
    wide_eof = 0
    if ((getline wl < WIDE) > 0) {
      split(wl, w, FS)
      wide_id   = w[1]
      wide_tail = wl
      sub(/^[^,]*,/, "", wide_tail)  # strip first field + comma
    } else {
      wide_eof = 1
    }
  }

  {
    dvv_id = $1

    # advance wide until wide_id >= dvv_id
    while (!wide_eof && wide_id < dvv_id) {
      if ((getline wl < WIDE) > 0) {
        split(wl, w, FS)
        wide_id   = w[1]
        wide_tail = wl
        sub(/^[^,]*,/, "", wide_tail)
      } else {
        wide_eof = 1
      }
    }

    if (!wide_eof && wide_id == dvv_id) {
      # match: append wide columns
      print $0 OFS wide_tail
    } else {
      # no match: append NA,0 for each code
      out = $0
      for (i = 1; i <= n_codes; i++) {
        out = out ",NA,0"
      }
      print out
    }
  }
' "$DVV_BODY_SORTED" > "$JOINED_BODY"

cat "$JOINED_BODY" >> "$OUT_FILE"

log "Joined table saved to $OUT_FILE"

########################################
# 4) CLEANUP
########################################
rm -rf "$TMP_DIR"
log "Temporary directory removed."
log "END."