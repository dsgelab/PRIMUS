#!/usr/bin/env bash

# file_report_big.sh — Efficient single-pass summary via Miller

if [[ $# -ne 1 ]]; then
  echo "Usage: $0 path/to/file.csv[.gz]"
  exit 1
fi

FILE="$1"
TS=$(date +"%Y%m%d_%H%M%S")
LOG="report_${TS}.log"

# pick the right reader
if [[ "$FILE" == *.gz ]]; then
  READ="gunzip -c"
else
  READ="cat"
fi

# 1) grab header + first 3 data rows early (we’ll re-stream later for stats)
HEADER=$($READ "$FILE" | head -n1)
FIRST3=$($READ "$FILE" | head -n4 | tail -n +2)

# 2) run a single mlr pass to get per-column: non-null count, distinct count, null count, type
#    it emits CSV with columns: column,count,distinct,nulls,type
STATS=$($READ "$FILE" \
  | mlr --csv stats1 -a count,distinct,nulls,type)

# 3) parse out overall row-count and column-count
#    (row_count = count + nulls of the first column; cols = number of fields in header)
ROWCOL=$(echo "$STATS" \
  | awk -F, 'NR==2 { printf "%d %d\n", $2+$4, NF-4 }')
#    NF-4 because STATS has 5 fields: column,count,distinct,nulls,type
read -r NROWS NCOLS <<< "$ROWCOL"

{
  echo "Report generated: $(date +"%Y-%m-%d %H:%M:%S")"
  echo "File: $FILE"
  echo "Dimensions: ${NROWS} rows × ${NCOLS} cols"
  echo
  echo "Column names:"
  IFS=',' read -r -a COLS <<< "$HEADER"
  for c in "${COLS[@]}"; do
    echo "  • $c"
  done
  echo
  echo "First 3 data rows:"
  printf '%s\n' "$FIRST3" | sed 's/^/  /'
  echo

  # 4) pretty-print per-column stats from STATS
  echo "$STATS" \
    | tail -n +2 \
    | while IFS=, read -r col count distinct nulls dtype; do
        total=$((count + nulls))
        # guard against zero-row files
        if (( total > 0 )); then
          pct=$(awk -v m="$nulls" -v t="$total" \
                'BEGIN { printf("%.2f", m/t*100) }')
        else
          pct="NA"
        fi
        echo "Column: $col"
        echo "  Type: $dtype"
        echo "  Unique values: $distinct"
        echo "  Missing: $nulls of $total ($pct%)"
        echo
      done
} > "$LOG"

echo "Wrote report to $LOG"
