#!/usr/bin/env bash

# file_report.sh — write a CSV(.gz)-aware summary to a timestamped log

if [ $# -ne 1 ]; then
  echo "Usage: $0 path/to/file.csv[.gz]"
  exit 1
fi

FILE="$1"
TS=$(date +"%Y%m%d_%H%M%S")
LOG="report_${TS}.log"

# choose the right reader
if [[ "$FILE" == *.gz ]]; then
  READCMD="gunzip -c"
else
  READCMD="cat"
fi

{
  echo "Report generated: $(date +"%Y-%m-%d %H:%M:%S")"
  echo "File: $FILE"
  echo

  # --- dimensions & header
  HEADER=$($READCMD "$FILE" | head -n1)
  NCOL=$(printf "%s\n" "$HEADER" | awk -F, '{print NF}')
  NROW=$($READCMD "$FILE" | tail -n +2 | wc -l | tr -d ' ')

  echo "Dimensions: ${NROW} rows × ${NCOL} cols"
  echo "Column names:"
  IFS=',' read -r -a COLS <<< "$HEADER"
  for col in "${COLS[@]}"; do
    echo "  • $col"
  done
  echo

  # --- first 3 data rows
  echo "First 3 rows:"
  $READCMD "$FILE" \
    | head -n4 \
    | tail -n +2 \
    | sed 's/^/  /'
  echo

  # --- per-column stats
  for idx in "${!COLS[@]}"; do
    colname=${COLS[$idx]}
    colnum=$((idx + 1))

    # grab that one column (all data rows)
    vals=$($READCMD "$FILE" \
           | tail -n +2 \
           | cut -d',' -f"$colnum")

    total=$NROW
    miss=$(printf "%s\n" "$vals" \
           | grep -E '^$|^NA$' \
           | wc -l \
           | tr -d ' ')
    uniq=$(printf "%s\n" "$vals" \
           | sort \
           | uniq \
           | wc -l \
           | tr -d ' ')

    # infer type (default = chr)
    dtype="chr"
    # all-int?
    if ! printf "%s\n" "$vals" \
         | grep -Ev '^-?[0-9]+$' \
         | grep -q .; then
      dtype="int"
    # all-float?
    elif ! printf "%s\n" "$vals" \
         | grep -Ev '^-?[0-9]*\.[0-9]+$' \
         | grep -q .; then
      dtype="float"
    # all-YYYY-MM-DD?
    elif ! printf "%s\n" "$vals" \
         | grep -Ev '^[0-9]{4}-[0-9]{2}-[0-9]{2}$' \
         | grep -q .; then
      dtype="date(YYYY-MM-DD)"
    fi

    pct=$(awk -v m="$miss" -v t="$total" \
           'BEGIN { if (t>0) printf("%.2f", m/t*100); else print "NA" }')

    echo "Column #$colnum: $colname"
    echo "  Type: $dtype"
    echo "  Unique values: $uniq"
    echo "  Missing: $miss/${total} (${pct}%)"
    echo
  done
} > "$LOG"

echo "Report written to $LOG"
