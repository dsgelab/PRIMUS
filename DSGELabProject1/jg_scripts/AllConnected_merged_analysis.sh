#!/usr/bin/env bash
# ----------------------------------------------------------------------------
# Script Name: all_prescriptions_analysis.sh
# Description: Three analyses on all_prescriptions.csv.gz:
#   1) compare unique imputed vs original prescriptions
#   2) sensitivity: drop last original when ≥2, then repeat 1
#   3) missingness of DOCTOR_ID by SOURCE
# Only uses records from 2015 onwards.
# ----------------------------------------------------------------------------

set -euo pipefail
IFS=$'\n\t'

####### USER CONFIGURATION (edit these) #######
INPUT_FILE="/XXX/path/to/output/all_prescriptions.csv.gz"
OUTPUT_DIR="/XXX/path/to/output"
SORT_TMPDIR="/XXX/path/to/output/sort_tmp"
YEAR_CUTOFF=2015
##############################################

mkdir -p "$OUTPUT_DIR" "$SORT_TMPDIR"

LOG_FILE="${OUTPUT_DIR}/all_prescriptions_analysis_$(date +%Y%m%d_%H%M%S).log"
log(){ echo "[$(date +'%F %T')] $*" | tee -a "$LOG_FILE"; }
trap 'log "ERROR at line $LINENO"; exit 1' ERR

#######################
# 0) Preflight & Header
#######################
log "Starting analysis on '$INPUT_FILE'"
[[ -r "$INPUT_FILE" ]] || { log "ERROR: cannot read $INPUT_FILE"; exit 1; }

# Robustly read the full header (in case SOURCE wrapped to next line)
# --- robust header read ---
log "Reading header from $INPUT_FILE"

# turn off pipefail so SIGPIPE from zcat→head isn't fatal
set +o pipefail
raw1=$(zcat "$INPUT_FILE" | head -n1)
set -o pipefail

# if it somehow failed entirely, abort
if [[ -z "$raw1" ]]; then
  log "ERROR: couldn’t read header from $INPUT_FILE"
  exit 1
fi
if ! grep -q 'SOURCE' <<<"$raw1"; then
  raw2=$(zcat "$INPUT_FILE" | sed -n '2p')
  header="${raw1}${raw2}"
else
  header="$raw1"
fi
log "Header: $header"

# Map column names → positions
IFS=',' read -r -a cols <<<"$header"
declare -A idx
for i in "${!cols[@]}"; do
  idx["${cols[$i]}"]=$((i+1))
done
for col in PATIENT_ID CODE PRESCRIPTION_DATE SOURCE DOCTOR_ID; do
  [[ -n "${idx[$col]:-}" ]] \
    || { log "ERROR: missing column '$col' in header"; exit 1; }
done

pid_col=${idx[PATIENT_ID]}
code_col=${idx[CODE]}
date_col=${idx[PRESCRIPTION_DATE]}
src_col=${idx[SOURCE]}
doc_col=${idx[DOCTOR_ID]}

log "Columns → PATIENT_ID=$pid_col, CODE=$code_col, PRESC_DATE=$date_col, SOURCE=$src_col, DOCTOR_ID=$doc_col"

# helper to mktemp in our sort‐tmp dir
mktemp_sort() { mktemp --tmpdir="$SORT_TMPDIR" all_presc.XXXXXX; }

############################
# 1) Imputed vs Original
############################
log "=== Analysis 1: Unique imputed vs original prescriptions ==="

# 1a) extract unique (key,source) pairs for year>=YEAR_CUTOFF
tmp_key_src=$(mktemp_sort)
zcat "$INPUT_FILE" \
  | awk -F',' -v pid="$pid_col" -v code="$code_col" -v date="$date_col" \
        -v src="$src_col" -v yr="$YEAR_CUTOFF" '
    NR>1 && substr($date,1,4) >= yr {
      key = $pid FS $code FS $date
      print key FS $src
    }
  ' \
  | sort \
      --temporary-directory="$SORT_TMPDIR" \
      --buffer-size=200M \
      --parallel="$(nproc)" \
      -u \
  > "$tmp_key_src"

# 1b) group by key and tally
awk -F'|' -v OFS=' ' '
  # but our tmp_key_src is comma-delimited, so override FS:
  BEGIN { FS=OFS="," }
  {
    key = $1 FS $2 FS $3
    src = $4
    if (key != prev) {
      if (NR>1) {
        if (have_imp && !have_orig) imp_only++
        else if (have_orig && !have_imp) orig_only++
        else if (have_imp && have_orig) both++
      }
      prev = key; have_imp=have_orig=0
    }
    if (src == "Imputed_Prescription") have_imp=1
    else if (src == "Prescription") have_orig=1
  }
  END {
    if (have_imp && !have_orig) imp_only++
    else if (have_orig && !have_imp) orig_only++
    else if (have_imp && have_orig) both++
    total_imp = imp_only + both
    total_orig = orig_only + both
    printf "Total unique imputed: %d\n", total_imp
    printf "Total unique original: %d\n", total_orig
    printf "Imputed only: %d (%.2f%% of imputed)\n", imp_only, (imp_only/total_imp)*100
    printf "Original only: %d (%.2f%% of original)\n", orig_only, (orig_only/total_orig)*100
    printf "Matched (both): %d\n", both
  }
' "$tmp_key_src" | tee -a "$LOG_FILE"

rm "$tmp_key_src"

##################################
# 2) Sensitivity (drop last orig)
##################################
log "=== Analysis 2: Sensitivity (drop last original if ≥2) ==="
# 2a) year‐filtered full data
tmp_filt=$(mktemp_sort)
zcat "$INPUT_FILE" \
  | awk -F',' -v date="$date_col" -v src="$src_col" -v yr="$YEAR_CUTOFF" '
      NR==1 || (NR>1 && substr($date,1,4) >= yr)
    ' \
  > "$tmp_filt"

# 2b) isolate original & sort by pid,code,date
tmp_orig_all=$(mktemp_sort)
awk -F',' -v src="$src_col" '
  NR==1 || $src=="Prescription"
' "$tmp_filt" \
  > "$tmp_orig_all"

tmp_orig_sorted=$(mktemp_sort)
sort \
  --temporary-directory="$SORT_TMPDIR" \
  --buffer-size=200M \
  --parallel="$(nproc)" \
  -t, -k${pid_col},${pid_col} -k${code_col},${code_col} -k${date_col},${date_col} \
  "$tmp_orig_all" \
  > "$tmp_orig_sorted"

# 2c) drop last per (pid,code) when count≥2
tmp_sens_orig=$(mktemp_sort)
awk -F',' -v pid="$pid_col" -v code="$code_col" -v date="$date_col" '
  {
    key = $pid FS $code
    if (key != prev) {
      if (NR>1) {
        if (count==1) print prev_line
      }
      prev_line=$0; prev=key; count=1
    } else {
      print prev_line
      prev_line=$0; count++
    }
  }
  END { if (count==1) print prev_line }
' "$tmp_orig_sorted" > "$tmp_sens_orig"

# 2d) isolate imputed (no change except year filtering)
tmp_sens_imp=$(mktemp_sort)
awk -F',' -v src="$src_col" '
  NR==1 || $src=="Imputed_Prescription"
' "$tmp_filt" > "$tmp_sens_imp"

# 2e) repeat the unique-compare logic
tmp_key_src2=$(mktemp_sort)
cat "$tmp_sens_orig" "$tmp_sens_imp" \
  | awk -F',' -v pid="$pid_col" -v code="$code_col" -v date="$date_col" -v src="$src_col" '
      NR>1 {
        key = $pid FS $code FS $date
        print key FS $src
      }
    ' \
  | sort \
      --temporary-directory="$SORT_TMPDIR" \
      --buffer-size=200M \
      --parallel="$(nproc)" \
      -u \
  > "$tmp_key_src2"

awk -F',' -v OFS=' ' '
  BEGIN { imp_only=orig_only=both=0 }
  {
    key=$1 FS $2 FS $3; src=$4
    if (key != prev) {
      if (NR>1) {
        if (have_imp && !have_orig) imp_only++
        else if (have_orig && !have_imp) orig_only++
        else if (have_imp && have_orig) both++
      }
      prev=key; have_imp=have_orig=0
    }
    if (src=="Imputed_Prescription") have_imp=1
    else if (src=="Prescription") have_orig=1
  }
  END {
    if (have_imp && !have_orig) imp_only++
    else if (have_orig && !have_imp) orig_only++
    else if (have_imp && have_orig) both++
    total_imp=imp_only+both
    total_orig=orig_only+both
    printf "After dropping last original when ≥2:\n"
    printf "  Total imputed: %d\n", total_imp
    printf "  Total original: %d\n", total_orig
    printf "  Imputed only: %d (%.2f%%)\n", imp_only, (imp_only/total_imp)*100
    printf "  Original only: %d (%.2f%%)\n", orig_only, (orig_only/total_orig)*100
    printf "  Matched: %d\n", both
  }
' "$tmp_key_src2" | tee -a "$LOG_FILE"

# clean up
rm "$tmp_filt" "$tmp_orig_all" "$tmp_orig_sorted" "$tmp_sens_orig" "$tmp_sens_imp" "$tmp_key_src2"

##################################
# 3) Doctor_ID missingness
##################################
log "=== Analysis 3: DOCTOR_ID missingness by SOURCE ==="
zcat "$INPUT_FILE" \
  | awk -F',' -v doc="$doc_col" -v src="$src_col" -v date="$date_col" -v yr="$YEAR_CUTOFF" '
      NR>1 && substr($date,1,4) >= yr {
        s = $src
        total[s]++
        if ($doc=="" || $doc=="NA") missing[s]++
      }
    END {
      for (st in total) {
        printf "Source: %s\n", st
        printf "  Total rows: %d\n", total[st]
        printf "  Missing DOCTOR_ID: %d (%.2f%%)\n\n", missing[st], (missing[st]/total[st])*100
      }
    }
  ' | tee -a "$LOG_FILE"

log "All analyses completed."