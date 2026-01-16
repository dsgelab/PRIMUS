#!/bin/bash

# Global configuration
DATE=$(date +%Y%m%d)
BASE_DIR="/media/volume/Projects/DSGELabProject1/ProcessedData/"
OUTPUT_DIR="${BASE_DIR}/Pairs_${DATE}"
TEMP_DIR="${OUTPUT_DIR}/temp"
N_THREADS=10
DOCTOR_LIST="/media/volume/Projects/DSGELabProject1/doctors_20250424.csv"

# Create output directory structure
mkdir -p "$OUTPUT_DIR"
mkdir -p "$TEMP_DIR"

echo "Starting event count analysis at $(date)"
echo "Output directory: $OUTPUT_DIR"
echo "================================================"

# ============================================================
# Step 0: Prepare doctor ID lookup file
# ============================================================

echo "Step 0: Preparing doctor ID list..."
START_STEP0=$(date +%s)

DOCTOR_SORTED="${TEMP_DIR}/doctors_sorted.txt"
sort --temporary-directory="$TEMP_DIR" --parallel="$N_THREADS" -u "$DOCTOR_LIST" > "$DOCTOR_SORTED"

DOCTOR_COUNT=$(wc -l < "$DOCTOR_SORTED")
END_STEP0=$(date +%s)
echo "✓ Loaded $DOCTOR_COUNT unique doctor IDs"
echo "✓ Completed in $((END_STEP0 - START_STEP0)) seconds"
echo ""

# ============================================================
# Step 1: Process ATC codes (7-char prefix)
# ============================================================

echo "Step 1: Processing ATC codes..."
START_STEP1=$(date +%s)

INPUT_ATC="/media/volume/Projects/DSGELabProject1/ProcessedData/AllConnectedPurchases_20250421.csv"
OUTPUT_ATC="${OUTPUT_DIR}/ATC_AllCounts.csv"

# Extract ID and 7-char ATC code, filter to doctor IDs only, sort, unique ID-CODE pairs, count all
tail -n +2 "$INPUT_ATC" | \
    awk -F',' '{print $1 "," substr($3,1,7)}' | \
    sort --temporary-directory="$TEMP_DIR" --parallel="$N_THREADS" | \
    join -t',' -1 1 -2 1 - "$DOCTOR_SORTED" | \
    sort --temporary-directory="$TEMP_DIR" --parallel="$N_THREADS" -u | \
    awk -F',' '{count[$2]++} END {for (code in count) print code "," count[code]}' | \
    awk 'BEGIN {print "CODE,UNIQUE_ID_COUNT"} {print}' > "$OUTPUT_ATC"

ATC_COUNT=$(tail -n +2 "$OUTPUT_ATC" | wc -l)
END_STEP1=$(date +%s)
echo "✓ Found $ATC_COUNT unique ATC codes"
echo "✓ Output: $OUTPUT_ATC"
echo "✓ Completed in $((END_STEP1 - START_STEP1)) seconds"
echo ""

# ============================================================
# Step 2: Process ICD codes (3-char prefix)
# ============================================================

echo "Step 2: Processing ICD codes..."
START_STEP2=$(date +%s)

INPUT_ICD="/media/volume/Projects/DSGELabProject1/ProcessedData/AllConnectedDiagnosis_20250528.csv"
OUTPUT_ICD="${OUTPUT_DIR}/ICD_AllCounts.csv"

# Extract ID and 3-char ICD code, filter to doctor IDs only, sort, unique ID-CODE pairs, count all
tail -n +2 "$INPUT_ICD" | \
    awk -F',' '{print $1 "," substr($3,1,3)}' | \
    sort --temporary-directory="$TEMP_DIR" --parallel="$N_THREADS" | \
    join -t',' -1 1 -2 1 - "$DOCTOR_SORTED" | \
    sort --temporary-directory="$TEMP_DIR" --parallel="$N_THREADS" -u | \
    awk -F',' '{count[$2]++} END {for (code in count) print code "," count[code]}' | \
    awk 'BEGIN {print "CODE,UNIQUE_ID_COUNT"} {print}' > "$OUTPUT_ICD"

ICD_COUNT=$(tail -n +2 "$OUTPUT_ICD" | wc -l)
END_STEP2=$(date +%s)
echo "✓ Found $ICD_COUNT unique ICD codes"
echo "✓ Output: $OUTPUT_ICD"
echo "✓ Completed in $((END_STEP2 - START_STEP2)) seconds"
echo ""

# ============================================================
# Step 3: Generate combined summary
# ============================================================

echo "Step 3: Generating summary..."
START_STEP3=$(date +%s)

SUMMARY_FILE="${OUTPUT_DIR}/summary.txt"

cat > "$SUMMARY_FILE" << EOF
All Event Counts Analysis Summary
==================================
Date: $(date)
Output Directory: $OUTPUT_DIR
Doctor List: $DOCTOR_LIST
Total Doctor IDs: $DOCTOR_COUNT

ATC Codes (7-char):
  - Total unique codes: $ATC_COUNT
  - Output file: ATC_AllCounts.csv

ICD Codes (3-char):
  - Total unique codes: $ICD_COUNT
  - Output file: ICD_AllCounts.csv

Processing Time:
  - Doctor list preparation: $((END_STEP0 - START_STEP0)) seconds
  - ATC: $((END_STEP1 - START_STEP1)) seconds
  - ICD: $((END_STEP2 - START_STEP2)) seconds
  - Total: $((END_STEP2 - START_STEP0)) seconds
EOF

END_STEP3=$(date +%s)
echo "✓ Summary written to: $SUMMARY_FILE"
echo ""

# ============================================================
# Step 4: Cleanup
# ============================================================

echo "Step 4: Cleaning up temporary files..."
rm -rf "$TEMP_DIR"
echo "✓ Temporary directory cleaned"
echo ""

# ============================================================
# Final summary
# ============================================================

TOTAL_TIME=$((END_STEP2 - START_STEP0))
echo "================================================"
echo "All steps completed successfully!"
echo "Total processing time: $TOTAL_TIME seconds"
echo "Output directory: $OUTPUT_DIR"
echo "Doctor IDs used: $DOCTOR_COUNT"
echo "ATC codes found: $ATC_COUNT"
echo "ICD codes found: $ICD_COUNT"
echo "================================================"