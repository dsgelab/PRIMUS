

# Global vars
TEMP_DIR="/media/volume/Projects/DSGELabProject1/DiD_Highthroughput/Version2/GeneratePairs/temp"
mkdir -p "$TEMP_DIR"
N_THREADS=10
DATE=$(date +%Y%m%d)

# ----------------------------------------------------
# Step 1: Extract events

START_STEP1=$(date +%s)

# ATC extraction (5-char code)
INPUT_FILE="/media/volume/Projects/DSGELabProject1/ProcessedData/AllConnectedPurchases_20250421.csv"
OUTCOME_FILE="/media/volume/Projects/DSGELabProject1/DiD_Highthroughput/Version2/GeneratePairs/ATC_UniqueEvents_$DATE.csv"
if [ ! -f "$OUTCOME_FILE" ]; then
    echo "ID,CODE" > "$OUTCOME_FILE"
    tail -n +2 "$INPUT_FILE" | awk -F',' '{ print $1 "," substr($3,1,5) }' | sort --temporary-directory="$TEMP_DIR" --parallel="$N_THREADS" | uniq >> "$OUTCOME_FILE"
else
    echo "$OUTCOME_FILE already exists, skipping extraction."
fi

# ICD extraction (3-char code)
INPUT_FILE="/media/volume/Projects/DSGELabProject1/ProcessedData/AllConnectedDiagnosis_20250528.csv"
OUTCOME_FILE="/media/volume/Projects/DSGELabProject1/DiD_Highthroughput/Version2/GeneratePairs/ICD_UniqueEvents_$DATE.csv"
if [ ! -f "$OUTCOME_FILE" ]; then
    echo "ID,CODE" > "$OUTCOME_FILE"
    tail -n +2 "$INPUT_FILE" | awk -F',' '{ print $1 "," substr($3,1,3) }' | sort --temporary-directory="$TEMP_DIR" --parallel="$N_THREADS" | uniq >> "$OUTCOME_FILE"
else
    echo "$OUTCOME_FILE already exists, skipping extraction."
fi

END_STEP1=$(date +%s)
echo "Step 1 completed in $((END_STEP1 - START_STEP1)) seconds."


# ----------------------------------------------------
# Step 2: Prepare outcomes

START_STEP2=$(date +%s)

OUTCOME_FILE="/media/volume/Projects/DSGELabProject1/DiD_Highthroughput/Version2/GeneratePairs/OutcomeCounts_$DATE.csv"
if [ ! -f "$OUTCOME_FILE" ]; then
    python3 "/media/volume/Projects/DSGELabProject1/DiD_Highthroughput/Version2/GeneratePairs/A_prepare_outcomes.py" \
        --output_file "$OUTCOME_FILE"
else
    echo "$OUTCOME_FILE already exists, skipping outcome preparation."
fi

END_STEP2=$(date +%s)
echo "Step 2 completed in $((END_STEP2 - START_STEP2)) seconds."

# ----------------------------------------------------
# Step 3: Generate pairs (for both ATC and ICD events)

START_STEP3=$(date +%s)

python3 "/media/volume/Projects/DSGELabProject1/DiD_Highthroughput/Version2/GeneratePairs/B_calculate_pairs.py" \
    --events "/media/volume/Projects/DSGELabProject1/DiD_Highthroughput/Version2/GeneratePairs/ATC_UniqueEvents_$DATE.csv" \
    --outcomes "/media/volume/Projects/DSGELabProject1/DiD_Highthroughput/Version2/GeneratePairs/OutcomeCounts_$DATE.csv" \
    --doctor_list "/media/volume/Projects/DSGELabProject1/doctors_20250424.csv" \
    --output "/media/volume/Projects/DSGELabProject1/DiD_Highthroughput/Version2/GeneratePairs/ATC_pairs_$DATE.csv"

python3 "/media/volume/Projects/DSGELabProject1/DiD_Highthroughput/Version2/GeneratePairs/B_calculate_pairs.py" \
    --events "/media/volume/Projects/DSGELabProject1/DiD_Highthroughput/Version2/GeneratePairs/ICD_UniqueEvents_$DATE.csv" \
    --outcomes "/media/volume/Projects/DSGELabProject1/DiD_Highthroughput/Version2/GeneratePairs/OutcomeCounts_$DATE.csv" \
    --doctor_list "/media/volume/Projects/DSGELabProject1/doctors_20250424.csv" \
    --output "/media/volume/Projects/DSGELabProject1/DiD_Highthroughput/Version2/GeneratePairs/ICD_pairs_$DATE.csv"

END_STEP3=$(date +%s)
echo "Step 3 completed in $((END_STEP3 - START_STEP3)) seconds."

# ----------------------------------------------------
# Step 4: Clean up temporary files 

rm -rf "$TEMP_DIR"
echo "Temporary files cleaned up."
echo "All steps completed successfully."

