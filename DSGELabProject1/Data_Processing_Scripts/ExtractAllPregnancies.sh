
# ------------------------------------------------
# PATHS: Modify as needed
# ------------------------------------------------

# input files
base_dir='/media/volume/Projects/DSGELabProject1'
diagnosis_file="$base_dir/ProcessedData/AllConnectedDiagnosis_20250528.csv"
event_code="O" # ICD-10 code prefix for pregnancy-related diagnoses

# Output directories
today=$(date '+%Y%m%d')
result_file="$base_dir/ProcessedData/AllPregnanciesEvents_${today}.csv"

# ------------------------------------------------
# SCRIPT: Extract all pregnancy-related events
# ------------------------------------------------  

echo "Extracting all pregnancy-related events from $diagnosis_file..."
start_time=$(date +%s)

# Find the position of the ICD10_CODE column
icd_col=$(head -1 "$diagnosis_file" | tr ',' '\n' | grep -n '^ICD10_CODE$' | cut -d: -f1)

# Extract rows where ICD10_CODE starts with event_code
awk -F',' -v col="$icd_col" -v code="$event_code" 'NR==1 || index($col, code)==1 { print }' "$diagnosis_file" > "$result_file"

end_time=$(date +%s)
elapsed=$((end_time - start_time))
echo "Extraction complete. Results saved to $result_file."
echo "Elapsed time: ${elapsed} seconds."

