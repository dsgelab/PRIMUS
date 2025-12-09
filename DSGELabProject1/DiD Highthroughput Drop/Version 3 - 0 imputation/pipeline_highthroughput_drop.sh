# ------------------------------------------------
# PATHS: Modify as needed
# ------------------------------------------------

base_dir='/media/volume/Projects/DSGELabProject1'

# Input files
list_of_doctors="$base_dir/doctors_20250424.csv"
list_of_doctors_spouses_children="$base_dir/doctors_and_spouses+children_20250521.csv"
diagnosis_file="$base_dir/ProcessedData/AllConnectedDiagnosis_20250528.csv"
purchases_file="$base_dir/ProcessedData/AllConnectedPurchases_FirstEvents_20250421.csv"
outcome_file="/media/volume/Projects/mattferr/did_pipeline/Outcomes_ForRatio_20250506.csv"
covariates="$base_dir/doctor_characteristics_20250520.csv"

# Event codes
event_codes_file="$base_dir/DiD_Highthroughput/Version3/GeneratePairs/Pairs_20251028_0707/event_codes_ICD_20251028_0707.csv"

# Output directories
today=$(date '+%Y%m%d')
experiment_dir="$base_dir/DiD_Experiments/Version3_Highthroughput_drop"
processed_events_dir="$base_dir/DiD_Experiments/Version2_Highthroughput_drop/ProcessedEvents_20251028"
processed_outcomes_dir="$base_dir/DiD_Experiments/Version2_Highthroughput_drop/ProcessedOutcomes_20251028"
results_dir="$experiment_dir/Results"

# Create directories
mkdir -p "$results_dir"

# ------------------------------------------------
# PIPELINE EXECUTION
# ------------------------------------------------

pipeline_start_time=$SECONDS

# ------------------------------------------------
# STEP 3: Run Drop analysis
# ------------------------------------------------
echo ""
echo "=== STEP 3: Running Drop Analysis for Event Codes ==="
step3_start_time=$SECONDS

# Read event codes from file (skip header if present)
event_codes=()
while IFS=',' read -r event_code _; do
    [[ "$event_code" == "EVENT_CODE" ]] && continue
    event_codes+=("$event_code")
done < "$event_codes_file"

total_events=${#event_codes[@]}
echo "Total event codes to analyze: $total_events"

# Initialize results file with correct header
results_file="$results_dir/DropAnalysisResults_${today}.csv"
echo "EVENT_CODE,ATT_DROP,SE_DROP,N_CASES,N_CONTROLS" > "$results_file"

successful_events=0
failed_events=0

for ((i=0; i<total_events; i++)); do
    event_code="${event_codes[i]}"
    event_count=$((i + 1))

    echo ""
    echo "--- Event $event_count/$total_events: $event_code ---"
    event_start_time=$SECONDS

    # Run DropAnalysis.R for each event code
    Rscript --vanilla "$base_dir/DiD_Highthroughput/Version3_drop/DropAnalysis.R" \
        "$list_of_doctors" \
        "$processed_events_dir/processed_events.parquet" \
        "$event_code" \
        "$processed_outcomes_dir/processed_outcomes.parquet" \
        "$covariates" \
        "$results_file"

    exit_code=$?

    event_end_time=$SECONDS
    event_duration=$((event_end_time - event_start_time))

    if [[ $exit_code -eq 0 ]]; then
        echo "✓ Event completed successfully in $event_duration seconds"
        successful_events=$((successful_events + 1))
    else
        echo "✗ Event failed or was skipped in $event_duration seconds"
        failed_events=$((failed_events + 1))
    fi
done

step3_end_time=$SECONDS
echo ""
echo "Step 3 completed in $(($step3_end_time - $step3_start_time)) seconds"

# ------------------------------------------------
# SUMMARY
# ------------------------------------------------
pipeline_end_time=$SECONDS
total_duration=$((pipeline_end_time - pipeline_start_time))

echo ""
echo "=== PIPELINE SUMMARY ==="
echo "Total duration: $total_duration seconds ($((total_duration / 60)) minutes)"
echo "Pairs analyzed: $total_events"
echo "Successful pairs: $successful_events"
echo "Failed / skipped pairs: $failed_events"
echo ""
echo "Results saved to: $results_file"
echo ""
echo "=== PIPELINE COMPLETED ==="
