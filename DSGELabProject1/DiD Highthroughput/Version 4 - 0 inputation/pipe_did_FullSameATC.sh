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

# Event and outcome codes
event_codes_file="$base_dir/DiD_Highthroughput/Version3/GeneratePairs/Pairs_20251028_0707/event_codes_ATC_20251028_0707.csv"
outcome_codes_file="$base_dir/DiD_Highthroughput/Version3/GeneratePairs/Pairs_20251028_0707/ATC_codes_20251028_0707.csv"
pairs_file="$base_dir/DiD_Highthroughput/Version3/GeneratePairs/Pairs_20251028_0707/event_outcome_pairs_ATC_20251028_0707.csv"

# Output directories
today=$(date '+%Y%m%d')
experiment_dir="$base_dir/DiD_Experiments/Version4_Highthroughput"
processed_events_dir="$base_dir/DiD_Experiments/Version3_Highthroughput/ProcessedEvents_20251030"
processed_outcomes_dir="$base_dir/DiD_Experiments/Version3_Highthroughput/ProcessedOutcomes_20251030"
results_dir="$experiment_dir/Results"

# Create directories
mkdir -p "$processed_events_dir" "$processed_outcomes_dir" "$results_dir"

# ------------------------------------------------
# PIPELINE EXECUTION
# ------------------------------------------------

pipeline_start_time=$SECONDS

# ------------------------------------------------
# STEP 3: Run DiD analysis for specified pairs only
# ------------------------------------------------
echo ""
echo "=== STEP 3: Running DiD Analysis for Specified Pairs ==="
step3_start_time=$SECONDS

# Read pairs from CSV file (no header, format: event_code,outcome_code)
declare -a event_codes
declare -a outcome_codes

while IFS=',' read -r event_code outcome_code; do
    event_codes+=("$event_code")
    outcome_codes+=("$outcome_code")
done < "$pairs_file"

total_pairs=${#event_codes[@]}
echo "Total pairs to analyze: $total_pairs"

# Initialize results file as CSV
results_file="$results_dir/Results_ATC_${today}.csv"
echo "EVENT_CODE,OUTCOME_CODE,AVG_EFFECT_BEFORE,AVG_SE_BEFORE,AVG_EFFECT_AFTER,AVG_SE_AFTER,N_CASES,N_CONTROLS" > "$results_file"

successful_pairs=0
failed_pairs=0

# Loop through specified event-outcome pairs
for ((i=0; i<total_pairs; i++)); do
    event_code="${event_codes[i]}"
    outcome_code="${outcome_codes[i]}"
    pair_count=$((i + 1))
    
    echo ""
    echo "--- Pair $pair_count/$total_pairs: $event_code -> $outcome_code ---"
    pair_start_time=$SECONDS

    # Run DiD analysis and save results to results file
    Rscript --vanilla "$base_dir/DiD_Highthroughput/Version4/DiD_analysis_highthroughput.R" \
        "$processed_events_dir/processed_events.parquet" \
        "$processed_outcomes_dir/processed_outcomes.parquet" \
        "$event_code" \
        "$outcome_code" \
        "$list_of_doctors" \
        "$covariates" \
        "$results_file"

    exit_code=$?

    pair_end_time=$SECONDS
    pair_duration=$((pair_end_time - pair_start_time))

    if [[ $exit_code -eq 0 ]]; then
        echo "✓ Pair completed successfully in $pair_duration seconds"
        successful_pairs=$((successful_pairs + 1))
    else
        echo "✗ Pair failed or was skipped in $pair_duration seconds"
        failed_pairs=$((failed_pairs + 1))
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
echo "Pairs analyzed: $total_pairs"
echo "Successful pairs: $successful_pairs"
echo "Failed / skipped pairs: $failed_pairs"
echo ""
echo "Results saved to: $results_file"
echo ""
echo "=== PIPELINE COMPLETED ==="