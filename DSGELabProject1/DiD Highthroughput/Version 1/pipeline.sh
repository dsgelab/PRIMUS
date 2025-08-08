
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
event_codes_file="$base_dir/DiD_Highthroughput/event_codes.txt"
outcome_codes_file="$base_dir/DiD_Highthroughput/outcome_codes.txt"

# Output directories
today=$(date '+%Y%m%d')
experiment_dir="$base_dir/DiD_Experiments/Version1_Highthroughput"
processed_events_dir="$experiment_dir/ProcessedEvents_${today}"
processed_outcomes_dir="$experiment_dir/ProcessedOutcomes_${today}"
results_dir="$experiment_dir/Results"

# Create directories
mkdir -p "$processed_events_dir" "$processed_outcomes_dir" "$results_dir"

# ------------------------------------------------
# PIPELINE EXECUTION
# ------------------------------------------------

pipeline_start_time=$SECONDS

# ------------------------------------------------
# STEP 1: Process all events
# ------------------------------------------------
echo ""
echo "=== STEP 1: Processing Events ==="
step1_start_time=$SECONDS

if [[ -f "$processed_events_dir/processed_events.parquet" ]]; then
    echo "Processed events already exist, skipping..."
else
    python3 "$base_dir/DiD_Highthroughput/Step1_ProcessEvents.py" \
        --id_list "$list_of_doctors" \
        --diagnosis_file "$diagnosis_file" \
        --purchases_file "$purchases_file" \
        --event_codes "$event_codes_file" \
        --outdir "$processed_events_dir"
fi

step1_end_time=$SECONDS
echo "Step 1 completed in $(($step1_end_time - $step1_start_time)) seconds"

# ------------------------------------------------
# STEP 2: Process all outcomes
# ------------------------------------------------
echo ""
echo "=== STEP 2: Processing Outcomes ==="
step2_start_time=$SECONDS

# Create temp directory for chunked processing
temp_dir="$processed_outcomes_dir/temp"
mkdir -p "$temp_dir"

if [[ -f "$processed_outcomes_dir/processed_outcomes.parquet" ]]; then
    echo "Processed outcomes already exist, skipping..."
else
    python3 "$base_dir/DiD_Highthroughput/Step2_ProcessOutcomes.py" \
        --id_list "$list_of_doctors" \
        --outcome_file "$outcome_file" \
        --outcome_codes "$outcome_codes_file" \
        --outdir "$processed_outcomes_dir" \
        --method chunked \
        --chunk_size 1000000 \
        --temp_dir "$temp_dir"
fi

step2_end_time=$SECONDS
echo "Step 2 completed in $(($step2_end_time - $step2_start_time)) seconds"

# ------------------------------------------------
# Copy event and outcome codes into processed folders
# ------------------------------------------------
cp "$event_codes_file" "$processed_events_dir/"
cp "$outcome_codes_file" "$processed_outcomes_dir/"

# ------------------------------------------------
# STEP 3: Run DiD analysis for all pairs
# ------------------------------------------------
echo ""
echo "=== STEP 3: Running DiD Analysis for All Pairs ==="
step3_start_time=$SECONDS

# Read event and outcome codes (one per line)
mapfile -t event_codes < "$event_codes_file"
mapfile -t outcome_codes < "$outcome_codes_file"

echo "Total pairs to analyze: $((${#event_codes[@]} * ${#outcome_codes[@]}))"

# Initialize results file as CSV
results_file="$results_dir/Results_${today}.csv"
echo "EVENT_CODE,OUTCOME_CODE,EFFECT_SIZE,P_VALUE,CI_LOWER,CI_UPPER,N_CASES,N_CONTROLS" > "$results_file"

pair_count=0
successful_pairs=0
failed_pairs=0

# Loop through all event-outcome pairs
for event_code in "${event_codes[@]}"; do
    for outcome_code in "${outcome_codes[@]}"; do
        pair_count=$((pair_count + 1))
        echo ""
        echo "--- Pair $pair_count: $event_code -> $outcome_code ---"

        # Check if this pair already exists in the results file (skip header)
        if grep -q "^${event_code},${outcome_code}," "$results_file"; then
            echo "Pair already analyzed, skipping..."
            successful_pairs=$((successful_pairs + 1))
            continue
        fi

        pair_start_time=$SECONDS

        # Run DiD analysis and save results to results file
        Rscript --vanilla "$base_dir/DiD_Highthroughput/DiD_analysis_highthroughput.R" \
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
echo "Pairs analyzed: $pair_count"
echo "Successful pairs: $successful_pairs"
echo "Failed / skipped pairs: $failed_pairs"
echo ""
echo "Results saved to: $results_file"
echo ""
echo "=== PIPELINE COMPLETED ==="