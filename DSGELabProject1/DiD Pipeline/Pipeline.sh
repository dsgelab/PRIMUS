# ------------------------------------------------
# PATHS: Modify as needed
# NB: some global paths/variables are used in multiple steps
base_dir='/media/volume/Projects/DSGELabProject1'  # directory that contains the longitudinal file and the filtered files

#STEP 2
list_of_doctors_spouses_children=$base_dir/doctors_and_spouses+children_20250305.csv
diagnosis_file=$base_dir/"ProcessedData/AllConnectedDiagnosis_20250528.csv"
purchases_file=$base_dir/"ProcessedData/imputed_prescriptions_20250501152849.csv.gz"

# STEP 3
list_of_doctors=$base_dir/doctors_20250424.csv
covariates=$base_dir/doctor_characteristics_wlongest_Specialty_20250220.csv
specialty=$base_dir/condensed_specialty_dict_csv
outcome_file="/media/volume/Projects/mattferr/did_pipeline/Outcomes_forRatio_20250506.csv"

# ------------------------------------------------
# DEFINE PAIRS: List of (event_code, outcome_code) pairs
pairs=(
    "Diag CHD ^(I20.0|I21|I22) Statins C10AA" 
)

# ------------------------------------------------
# PIPELINE 
# Record the start time of the entire pipeline
pipeline_start_time=$SECONDS

# Loop through each pair
for pair in "${pairs[@]}"; do
    # Parse the event/outcome pair
    event_register=$(echo $pair | cut -d' ' -f1)
    event_code_name=$(echo $pair | cut -d' ' -f2)
    event_code=$(echo $pair | cut -d' ' -f3)
    outcome_code_name=$(echo $pair | cut -d' ' -f4)
    outcome_code=$(echo $pair | cut -d' ' -f5)
        
    # STEP 1: Create experiment directory
    today=$(date '+%Y%m%d')
    out_dir="$base_dir/DiD_Experiments/Version4/Experiment_${event_code_name}_${outcome_code_name}_$today"
    mkdir -p $out_dir

    # Record the start time of the pipeline
    start_time=$SECONDS

    # STEP2: Extract desired event 
    echo "Extracting desired event"
    step_start_time=$SECONDS
    if [[ "$event_register" == "Diag" ]]; then
        echo "ICD10 code: $event_code"
        python3 $base_dir/DiD_Pipeline/Version4_20250611/ExtractEvents.py --id_list $list_of_doctors_spouses_children --inpath $diagnosis_file --event_register $event_register --outdir $out_dir --event_code $event_code
    elif [[ "$event_register" == "Purch" ]]; then
        echo "ATC code: $event_code"
        python3 $base_dir/DiD_Pipeline/Version4_20250611/ExtractEvents.py --id_list $list_of_doctors_spouses_children --inpath $purchases_file --event_register $event_register --outdir $out_dir --event_code $event_code
    else
        echo "Invalid event register"
    fi
    step_end_time=$SECONDS
    echo "Step completed in $(($step_end_time - $step_start_time)) seconds"

    # STEP 3: Run Difference-in-Difference analysis
    echo "Running DiD analysis"
    step_start_time=$SECONDS
    Rscript --vanilla DiD_analysis.R $list_of_doctors $out_dir/Events.csv $event_code $outcome_file $outcome_code $covariates $specialty $out_dir
    step_end_time=$SECONDS
    echo "Step completed in $(($step_end_time - $step_start_time)) seconds"

    # Finish current pair
    end_time=$SECONDS
    echo "Pair ($event_code_name, $outcome_code_name) completed"
    echo "Time taken for this pair: $(($end_time - $start_time)) seconds"
    echo "----------------------------------------"
done

# Finish entire pipeline
pipeline_end_time=$SECONDS
echo "All pairs completed"
echo "Total pipeline time: $(($pipeline_end_time - $pipeline_start_time)) seconds"