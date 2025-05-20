
# ------------------------------------------------
# PATHS: Modify as needed
# NB: some global paths/variables are used in multiple steps
base_dir='/media/volume/Projects/DSGELabProject1'  # directory that contains the longitudinal file and the filtered files

#STEP 2
list_of_doctors_spouses_children=$base_dir/doctors_and_spouses+children_20250305.csv
# filtered files were extracted using : FilterDiagnosisTHL.py and FilterPurchasesKela.py
filtered_THL_diagnosis=$base_dir/"ProcessedData/filtered_diagnosis_hilmo_20250310.csv"
filtered_kela_purchases=$base_dir/"ProcessedData/filtered_purchases_kela_20250310.csv"

event_register=''       # possible values: 'Diag', 'Purch'
event_code=''           # ICD10 if register is 'Diag', ATC code if register is 'Purch'

# STEP 3
filtered_outcomes="/media/volume/Projects/mattferr/TestPipeline/Outcomes_forRatio.csv"

# STEP 4
list_of_doctors=$base_dir/doctors_20250220.csv
map_relatives=$base_dir/doctors_and_relative_20250305.csv
covariates=$base_dir/doctor_characteristics_wlongest_Specialty_20250220.csv
outcome_code=''         # ATC code
use_relatives=''        # possible values: 'no', 'yes'

# ------------------------------------------------
# PIPELINE 
# STEP 1: Create experiment directory
today=$(date '+%Y%m%d')
out_dir="$base_dir/DiD_Experiments/Version/Experiment_$event_register-$event_code-$outcome_code-$today"
mkdir -p $out_dir

# Record the start time of the pipeline
start_time=$SECONDS

# STEP2: Extract desired event 
echo "Extracting desired event"
step_start_time=$SECONDS
if [[ "$event_register" == "Diag" ]]; then
    echo "ICD10 code: $event_code"
    python3 $base_dir/DiD_Pipeline/Version/ExtractEvents.py --id_list $list_of_doctors_spouses_children --inpath $filtered_THL_diagnosis --outdir $out_dir --event_code $event_code
elif [[ "$event_register" == "Purch" ]]; then
    echo "ATC code: $event_code"
    python3 $base_dir/DiD_Pipeline/Version/ExtractEvents.py --id_list $list_of_doctors_spouses_children --inpath $filtered_kela_purchases --outdir $out_dir --event_code $event_code
else
    echo "Invalid event register"
fi
step_end_time=$SECONDS
echo "Step completed in $(($step_end_time - $step_start_time)) seconds"

# STEP 3: Prepare outcome file
# will impute from purchases in the future, for the moment filtering all medications from longitudinal file (for study cohort)
# filtered outcomes were extracted using: ExtractOutcomes.py with outcome_code = ''

# STEP 4: Run Difference-in-Difference analysis
echo "Running DiD analysis"
step_start_time=$SECONDS
Rscript --vanilla DiD_analysis.R $list_of_doctors $map_relatives $use_relatives $out_dir/ID_cases.csv $out_dir/ID_controls.csv $out_dir/Events.csv $filtered_outcomes $outcome_code $covariates $out_dir
step_end_time=$SECONDS
echo "Step completed in $(($step_end_time - $step_start_time)) seconds"

# Finish
end_time=$SECONDS
echo "Pipeline completed"
echo "Total time taken: $(($end_time - $start_time)) seconds"