
# ------------------------------------------------
# Modify as needed
base_dir='/media/volume/Projects/DSGELabProject1'  # directory that contains the longitudinal file and the filtered files

longitudinal_file='doctor_patient_longitudinal_20250220.csv.gz'
filtered_THL_diagnosis="filtered_diagnosis_hilmo_20250310.csv"
filtered_kela_purchases="filtered_purchases_kela_20250310.csv"
filtered_outcomes="/media/volume/Projects/mattferr/TestPipeline/Outcomes_forRatio.csv"

list_of_doctors=$base_dir/doctors_20250220.csv
list_of_doctors_spouses_children=$base_dir/doctors_and_spouses+children_20250305.csv
map_relatives=$base_dir/doctors_and_relative_20250305.csv

event_register=''       # possible values: 'Diag', 'Purch'
event_code=''           # ICD10 if register is 'Diag', ATC code if register is 'Purch'
outcome_code=''         # ATC code
use_relatives=''        # possible values: 'no', 'yes'

# ------------------------------------------------
# Pipeline 
# Step 1: Create experiment directory
today=$(date '+%Y%m%d')
out_dir="$base_dir/Experiments/Experiment_$event_register-$event_code-$outcome_register-$outcome_code-$today"
mkdir -p $out_dir

# Record the start time of the pipeline
start_time=$SECONDS

# Step 2: Extract desired event 
echo "Extracting desired event"
step_start_time=$SECONDS
if [ "$event_register" == "Diag" ]; then
    echo "ICD10 code: $event_code"
    python3 $base_dir/AnalysisPipeline/Version2/ExtractEvents.py --id_list $list_of_doctors_spouses_children --inpath $base_dir/$filtered_THL_diagnosis --outdir $out_dir --event_code $event_code
elif [ "$event_register" == "Purch" ]; then
    echo "ATC code: $event_code"
    python3 $base_dir/AnalysisPipeline/Version2/ExtractEvents.py --id_list $list_of_doctors_spouses_children --inpath $base_dir/$filtered_kela_purchases --outdir $out_dir --event_code $event_code
else
    echo "Invalid event register"
fi
step_end_time=$SECONDS
echo "Step completed in $(($step_end_time - $step_start_time)) seconds"

# Step 4: Run analysis
echo "Running DiD analysis"
step_start_time=$SECONDS
Rscript --vanilla DiD_analysis.R $list_of_doctors $map_relatives $use_relatives $out_dir/ID_cases.csv $out_dir/ID_controls.csv $out_dir/Events.csv $filtered_outcomes $outcome_code $out_dir
step_end_time=$SECONDS
echo "Step completed in $(($step_end_time - $step_start_time)) seconds"

# Finish
end_time=$SECONDS
echo "Pipeline completed"
echo "Total time taken: $(($end_time - $start_time)) seconds"