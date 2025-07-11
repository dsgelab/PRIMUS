# ------------------------------------------------
# PATHS: Modify as needed
# NB: some global paths/variables are used in multiple steps
base_dir='/media/volume/Projects/DSGELabProject1'  # directory that contains the longitudinal file and the filtered files

#STEP 2
list_of_doctors_spouses_children=$base_dir/doctors_and_spouses+children_20250305.csv
diagnosis_file=$base_dir/ProcessedData/AllConnectedDiagnosis_20250528.csv
purchases_file=$base_dir/ProcessedData/imputed_prescriptions_20250501152849.csv.gz

# STEP 3
list_of_doctors=$base_dir/doctors_20250424.csv
covariates=$base_dir/doctor_characteristics_20250520.csv
outcome_file="/media/volume/Projects/mattferr/did_pipeline/Outcomes_ForRatio_20250506.csv"

# ------------------------------------------------
# DEFINE PAIRS: List of (event_code, outcome_code) pairs
pairs=(
    # Prescription & Use of Psychoactive medications
    "Purch Antidepressants ^N06A Antidepressants ^N06A" 
    "Purch HypnoticsSedatives ^N05C HypnoticsSedatives ^N05C" 
    "Purch Anxiolytics ^N05B Anxiolytics ^N05B" 
    "Purch Antipsychotics ^N05A Antipsychotics ^N05A" 
    "Purch Psychostimulants ^N06D Psychostimulants ^N06D" 
    "Purch AntiDementiaDrugs ^N06B AntiDementiaDrugs ^N06B" 
    # Prescription & Use of Pain-relief medications
    "Purch Opioids ^N02A Opioids ^N02A"
    # Diagnosis of non-debiliotating cardiovascular diseases
    "Diag I9_CHD ^(I20.0|I200|I21|I22) Statins C10AA"
    "Diag I9_ANGINA ^I20 Statins C10AA"
    "Diag I9_CORATHER ^(I24|I25|T82.2|T822|Z95.1|Z951) Statins C10AA"
    "Diag I9_AF ^I48 Statins C10AA"
    "Diag I9_OTHARR ^I49 Statins C10AA"
    "Diag I9_ATHSCLE ^I70 Statins C10AA"
    "Diag I9_PHLETHROMBDVTLOW ^(I80.1|I80.20|I80.29) Statins C10AA"
    "Diag I9_VEINSOTH ^I87 Statins C10AA"
    "Diag I9_NSTEMI ^I21.4 Statins C10AA"
    # Prescription & Use of Cardiovascular medications for primary prevention
    "Purch Antihypertensives ^C02 Antihypertensives ^C02"
    "Purch Diuretics ^C03 Diuretics ^C03"
    "Purch BetaBlockers ^C07 BetaBlockers ^C07"
    "Purch CalciumChannelBlockers ^C08 CalciumChannelBlockers ^C08"
    "Purch ACEi ^(C09A|C09B) ACEi ^(C09A|C09B)"
    "Purch ARBs ^(C09C|C09D) ARBs ^(C09C|C09D)"
    "Purch Statins ^C10AA Statins ^C10AA"
    "Purch Fibrates ^C10AB Fibrates ^C10AB"
    "Purch FactorXaInhibitors ^B01AF FactorXaInhibitors ^B01AF"
    "Purch VitKantagonist ^B01AA VitKantagonist ^B01AA"
    "Purch Aspirin ^(B01AC06|B01AC56) Aspirin ^(B01AC06|B01AC56)"
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
    out_dir="$base_dir/DiD_Experiments/Version5/Experiment_${event_code_name}_${outcome_code_name}_$today"
    mkdir -p $out_dir

    # Record the start time of the pipeline
    start_time=$SECONDS

    # STEP2: Extract desired event (skip if Events.csv exists)
    if [[ -f "$out_dir/Events.csv" ]]; then
        echo "Events already extracted, SKIP EXTRACTION"
    else
        echo "Extracting desired event"
        step_start_time=$SECONDS
        if [[ "$event_register" == "Diag" ]]; then
            echo "ICD10 code: $event_code"
            python3 $base_dir/DiD_Pipeline/Version5_20250709/ExtractEvents.py --id_list $list_of_doctors_spouses_children --inpath $diagnosis_file --event_register $event_register --outdir $out_dir --event_code $event_code
        elif [[ "$event_register" == "Purch" ]]; then
            echo "ATC code: $event_code"
            python3 $base_dir/DiD_Pipeline/Version5_20250709/ExtractEvents.py --id_list $list_of_doctors_spouses_children --inpath $purchases_file --event_register $event_register --outdir $out_dir --event_code $event_code
        else
            echo "Invalid event register"
        fi
        step_end_time=$SECONDS
        echo "Step completed in $(($step_end_time - $step_start_time)) seconds"
    fi

    # STEP 3: Run Difference-in-Difference analysis
    echo "Running DiD analysis"
    step_start_time=$SECONDS
    Rscript --vanilla DiD_analysis.R $list_of_doctors $out_dir/Events.csv $event_code $outcome_file $outcome_code $covariates $out_dir
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