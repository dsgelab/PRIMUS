# This pipeline is responsible for runnign all plotting script in a specific order.
# Note that the script names might change, as well as their content. Make sure to update this pipeline accordingly.
# Make sure all scripts are in the same directory as this pipeline.

#!/bin/bash

# Run plotting scripts in order
scripts=(
    "Supplements_RelativeChange.R"
    "Supplements_DiD_EventTimeEffects.R"
    "Supplements_DiD_CaseCohortEffects.R"
    "Supplements_DiD_N_Ni_Y_CombinedPlots.R"
    "Figure5.R"
    "Supplements_RosuvastatinValidations.R"
    "Supplements_StratifiedAnalysis_Age.R"
    "Supplements_StratifiedAnalysis_Sex.R"
    "Supplements_StratifiedAnalysis_Specialty.R"
    "Supplements_StratifiedAnalysis_Specialty_V2.R"
    "Supplements_StratifiedAnalysis_SelfPrescription.R"
    "Supplements_StratifiedAnalysis_PrescriptionTier.R"
    "Supplements_PostDefinition.R"
    "Supplements_ATC_Landscape.R"
    "Supplements_ExperimentComparison.R"
)

# Check if all scripts exist before running
for script in "${scripts[@]}"; do
    if [[ ! -f "$script" ]]; then
        echo "✗ Error: $script not found"
        exit 1
    fi
done

# General start time
general_start=$(date +%s)
echo "Starting pipeline at $(date)"

for script in "${scripts[@]}"; do
    echo "Running $script..."
    script_start=$(date +%s)
    if Rscript "$script"; then
        script_end=$(date +%s)
        script_elapsed=$(($script_end - $script_start))
        echo "✓ $script completed successfully in ${script_elapsed} seconds"
    else
        echo "✗ $script failed"
        exit 1
    fi
done

general_end=$(date +%s)
total_elapsed=$(($general_end - $general_start))
echo "All scripts completed successfully in ${total_elapsed} seconds total!"
