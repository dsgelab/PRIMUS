
# NOTE: 
# before running this script make sure that all results file paths
# used in each script are correctly declared and dated

list_of_files=(
    "Supplements_DepressionDistress.R"
    "Supplements_DepressionDistress_SickLeave.R"
    "Supplements_Pregnancy.R"
    "Supplements_ExtraDiagnosis.R"
    "Figure3.R"
)

# Verify that the R script exists before attempting to run it.
for file in "${list_of_files[@]}"; do

    # plotting scripts are located in the same directory as this script
    path="./$file"
    if [[ ! -f "$path" ]]; then
        printf 'ERROR: file not found: %s\n' "$path" >&2
        continue
    fi
done

# Run the plot scripts
for file in "${list_of_files[@]}"; do

    # plotting scripts are located in the same directory as this script
    path="./$file"
    printf 'Running %s...\n' "$file"
    start_time=$(date +%s)

    if Rscript --vanilla "$path"; then
        status=0
    else
        status=$?
    fi

    elapsed=$(( $(date +%s) - start_time ))
    printf 'Finished %s in %s second(s).\n' "$file" "$elapsed"

    if (( status != 0 )); then
        printf 'ERROR: %s exited with status %s.\n' "$file" "$status" >&2
    fi
done