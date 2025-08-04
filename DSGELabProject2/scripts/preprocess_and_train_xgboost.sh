#!/bin/bash

set -e

start_time=$(date +%s)

if [ $# -lt 1 ]; then
    echo "Usage: $0 <icd10_code> [mode]"
    exit 1
fi

icd10_code="$1"
icd10_code_no_dot="${icd10_code//./}"
mode="${2:-test}"

exec > >(tee "logs/preprocess_and_train_xgboost_${icd10_code_no_dot}.log") 2>&1

# Create csv for patients having a diagnosis with the given icd10 code, and patient diagnosis & prescription history
Rscript create_diagnoses_and_patient_history.R --icd10_code "$icd10_code"

# Create csv for doctor diagnosis and prescription history
python3 doctor_history.py --icd10_code "$icd10_code"

# Create csv for patients having a diagnosis with the given icd10 code and a label for whether they were prescribed antibiotics for the given icd10 code
Rscript create_diagnoses_with_prescriptions.R --icd10_code "$icd10_code"

# Create csv train and test sets
python3 preprocess_xgboost.py --outdir XGBoost --inputfile "$icd10_code_no_dot"DiagnosesWithPrescriptions --suffix "$icd10_code_no_dot"

# Train XGBoost model
if [ "$mode" = "test" ]; then
    suffix="${icd10_code_no_dot}T" # T for transferred model
else
    suffix="$icd10_code_no_dot"
fi
python3 train_xgboost.py --outdir XGBoost/results --trainfile XGBoost/xgboost_train_"$icd10_code_no_dot".csv --testfile XGBoost/xgboost_test_"$icd10_code_no_dot".csv \
    --balanced 0 --dropna 0 --valsize 0.1 --dsize 1 --nproc 16 --nsearch 100 --shapdsize auto --fitlc 1 --suffix "$suffix" --mode "$mode" \
    --modelfile XGBoost/results/xgb_model_2025-07-17-1457.pkl --testfileorig XGBoost/xgboost_test_all.csv

end_time=$(date +%s)
elapsed=$(( end_time - start_time ))
echo "Total elapsed time: $elapsed seconds"