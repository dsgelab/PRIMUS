import pandas as pd
import numpy as np
import json
from datetime import datetime
from utils import find_latest_file_by_date, format_seconds_to_hms
import time
from pathlib import Path

INDIR = Path("/media/volume/Projects/mikael/ProcessedData/")
OUTDIR = Path("/media/volume/Projects/mikael/ProcessedData/")
DIAGNOSIS_DTYPES = {"PATIENT_ID": "str", "VISIT_DATE": "str", "ICD10_CODE": "str", "DOCTOR_ID": "str"}
CHUNK_SIZE = 20_000_000


def get_patient_history_end(df, patient_diagnoses):
    def count_before(row):
        patient_id = row["PATIENT_ID"]
        visit_date = row["VISIT_DATE"]
        dates = patient_diagnoses.get(patient_id)["dates"]
        if dates is None or len(dates) == 0:
            return 0
        visit_date = np.datetime64(visit_date)
        return np.searchsorted(dates, visit_date, side="left")

    return df.apply(count_before, axis=1)


def generate_history(df, events_file, events_dtypes, current_date):
    patient_diagnoses = {}

    for chunk in pd.read_csv(events_file, dtype=events_dtypes, usecols=events_dtypes.keys(), chunksize=CHUNK_SIZE):
        for _, row in chunk.iterrows():
            patient_id = row["PATIENT_ID"]
            visit_date = row["VISIT_DATE"]
            diagnosis = row["ICD10_CODE"]
            patient_diagnoses.setdefault(patient_id, []).append((visit_date, diagnosis))

    for patient_id, diagnoses in patient_diagnoses.items():
        diagnoses.sort(key=lambda x: x[0])
        dates, codes = zip(*diagnoses)
        patient_diagnoses[patient_id] = {"dates": dates, "codes": codes}

    df["HISTORY_SIZE"] = df.apply(get_patient_history_end, axis=1, args=(patient_diagnoses,))

    with open(f"patient_diagnoses_{current_date}.json", "w") as f:
        json.dump(patient_diagnoses, f)


if __name__ == "__main__":
    start_time = time.time()
    current_date = datetime.now().strftime("%Y-%m-%d")

    df_file = find_latest_file_by_date(INDIR, r"J069DiagnosesWithPrescriptionsAll_(\d{8}).csv")
    df = pd.read_csv(df_file)
    events_file = find_latest_file_by_date(INDIR, r"AllConnectedDiagnoses_(\d{8}).csv")

    generate_history(df, events_file, DIAGNOSIS_DTYPES, current_date)

    print(f"Time taken: {format_seconds_to_hms(time.time() - start_time)}")
