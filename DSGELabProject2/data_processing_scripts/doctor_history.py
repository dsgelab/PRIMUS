import pandas as pd
from utils import find_latest_file_by_date, format_seconds_to_hms
from pathlib import Path
from datetime import datetime
import time

INDIR = Path("/media/volume/Projects/mikael/ProcessedData/")
OUTDIR = Path("/media/volume/Projects/mikael/ProcessedData/")

current_date = datetime.now().strftime("%Y%m%d")
DIAGNOSIS_DTYPES = {"PATIENT_ID": "str", "VISIT_DATE": "str", "ICD10_CODE": "str", "DOCTOR_ID": "str"}
PRESCRIPTION_DTYPES = {
    "PATIENT_ID": "str",
    "CODE": "str",
    "PRESCRIPTION_DATE": "str",
}


def save_history(events_filename, events_dtypes, code_column, group_column, date_column, history_column_prefix, outfile_label, diagnosis):
    print(f"Generating {outfile_label} history...")
    events_file = find_latest_file_by_date(INDIR, events_filename)
    events = pd.read_csv(events_file, dtype=events_dtypes, usecols=events_dtypes.keys())
    events = events[events[code_column].str.match("^[A-Z]", na=False)]  # Clean up improper codes
    events[group_column] = events[code_column].str[0]
    print(f"{outfile_label} events loaded (N={len(events)}).")

    code_groups = events[group_column].unique()

    def get_earliest_events(events, diagnosis, code_groups, group_column, date_column):
        doctor_ids = diagnosis["DOCTOR_ID"].unique()
        full_index = pd.MultiIndex.from_product([doctor_ids, code_groups], names=["PATIENT_ID", group_column])
        earliest_events = (
            events.groupby(["PATIENT_ID", group_column])
            .agg(EARLIEST_EVENT_DATE=(date_column, "min"))
            .reset_index()
            .set_index(["PATIENT_ID", group_column])
            .reindex(full_index, fill_value=pd.NA)
            .reset_index()
            .rename(columns={"PATIENT_ID": "DOCTOR_ID"})
            .sort_values(["DOCTOR_ID", group_column])
        )
        return earliest_events

    earliest_events = get_earliest_events(events, diagnosis, code_groups, group_column, date_column)
    print("Earliest events generated.")

    def add_history_columns(earliest_events, diagnosis, code_groups, group_column, history_column_prefix):
        doctor_event_history = (
            diagnosis[["DOCTOR_ID", "VISIT_DATE"]].drop_duplicates().sort_values(["DOCTOR_ID", "VISIT_DATE"]).reset_index(drop=True)
        )

        for group in code_groups:
            group_history = earliest_events[earliest_events[group_column] == group][["DOCTOR_ID", "EARLIEST_EVENT_DATE"]]
            merged = pd.merge(
                doctor_event_history,
                group_history,
                on="DOCTOR_ID",
                how="inner",
            )
            doctor_event_history[f"{history_column_prefix}_{group}"] = (merged["EARLIEST_EVENT_DATE"] < merged["VISIT_DATE"]).astype(int)
        return doctor_event_history

    doctor_event_history = add_history_columns(earliest_events, diagnosis, code_groups, group_column, history_column_prefix)
    print("History columns added.")

    doctor_event_history.to_csv(OUTDIR / f"J069Doctor{outfile_label}History_{current_date}.csv", index=False)
    print(f"{outfile_label} history saved.")


if __name__ == "__main__":
    start_time = time.time()

    diagnosis_file = find_latest_file_by_date(INDIR, r"FirstConnectedJ069Diagnoses_(\d{8}).csv")  # First J06.9 diagnosis for each patient
    diagnosis = pd.read_csv(diagnosis_file, dtype=DIAGNOSIS_DTYPES, usecols=DIAGNOSIS_DTYPES.keys())
    diagnosis = diagnosis[diagnosis["DOCTOR_ID"].notna()]
    print(f"J069 patients loaded (N={len(diagnosis)}).")

    save_history(r"AllConnectedDiagnoses_(\d{8}).csv", DIAGNOSIS_DTYPES, "ICD10_CODE", "ICD10_GROUP", "VISIT_DATE", "HAD", "Diagnosis", diagnosis)
    save_history(
        r"imputed_prescriptions_(\d{14}).csv", PRESCRIPTION_DTYPES, "CODE", "CODE_GROUP", "PRESCRIPTION_DATE", "GOT", "Prescription", diagnosis
    )

    print(f"Script finished in {format_seconds_to_hms(time.time() - start_time)}.")
