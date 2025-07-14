import pandas as pd
from utils import find_latest_file_by_date, format_seconds_to_hms
from pathlib import Path
from datetime import datetime
import time
import numpy as np
import sys

INDIR = Path("/media/volume/Projects/mikael/ProcessedData/")
OUTDIR = Path("/media/volume/Projects/mikael/ProcessedData/")
CHUNK_SIZE = 20_000_000

current_date = datetime.now().strftime("%Y%m%d")
DIAGNOSIS_DTYPES = {"PATIENT_ID": "str", "VISIT_DATE": "str", "ICD10_CODE": "str", "DOCTOR_ID": "str"}
PRESCRIPTION_DTYPES = {
    "PATIENT_ID": "str",
    "DOCTOR_ID": "str",
    "CODE": "str",
    "PRESCRIPTION_DATE": "str",
}


def get_size_gb(obj, name):
    def get_size(obj, seen=None):
        """Recursively finds size of objects in bytes."""
        size = sys.getsizeof(obj)
        if seen is None:
            seen = set()
        obj_id = id(obj)
        if obj_id in seen:
            return 0
        seen.add(obj_id)
        if isinstance(obj, dict):
            size += sum([get_size(v, seen) for v in obj.values()])
            size += sum([get_size(k, seen) for k in obj.keys()])
        elif isinstance(obj, (list, tuple, set)):
            size += sum([get_size(i, seen) for i in obj])
        return size

    size_gb = get_size(obj) / (1024**3)
    print(f"Estimated size of {name}: {size_gb:.3f} GB")
    return size_gb


def count_dates_before(doctor_event_history, doctor_dates):
    def count_before(row):
        doctor_id = row["DOCTOR_ID"]
        visit_date = row["VISIT_DATE"]
        dates = doctor_dates.get(doctor_id)
        if dates is None or len(dates) == 0:
            return 0
        visit_date = np.datetime64(visit_date)
        return np.searchsorted(dates, visit_date, side="left")

    return doctor_event_history.apply(count_before, axis=1)


def add_mean_yearly_past_prescriptions_column(events_file, events_dtypes, date_column, doctor_event_history, diagnosis):
    doctor_presc_dates = {}

    for chunk in pd.read_csv(events_file, dtype=events_dtypes, usecols=events_dtypes.keys(), chunksize=CHUNK_SIZE):
        # Accumulate all prescription dates per doctor. These are used to calculate the mean yearly past prescriptions.
        doctor_date_groups = chunk.groupby("DOCTOR_ID")[date_column]
        for doctor_id, group in doctor_date_groups:
            doctor_presc_dates.setdefault(doctor_id, []).extend(group.tolist())

    get_size_gb(doctor_presc_dates, "doctor_presc_dates")

    diagnosis["VISIT_DATE"] = pd.to_datetime(diagnosis["VISIT_DATE"])
    doctor_event_history["VISIT_DATE"] = pd.to_datetime(doctor_event_history["VISIT_DATE"])
    # Convert accumulated prescription dates to sorted numpy arrays
    for doctor_id in doctor_presc_dates:
        doctor_presc_dates[doctor_id] = np.sort(np.array(doctor_presc_dates[doctor_id], dtype="datetime64[ns]"))
    doctor_earliest_presc = {doctor_id: doctor_presc_dates[doctor_id][0] for doctor_id in doctor_presc_dates}
    doctor_event_history["EARLIEST_PRESC_DATE"] = doctor_event_history["DOCTOR_ID"].map(doctor_earliest_presc)

    doctor_event_history["N_PRESCRIPTIONS_BEFORE"] = count_dates_before(doctor_event_history, doctor_presc_dates)
    mask_valid = doctor_event_history["EARLIEST_PRESC_DATE"].notna() & (
        doctor_event_history["VISIT_DATE"] > doctor_event_history["EARLIEST_PRESC_DATE"]
    )
    years = (doctor_event_history["VISIT_DATE"] - doctor_event_history["EARLIEST_PRESC_DATE"]).dt.days / 365.25
    doctor_event_history["MEAN_YEARLY_PRESCRIPTIONS"] = pd.NA
    doctor_event_history.loc[mask_valid, "MEAN_YEARLY_PRESCRIPTIONS"] = (
        doctor_event_history.loc[mask_valid, "N_PRESCRIPTIONS_BEFORE"] / years[mask_valid]
    ).round(2)
    doctor_event_history.drop(columns=["N_PRESCRIPTIONS_BEFORE", "EARLIEST_PRESC_DATE"], inplace=True)


def save_history(events_filename, events_dtypes, code_column, group_column, date_column, history_column_prefix, outfile_label, diagnosis):
    print(f"Generating {outfile_label} history...")
    events_file = find_latest_file_by_date(INDIR, events_filename)

    earliest_events_dict = {}
    code_groups_set = set()

    # Very large file, so read in chunks
    for chunk in pd.read_csv(events_file, dtype=events_dtypes, usecols=events_dtypes.keys(), chunksize=CHUNK_SIZE):
        chunk = chunk[chunk[code_column].str.match("^[A-Z]", na=False)]  # Clean up improper codes
        chunk[date_column] = pd.to_datetime(chunk[date_column])
        chunk[group_column] = chunk[code_column].str[0]
        code_groups_set.update(chunk[group_column].unique())

        # Update earliest event date per patient and group
        chunk_earliest_group_dates = chunk.groupby(["PATIENT_ID", group_column])[date_column].min().reset_index()
        for _, row in chunk_earliest_group_dates.iterrows():
            key = (row["PATIENT_ID"], row[group_column])
            date_val = row[date_column]
            if key not in earliest_events_dict or pd.to_datetime(date_val) < pd.to_datetime(earliest_events_dict[key]):
                earliest_events_dict[key] = date_val

    get_size_gb(earliest_events_dict, "earliest_events_dict")

    code_groups = sorted(code_groups_set)
    earliest_events_df = pd.DataFrame([{"PATIENT_ID": k[0], group_column: k[1], "EARLIEST_EVENT_DATE": v} for k, v in earliest_events_dict.items()])

    doctor_ids = diagnosis["DOCTOR_ID"].unique()
    full_index = pd.MultiIndex.from_product([doctor_ids, code_groups], names=["PATIENT_ID", group_column])
    earliest_events = (
        earliest_events_df.set_index(["PATIENT_ID", group_column])
        .reindex(full_index, fill_value=pd.NA)
        .reset_index()
        .rename(columns={"PATIENT_ID": "DOCTOR_ID"})
        .sort_values(["DOCTOR_ID", group_column])
    )
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

    if outfile_label == "Prescription":
        add_mean_yearly_past_prescriptions_column(events_file, events_dtypes, date_column, doctor_event_history, diagnosis)
        print("Mean yearly past prescriptions column added.")

    outfile = OUTDIR / f"J069Doctor{outfile_label}History_{current_date}.csv"
    doctor_event_history.to_csv(outfile, index=False)
    print(f"{outfile_label} history saved to {outfile.name}.")


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
