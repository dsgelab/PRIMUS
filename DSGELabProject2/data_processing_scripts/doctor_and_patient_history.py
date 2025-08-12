import pandas as pd
from utils import find_latest_file_by_date, format_seconds_to_hms
from pathlib import Path
from datetime import datetime
import time
import numpy as np
import sys
import os
import argparse
from enum import StrEnum

INDIR = Path("/media/volume/Projects/mikael/ProcessedData/")
OUTDIR = Path("/media/volume/Projects/mikael/ProcessedData/")
CHUNK_SIZE = 20_000_000
current_date = datetime.now().strftime("%Y%m%d")
DIAGNOSIS_DTYPES = {"PATIENT_ID": "str", "VISIT_DATE": "str", "ICD10_CODE": "str", "DOCTOR_ID": "str"}
PRESCRIPTION_DTYPES = {
    "PATIENT_ID": "str",
    "DOCTOR_ID": "str",
    "ATC_CODE": "str",
    "PRESCRIPTION_DATE": "str",
}


class GroupingMethod(StrEnum):
    FIRST_DIGIT = "first-letter"
    THREE_DIGITS = "three-digits"

    @classmethod
    def values(cls):
        return [member.value for member in cls]


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


def add_mean_past_yearly_column(events_file, events_dtypes, date_column, doctor_event_history, label):
    """
    Calculate and add the mean yearly number of past events (prescriptions or diagnoses) for each doctor.

    For each doctor in the provided event history DataFrame, this function:
      - Aggregates all past event dates (prescriptions or diagnoses) from a large CSV file, reading in chunks for efficiency.
      - For each doctor, determines the earliest event date and counts the number of events that occurred before each event in the history.
      - Computes the mean yearly rate of past events for each doctor up to the current event date, and adds this as a new column to the DataFrame.

    Args:
        events_file (str or Path): Path to the CSV file containing all events (prescriptions or diagnoses).
        events_dtypes (dict): Dictionary specifying dtypes for reading the CSV.
        date_column (str): Name of the column containing event dates.
        doctor_event_history (pd.DataFrame): All doctor diagnosis events (visit date and doctor id) for unique patients within the selected diagnosis.
        label (str): Either "Diagnosis" or "Prescription", used for naming the new column.

    Returns:
        None. Modifies doctor_event_history in place by adding a column 'MEAN_YEARLY_<EVENTS>'.
    """
    plurals = {"Diagnosis": "diagnoses", "Prescription": "prescriptions"}
    doctor_past_dates = {}  # All past prescription or diagnosis dates per doctor

    for chunk in pd.read_csv(events_file, dtype=events_dtypes, usecols=events_dtypes.keys(), chunksize=CHUNK_SIZE):
        # Accumulate all prescription dates per doctor. These are used to calculate the mean yearly past prescriptions.
        doctor_date_groups = chunk.groupby("DOCTOR_ID")[date_column]
        for doctor_id, group in doctor_date_groups:
            doctor_past_dates.setdefault(doctor_id, []).extend(group.tolist())

    get_size_gb(doctor_past_dates, f"Doctor past {label} dates")

    # Convert accumulated dates to sorted numpy arrays
    for doctor_id in doctor_past_dates:
        doctor_past_dates[doctor_id] = np.sort(np.array(doctor_past_dates[doctor_id], dtype="datetime64[ns]"))
    doctor_earliest_date = {doctor_id: doctor_past_dates[doctor_id][0] for doctor_id in doctor_past_dates}
    doctor_event_history["EARLIEST_DATE"] = doctor_event_history["DOCTOR_ID"].map(doctor_earliest_date)

    doctor_event_history["N_EVENTS_BEFORE"] = count_dates_before(doctor_event_history, doctor_past_dates)
    mask_valid = doctor_event_history["EARLIEST_DATE"].notna() & (doctor_event_history["VISIT_DATE"] > doctor_event_history["EARLIEST_DATE"])
    years = (doctor_event_history["VISIT_DATE"] - doctor_event_history["EARLIEST_DATE"]).dt.days / 365.25
    yearly_column_name = f"MEAN_YEARLY_{plurals[label].upper()}"
    doctor_event_history[yearly_column_name] = pd.NA
    doctor_event_history.loc[mask_valid, yearly_column_name] = (doctor_event_history.loc[mask_valid, "N_EVENTS_BEFORE"] / years[mask_valid]).round(1)
    doctor_event_history.drop(columns=["N_EVENTS_BEFORE", "EARLIEST_DATE"], inplace=True)


def expand_code_ranges(code_ranges):
    """
    Convert a list of ICD-10 code ranges into a dictionary mapping each individual code to its range. Assumes that all
    ICD10 codes are valid.
    Args:
        code_ranges: List of strings in ICD10 group format, e.g., "C00-C02"
    Returns:
        Dictionary mapping each individual code to its original range
        e.g., {'C00': 'C00-C02', 'C01': 'C00-C02', 'C02': 'C00-C02'}
    """
    code_dict = {}

    for code_range in code_ranges:
        parts = code_range.split("-")
        if len(parts) != 2:
            raise ValueError(f"Invalid code range: {code_range}")
        start_code, end_code = parts

        if start_code[2].isdigit() and end_code[2].isdigit():  # For example, C64-C66

            letter_prefix = start_code[0]
            start_num = int(start_code[1:])
            end_num = int(end_code[1:])

            for num in range(start_num, end_num + 1):
                individual_code = f"{letter_prefix}{num:02d}"
                code_dict[individual_code] = code_range

        elif start_code[2].isalpha() and start_code == end_code:  # For example, C7A-C7A
            code_dict[start_code] = code_range
        elif start_code[2].isdigit() and end_code[2].isalpha():  # For example, I30-I5A
            code_dict[end_code] = code_range  # Add I5A
            # For I10-I1A, the range is 10-19. For I30-I5A, the range is 30-59.
            for num in range(int(start_code[1:]), int(str(end_code[1]) + "9") + 1):  # Add I30-I59
                individual_code = f"{start_code[0]}{num:02d}"
                code_dict[individual_code] = code_range

    return code_dict


def add_code_group(df, code_column, outfile_label, code_group_dict, grouping_method):
    if grouping_method == GroupingMethod.FIRST_DIGIT:
        df["CODE_GROUP"] = df[code_column].str[0]
    elif grouping_method == GroupingMethod.THREE_DIGITS:
        if outfile_label == "Prescription":
            df["CODE_GROUP"] = df[code_column].str[:3]
        elif outfile_label == "Diagnosis":
            df["CODE_GROUP"] = df[code_column].str[:3].map(code_group_dict)


def calculate_history(events_filename, events_dtypes, code_column, date_column, history_column_prefix, outfile_label, diagnosis):
    if os.path.isabs(events_filename) or os.path.exists(events_filename):
        events_file = events_filename
    else:
        events_file = find_latest_file_by_date(INDIR, events_filename)
    print(f"Generating {outfile_label} history from {events_file}.")

    doctor_diagnosis = diagnosis[diagnosis["DOCTOR_ID"].notna()]

    earliest_events_dict = {}
    code_groups_set = set()

    nrows = 0
    n_code_group_nans = 0

    code_group_dict = {}  # Not needed for prescriptions
    if outfile_label == "Diagnosis":
        with open(code_ranges_filename, "r") as f:
            code_ranges = f.read().splitlines()
        code_group_dict.update(expand_code_ranges(code_ranges))

    # Very large file, so read in chunks
    for chunk in pd.read_csv(events_file, dtype=events_dtypes, usecols=events_dtypes.keys(), chunksize=CHUNK_SIZE):
        chunk[date_column] = pd.to_datetime(chunk[date_column])
        add_code_group(chunk, code_column, outfile_label, code_group_dict, grouping_method)
        nrows += len(chunk)
        n_code_group_nans += chunk["CODE_GROUP"].isna().sum()
        code_groups_set.update(chunk["CODE_GROUP"].dropna().unique())
        # Update earliest event date per patient and group
        chunk_earliest_group_dates = chunk.groupby(["PATIENT_ID", "CODE_GROUP"])[date_column].min().reset_index()
        for _, row in chunk_earliest_group_dates.iterrows():
            key = (row["PATIENT_ID"], row["CODE_GROUP"])
            date_val = row[date_column]
            if key not in earliest_events_dict or pd.to_datetime(date_val) < pd.to_datetime(earliest_events_dict[key]):
                earliest_events_dict[key] = date_val

    get_size_gb(earliest_events_dict, "earliest_events_dict")

    print("Percentage of nans in code groups:", round(n_code_group_nans / nrows * 100, 1))
    code_groups = sorted(code_groups_set)
    earliest_events_df = pd.DataFrame([{"PATIENT_ID": k[0], "CODE_GROUP": k[1], "EARLIEST_EVENT_DATE": v} for k, v in earliest_events_dict.items()])

    def get_earliest_events_with_full_index(earliest_events_df, index_column):
        """
        If there are no events (diagnoses/prescriptions) for a patient, they are not included in the earliest_events_df.
        This function ensures that the earliest_events_df has a full index, even if there are no events for a patient. That is,
        it adds a row for each missing patient filled with NaNs as earliest event dates.
        """
        all_ids = set(diagnosis[index_column])
        full_index = pd.MultiIndex.from_product([all_ids, code_groups], names=["PATIENT_ID", "CODE_GROUP"])
        earliest_events = (
            earliest_events_df.set_index(["PATIENT_ID", "CODE_GROUP"])
            .reindex(full_index, fill_value=pd.NA)
            .reset_index()
            .rename(columns={"PATIENT_ID": index_column})
            .sort_values([index_column, "CODE_GROUP"])
        )
        return earliest_events

    def make_doctor_history_columns(earliest_events_df, doctor_diagnosis, code_groups, history_column_prefix):
        earliest_events = get_earliest_events_with_full_index(earliest_events_df, "DOCTOR_ID")
        doctor_event_history = (
            doctor_diagnosis[["DOCTOR_ID", "VISIT_DATE"]].drop_duplicates().sort_values(["DOCTOR_ID", "VISIT_DATE"]).reset_index(drop=True)
        )
        doctor_event_history["VISIT_DATE"] = pd.to_datetime(doctor_event_history["VISIT_DATE"])

        new_columns = {}
        for group in code_groups:
            group_history = earliest_events[earliest_events["CODE_GROUP"] == group][["DOCTOR_ID", "EARLIEST_EVENT_DATE"]]
            merged = pd.merge(
                doctor_event_history,
                group_history,
                on="DOCTOR_ID",
                how="inner",
            )
            new_columns[f"{history_column_prefix}_{group}_DOC"] = (merged["EARLIEST_EVENT_DATE"] < merged["VISIT_DATE"]).astype(int)

        doctor_event_history = pd.concat([doctor_event_history, pd.DataFrame(new_columns)], axis=1)
        return doctor_event_history

    def make_patient_history_columns(earliest_events_df, diagnosis, history_column_prefix):
        earliest_events = get_earliest_events_with_full_index(earliest_events_df, "PATIENT_ID")
        patient_ids = diagnosis["PATIENT_ID"]
        patient_event_history = (
            pd.merge(
                diagnosis,
                earliest_events,
                on="PATIENT_ID",
            )
            .query("EARLIEST_EVENT_DATE < VISIT_DATE")
            .assign(present=1)
            .pivot_table(
                index="PATIENT_ID",
                columns="CODE_GROUP",
                values="present",
                aggfunc="first",
                fill_value=0,
            )
            .add_prefix(f"{history_column_prefix}_")
            .add_suffix("_PAT")
            .reindex(patient_ids, fill_value=0)
            .reset_index()
            .rename_axis(None, axis=1)
        )
        return patient_event_history

    doctor_event_history = make_doctor_history_columns(earliest_events_df, doctor_diagnosis, code_groups, history_column_prefix)
    print("Doctor history columns added.")

    add_mean_past_yearly_column(events_file, events_dtypes, date_column, doctor_event_history, outfile_label)
    print(f"Mean yearly past {outfile_label.lower()} column added.")

    patient_event_history = make_patient_history_columns(earliest_events_df, diagnosis, history_column_prefix)
    print("Patient history columns added.")

    def save_history(history, target_group):
        outfile = OUTDIR / f"{icd10_code_no_dot}{target_group}{outfile_label}History_{current_date}.csv"
        history.to_csv(outfile, index=False)
        print(f"{outfile_label} history saved to {outfile.name}.")

    save_history(doctor_event_history, "Doctor")
    save_history(patient_event_history, "Patient")


if __name__ == "__main__":
    start_time = time.time()

    parser = argparse.ArgumentParser()
    parser.add_argument("--icd10_code", help="Default: J06.9.", type=str, default="J06.9")
    parser.add_argument(
        "--grouping_method",
        help="Grouping method for ICD10 codes. Default: three-digits.",
        type=str,
        choices=GroupingMethod.values(),
        default=GroupingMethod.THREE_DIGITS.value,
    )
    parser.add_argument(
        "--code_ranges_filename",
        help="Path to the file containing ranges for ICD10 codes separated by newlines. Default: icd10_code_ranges.txt",
        type=str,
        default="icd10_code_ranges.txt",
    )
    args = parser.parse_args()

    icd10_code = args.icd10_code
    icd10_code_no_dot = icd10_code.replace(".", "")
    code_ranges_filename = INDIR / args.code_ranges_filename
    grouping_method = args.grouping_method

    diagnosis_file = find_latest_file_by_date(INDIR, rf"FirstConnected{icd10_code_no_dot}Diagnoses_(\d{{8}}).csv")  # First diagnosis for each patient
    diagnosis = pd.read_csv(diagnosis_file, dtype=DIAGNOSIS_DTYPES, usecols=DIAGNOSIS_DTYPES.keys())
    diagnosis["VISIT_DATE"] = pd.to_datetime(diagnosis["VISIT_DATE"])
    print(f"{icd10_code} patients loaded (N={len(diagnosis)}) from {diagnosis_file.name}.")

    calculate_history(r"AllConnectedDiagnoses_(\d{8}).csv", DIAGNOSIS_DTYPES, "ICD10_CODE", "VISIT_DATE", "HAD_ICD10", "Diagnosis", diagnosis)
    calculate_history(
        "/media/volume/Projects/DSGELabProject1/ProcessedData/AllConnectedPrescriptions_20250506.csv",
        PRESCRIPTION_DTYPES,
        "ATC_CODE",
        "PRESCRIPTION_DATE",
        "GOT_ATC",
        "Prescription",
        diagnosis,
    )

    print(f"Script finished in {format_seconds_to_hms(time.time() - start_time)}.")
