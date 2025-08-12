# This script helps test that the history features work as expected. For each history (patient/doctor, diagnosis/prescription history),
# it selects one patient with some history (any column with a 1) and stores all rows for that patient in a test file. These test files can be used to
# check that the history columns are correct.

from utils import find_latest_file_by_date
import pandas as pd
from pathlib import Path

code = "J06.9"
code_no_dot = code.replace(".", "")
outdir = "ProcessedData"

all_diagnoses_file = find_latest_file_by_date(outdir, r"AllConnectedDiagnoses_(\d{8})\.csv")
all_prescriptions_file = Path("/media/volume/Projects/DSGELabProject1/ProcessedData/AllConnectedPrescriptions_20250506.csv")
diagnosis_file = find_latest_file_by_date(outdir, rf"FirstConnected{code_no_dot}Diagnoses_(\d{{8}})\.csv")
diag_history_pat_file = find_latest_file_by_date(outdir, rf"{code_no_dot}PatientDiagnosisHistory_(\d{{8}})\.csv")
pres_history_pat_file = find_latest_file_by_date(outdir, rf"{code_no_dot}PatientPrescriptionHistory_(\d{{8}})\.csv")
diag_history_doc_file = find_latest_file_by_date(outdir, rf"{code_no_dot}DoctorDiagnosisHistory_(\d{{8}})\.csv")
pres_history_doc_file = find_latest_file_by_date(outdir, rf"{code_no_dot}DoctorPrescriptionHistory_(\d{{8}})\.csv")


class File:
    def __init__(self, person, date_column, path):
        self.person = person
        self.date_column = date_column
        self.path = path
        self.id = None
        self.df = None
        self.mean_df = None


def get_test_filename(path: Path, mean: bool = False):
    return path.parent / f"Test{'Mean' if mean else ''}{path.name}"


def test():

    def generate_test_history(files, all_file):
        for file in files:
            history = pd.read_csv(file.path)
            print(f"Processing file {file.path.name}")
            patient = history[history.filter(regex=r"(PAT|DOC)$").sum(axis=1) > 0].iloc[0]
            id_cols = ["PATIENT_ID"] if file.person == "patient" else ["DOCTOR_ID", "VISIT_DATE"]
            id_values = patient[id_cols].values
            file.id = id_values

        print(f"Reading all data from {all_file.name}")
        for chunk in pd.read_csv(all_file, chunksize=20_000_000):
            for file in files:

                if file.person == "patient":
                    patient_id = file.id[0]
                    mask = chunk["PATIENT_ID"] == patient_id
                else:
                    doctor_id, date = file.id
                    mask = (chunk["PATIENT_ID"] == doctor_id) & (chunk[file.date_column] < date)

                    mean_mask = chunk["DOCTOR_ID"] == doctor_id
                    file.mean_df = pd.concat([file.mean_df, chunk[mean_mask]], ignore_index=True)

                patient_chunk = chunk[mask]
                file.df = pd.concat([file.df, patient_chunk], ignore_index=True)

        for file in files:
            test_file = get_test_filename(file.path)
            file.df.to_csv(test_file, index=False)
            print(f"Test file saved to {test_file}")
            if file.mean_df is not None:
                test_file = get_test_filename(file.path, mean=True)
                file.mean_df.to_csv(test_file, index=False)
                print(f"Mean test file saved to {test_file}")

    diag_history_pat = File(person="patient", date_column="VISIT_DATE", path=diag_history_pat_file)
    diag_history_doc = File(person="doctor", date_column="VISIT_DATE", path=diag_history_doc_file)
    pres_history_pat = File(person="patient", date_column="PRESCRIPTION_DATE", path=pres_history_pat_file)
    pres_history_doc = File(person="doctor", date_column="PRESCRIPTION_DATE", path=pres_history_doc_file)

    generate_test_history((diag_history_pat, diag_history_doc), all_diagnoses_file)
    generate_test_history((pres_history_pat, pres_history_doc), all_prescriptions_file)


if __name__ == "__main__":
    test()
