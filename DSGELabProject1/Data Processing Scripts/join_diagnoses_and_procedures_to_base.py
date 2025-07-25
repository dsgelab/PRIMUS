# Info
# This script joins diagnosis and procedure files to the respective base files (processed_{hilmo|avohilmo}_visits_YYYYMMDD.csv) and saves them to two
# combined csv files (connected diagnoses and connected procedures). Pipeline diagram in doc/AllConnectedPipeline.png. The script takes roughly 23
# hours to run. Assumes that you have already run the join_doctors_to_base.py script that produces the base files.


# Libraries
import os
import pandas as pd
import time
import re
from pathlib import Path
from utils import find_latest_file_by_date, format_seconds_to_hms, count_csv_rows

INDIR = Path("/media/volume/Projects/mikael/ProcessedData/")
OUTDIR = Path("/media/volume/Projects/mikael/ProcessedData/")
AVOHILMO_DIAGNOSIS_FOLDER = Path("/media/volume/Data/Data_THL_2698_14.02.00_2023_pk1_kayntisyy_folk/THL/")
HILMO_DIAGNOSIS_FOLDER = Path("/media/volume/Data/Data_THL_2698_14.02.00_2023/THL/")
AVOHILMO_PROCEDURE_FOLDER = Path("/media/volume/Data/Data_THL_2698_14.02.00_2023/THL/")
HILMO_PROCEDURE_FOLDER = Path("/media/volume/Data/Data_THL_2698_14.02.00_2023/THL/")
CHUNK_SIZE = 50_000_000

current_date = time.strftime("%Y%m%d")
ENCODING = "latin-1"


class BaseDataset:
    """
    Handles the base files (doctor visits) for both Avohilmo and Hilmo.
    """

    def __init__(self, label: str, id_column: str):
        self.label = label
        self.id_column = id_column

    @property
    def file_path(self):
        return INDIR / find_latest_file_by_date(INDIR, rf"processed_{self.label.lower()}_visits_(\d{{8}}).csv")

    @property
    def dtypes(self):
        return {
            "PATIENT_ID": "str",
            self.id_column: "int",
            "VISIT_DATE": "str",
            "FD_HASH_CODE": "str",
            "DOCTOR_ID": "str",
        }


class OutputFile:
    """
    Represents a file to be produced by this script, for example, AllConnectedDiagnoses_YYYYMMDD.csv
    """

    def __init__(self, label: str):
        self.label = label

    @property
    def path(self):
        return OUTDIR / f"AllConnected{self.label}_{current_date}.csv"


class JoinedDataset:
    """
    Contains common functionality for joined datasets. Not used directly.
    """

    def __init__(self, folder: Path, file_regex: str):
        self.folder = folder
        self.file_regex = file_regex

    @property
    def file_paths(self):
        return tuple(self.folder / file for file in os.listdir(self.folder) if re.search(self.file_regex, file) and "~lock" not in file)


class ProcedureDataset(JoinedDataset):
    """
    Handles the procedure files for both Avohilmo and Hilmo.
    """

    def __init__(self, folder: Path, file_regex: str, id_column: str, procedure_column: str, order_column: str):
        super().__init__(folder, file_regex)
        self.id_column = id_column
        self.procedure_column = procedure_column
        self.order_column = order_column
        self.new_code_column = "NOMESCO_CODE"
        self.output_columns = [self.new_code_column]

    def preprocess(self, file_path):
        """
        Preprocesses a single procedure file (for example, FD2698_THL2023_H_TOIMENP_1.csv) and returns a dataframe.
        """
        dtypes = {
            "FID": "str",
            self.id_column: "int",
            self.order_column: "int",
            self.procedure_column: "str",
        }
        df = pd.read_csv(file_path, sep=";", encoding=ENCODING, usecols=dtypes.keys(), dtype=dtypes)
        df = df[df[self.procedure_column].str.match(r"^[A-Z0-9]{5}$", na=False)]  # Filter nomesco codes
        df = df[df[self.order_column] == 0]  # Filter only main procedure
        df = df.rename(columns={self.procedure_column: self.new_code_column})
        return df


class DiagnosisDataset(JoinedDataset):
    """
    Handles the diagnosis files for both Avohilmo and Hilmo.
    """

    def __init__(self, folder: Path, file_regex: str, id_column: str, order_column: str, code_column: str, class_column: str, class_value: str):
        super().__init__(folder, file_regex)
        self.id_column = id_column
        self.order_column = order_column
        self.code_column = code_column
        self.class_column = class_column
        self.class_value = class_value
        self.new_code_column = "ICD10_CODE"
        self.new_code_column2 = f"{self.new_code_column}_2ND"
        self.output_columns = [self.new_code_column, self.new_code_column2]

    def preprocess(self, file_path):
        """
        Preprocesses a single diagnosis file (for example, FD_2698_THL2023_2698_H_ICD10_1.csv) and returns a dataframe.
        Groups by self.id_column, producing two columns for primary and secondary ICD10 codes. Only keeps groups where primary diagnosis exists.
        """
        dtypes = {
            "FID": "str",
            self.id_column: "int",
            self.order_column: "int",
            self.code_column: "str",
            self.class_column: "str",
        }
        df = pd.read_csv(file_path, sep=";", encoding=ENCODING, usecols=dtypes.keys(), dtype=dtypes)
        df = df[df[self.class_column] == self.class_value]
        df = df[df[self.order_column].isin([0, 1])]
        df_pivot = df.pivot_table(index=self.id_column, columns=self.order_column, values=self.code_column, aggfunc="first").reset_index()
        df_pivot = df_pivot.rename(columns={0: self.new_code_column, 1: self.new_code_column2})
        df_pivot = df_pivot[df_pivot[self.new_code_column].notnull()]
        if self.new_code_column2 not in df_pivot.columns:
            df_pivot[self.new_code_column2] = pd.NA  # If the secondary diagnosis column is missing after pivot, add it with NaNs
        return df_pivot


def DatasetFactory(output_file, base_dataset):
    """
    Uses factory pattern to select the correct dataset according to the base dataset (Hilmo or Avohilmo)
    and output file (diagnoses or procedures).
    """
    registry = {
        ("Diagnoses", "Avohilmo"): DiagnosisDataset(
            folder=AVOHILMO_DIAGNOSIS_FOLDER,
            file_regex=r"AH_KAYNTISYY_\d",
            id_column="AVOHILMOID",
            order_column="JARJESTYS",
            code_column="LUOKITUS",
            class_column="LUOKITUSLUOKKA",
            class_value="icd10",
        ),
        ("Diagnoses", "Hilmo"): DiagnosisDataset(
            folder=HILMO_DIAGNOSIS_FOLDER,
            file_regex=r"H_ICD10_\d",
            id_column="HILMOID",
            order_column="N",
            code_column="KOODI",
            class_column="KENTTA",
            class_value="PDGO",
        ),
        ("Procedures", "Avohilmo"): ProcedureDataset(
            folder=AVOHILMO_PROCEDURE_FOLDER,
            file_regex=r"AH_TOIMENPITEET_\d",
            id_column="AVOHILMOID",
            procedure_column="TOIMENPIDE",
            order_column="JARJESTYS",
        ),
        ("Procedures", "Hilmo"): ProcedureDataset(
            folder=HILMO_PROCEDURE_FOLDER,
            file_regex=r"H_TOIMENP_\d",
            id_column="HILMOID",
            procedure_column="TOIMP",
            order_column="N",
        ),
    }
    return registry[(output_file.label, base_dataset.label)]


# Specifies the files to be produced by the script. Comment out the output files you don't need to update.
output_files = (OutputFile(label="Diagnoses"), OutputFile(label="Procedures"))
# Specifies the base datasets to be used by the script. Comment out the base datasets you don't need to include.
base_datasets = (
    BaseDataset(label="Avohilmo", id_column="AVOHILMOID"),
    BaseDataset(label="Hilmo", id_column="HILMOID"),
)


if __name__ == "__main__":
    script_start = time.time()

    # LOOPS
    # procedure, diagnosis
    # ..hilmo, avohilmo
    # ....chunks of base
    # ......chunks of diagnosis/procedure
    for output_file in output_files:
        output_file.path.unlink(missing_ok=True)  # Remove old file
        for base_dataset in base_datasets:
            total_rows = count_csv_rows(base_dataset.file_path)
            print(f"\n=== Processing file {base_dataset.file_path.name} ({total_rows} rows). Making file {output_file.path.name}. ===")
            for base_chunk in pd.read_csv(
                base_dataset.file_path,
                chunksize=CHUNK_SIZE,
                sep=",",
                encoding=ENCODING,
                usecols=base_dataset.dtypes.keys(),
                dtype=base_dataset.dtypes,
            ):  # The base dataset (processed_{hilmo|avohilmo}_visits_YYYYMMDD.csv) may contain 1,000,000,000 rows, so we read it in chunks
                base_chunk["SOURCE"] = base_dataset.label
                joined_dataset = DatasetFactory(output_file, base_dataset)  # Dataset to be joined to the base dataset
                # Loop over all joined datasets for a particular output file (diagnoses or procedures) and base dataset (Avohilmo or Hilmo),
                # for example, FD2698_THL2023_H_TOIMENP_1.csv, FD2698_THL2023_H_TOIMENP_2.csv etc.
                for file_path in joined_dataset.file_paths:
                    joined_chunk = joined_dataset.preprocess(file_path)
                    df = pd.merge(base_chunk, joined_chunk, on=base_dataset.id_column, how="inner")
                    df = df[["PATIENT_ID", "VISIT_DATE", "SOURCE", "FD_HASH_CODE", "DOCTOR_ID", *joined_dataset.output_columns]]
                    df.to_csv(output_file.path, mode="a", header=not output_file.path.exists(), encoding=ENCODING, index=False)
                    rows = f"{base_chunk.index[0] + 1}-{base_chunk.index[-1] + 1} of {total_rows}"
                    print(f"Joined {file_path.name} to {base_dataset.label} base rows {rows} and saved to {output_file.path.name}")

    print(f"\nFinished processing all files in {format_seconds_to_hms(time.time() - script_start)}.")
