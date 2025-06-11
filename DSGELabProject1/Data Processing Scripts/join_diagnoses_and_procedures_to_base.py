# Info
# This script joins diagnosis and procedure files to the respective base files and saves them to two combined csv files
# (connected diagnoses and connected procedures). Pipeline diagram in doc/AllConnectedPipeline.png.  The script takes roughly 9,5 hours to run.

# Libraries
import os
import pandas as pd
import time
import re
from pathlib import Path
from typing import NamedTuple
from utils import find_latest_file_by_date, format_seconds_to_hms, count_csv_rows

indir = Path("/media/volume/Projects/mikael/ProcessedData/")
outdir = Path("/media/volume/Projects/mikael/ProcessedData/")
avohilmo_diagnosis_path = Path("/media/volume/Data/Data_THL_2698_14.02.00_2023_pk1_kayntisyy_folk/THL/")
THL_path = Path("/media/volume/Data/Data_THL_2698_14.02.00_2023/THL/")

current_date = time.strftime("%Y%m%d")
CHUNK_SIZE = 50_000_000

DTYPES_HILMO_BASE = {"PATIENT_ID": "str", "HILMOID": "int", "VISIT_DATE": "str", "FD_HASH_CODE": "str", "DOCTOR_ID": "str"}
DTYPES_AVOHILMO_BASE = {"PATIENT_ID": "str", "AVOHILMOID": "int", "VISIT_DATE": "str", "FD_HASH_CODE": "str", "DOCTOR_ID": "str"}
DTYPES_AVOHILMO_PROCEDURES = {
    "FID": "str",
    "AVOHILMOID": "int",
    "JARJESTYS": "int",
    "TOIMENPIDE": "str",
}
DTYPES_HILMO_PROCEDURES = {
    "FID": "str",
    "HILMOID": "int",
    "TOIMPALKUPVM": "str",
    "N": "int",
    "TOIMP": "str",
}
DTYPES_AVOHILMO_DIAGNOSES = {
    "FID": "str",
    "AVOHILMOID": "int",
    "JARJESTYS": "int",
    "LUOKITUS": "str",
    "LUOKITUSLUOKKA": "str",
}
DTYPES_HILMO_DIAGNOSES = {
    "FID": "str",
    "HILMOID": "int",
    "KOODI": "str",
    "N": "int",
    "KENTTA": "str",
}


class BaseDataset(NamedTuple):
    """
    Handles the base files (doctor visits) for both Avohilmo and Hilmo.
    """

    label: str
    dtypes: dict[str, str]
    id_column: str

    @property
    def file_path(self):
        return indir / find_latest_file_by_date(indir, rf"processed_{self.label.lower()}_visits_(\d{{8}}).csv")


class OutputFile(NamedTuple):
    """
    Represents a file to be produced by this script, for example, AllConnectedDiagnoses_YYYYMMDD.csv
    """

    label: str

    @property
    def path(self):
        return outdir / f"AllConnected{self.label}_{current_date}.csv"


class ProcedureDataset(NamedTuple):
    """
    Handles the procedure files for both Avohilmo and Hilmo.
    """

    file_regex: str
    dtypes: dict[str, str]
    procedure_column: str
    new_code_column = "NOMESCO_CODE"
    order_column: str

    @property
    def file_paths(self):
        return tuple(THL_path / file for file in os.listdir(THL_path) if re.search(self.file_regex, file) and "~lock" not in file)

    def preprocess(self, file_path):
        """
        Preprocesses a single procedure file (for example, FD2698_THL2023_H_TOIMENP_1.csv) and returns a dataframe.
        """
        df = pd.read_csv(file_path, sep=";", encoding="latin1", usecols=self.dtypes.keys(), dtype=self.dtypes)
        df = df[df[self.procedure_column].str.match(r"^[A-Z0-9]{5}$", na=False)]  # Filter nomesco codes
        df = df[df[self.order_column] == 0]  # Filter only main procedure
        df = df.rename(columns={self.procedure_column: self.new_code_column})
        return df


class DiagnosisDataset(NamedTuple):
    """
    Handles the diagnosis files for both Avohilmo and Hilmo.
    """

    file_paths: tuple[Path]
    dtypes: dict[str, str]
    order_column: str
    code_column: str
    new_code_column = "ICD10_CODE"
    class_column: str
    class_value: str

    def preprocess(self, file_path):
        """
        Preprocesses a single diagnosis file (for example, FD_2698_THL2023_2698_H_ICD10_1.csv) and returns a dataframe.
        """
        df = pd.read_csv(file_path, sep=";", encoding="latin-1", usecols=self.dtypes.keys(), dtype=self.dtypes)
        # filter only first and main diagnosis
        df = df[(df[self.order_column] == 0) & (df[self.class_column] == self.class_value)]
        df = df.rename(columns={self.code_column: self.new_code_column})
        return df


def DatasetFactory(output_file, base_dataset):
    """
    Uses factory pattern to select the correct dataset according to the base dataset (Hilmo or Avohilmo)
    and output file (diagnoses or procedures).
    """
    registry = {
        ("Diagnoses", "Avohilmo"): DiagnosisDataset(
            file_paths=[avohilmo_diagnosis_path / file for file in os.listdir(avohilmo_diagnosis_path) if "~lock" not in file],
            dtypes=DTYPES_AVOHILMO_DIAGNOSES,
            order_column="JARJESTYS",
            code_column="LUOKITUS",
            class_column="LUOKITUSLUOKKA",
            class_value="icd10",
        ),
        ("Diagnoses", "Hilmo"): DiagnosisDataset(
            file_paths=[THL_path / file for file in os.listdir(THL_path) if re.search(r"H_ICD10_\d", file) and "~lock" not in file],
            dtypes=DTYPES_HILMO_DIAGNOSES,
            order_column="N",
            code_column="KOODI",
            class_column="KENTTA",
            class_value="PDGO",
        ),
        ("Procedures", "Avohilmo"): ProcedureDataset(
            file_regex=r"AH_TOIMENPITEET_\d",
            dtypes=DTYPES_AVOHILMO_PROCEDURES,
            procedure_column="TOIMENPIDE",
            order_column="JARJESTYS",
        ),
        ("Procedures", "Hilmo"): ProcedureDataset(
            file_regex=r"H_TOIMENP_\d",
            dtypes=DTYPES_HILMO_PROCEDURES,
            procedure_column="TOIMP",
            order_column="N",
        ),
    }
    return registry[(output_file.label, base_dataset.label)]


# Specifies the files to be produced by the script. Comment out the output files you don't need to update.
output_files = (OutputFile(label="Diagnoses"), OutputFile(label="Procedures"))
# Specifies the base datasets to be used by the script. Comment out the base datasets you don't need to include.
base_datasets = (
    BaseDataset(label="Avohilmo", dtypes=DTYPES_AVOHILMO_BASE, id_column="AVOHILMOID"),
    BaseDataset(label="Hilmo", dtypes=DTYPES_HILMO_BASE, id_column="HILMOID"),
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
                encoding="latin-1",
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
                    df = df[["PATIENT_ID", "VISIT_DATE", joined_dataset.new_code_column, "SOURCE", "FD_HASH_CODE", "DOCTOR_ID"]]
                    df.to_csv(output_file.path, mode="a", header=not output_file.path.exists(), encoding="latin-1", index=False)
                    rows = f"{base_chunk.index[0]+1}-{base_chunk.index[-1]+1} of {total_rows}"
                    print(f"Joined {file_path.name} to {base_dataset.label} base rows {rows} and saved to {output_file.path.name}")

    print(f"\nFinished processing all files in {format_seconds_to_hms(time.time() - script_start)}.")
