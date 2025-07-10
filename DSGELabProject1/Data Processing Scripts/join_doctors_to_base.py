# Info:
# avohilmo base files = 14
# hilmo base files = 5
# doctor files = 1

# This script processes the THL avohilmo and hilmo files in order to add the doctor ids to the main THL files. The script takes
# roughly 2,5 hours to run. Pipeline diagram in doc/merged_data.png.

import time
import os
import re
import pandas as pd
from pathlib import Path
from typing import NamedTuple
from utils import format_seconds_to_hms

script_start = time.time()

THL_path = Path("/media/volume/Data/Data_THL_2698_14.02.00_2023/THL/")
Valvira_path = "/media/volume/Data/Data_THL_2698_14.02.00_2023/Valvira/FD_2698_Liite 1 THL_2698_14.02.00_2023.csv"
outdir = Path("/media/volume/Projects/mikael/ProcessedData")

current_date = time.strftime("%Y%m%d")
DTYPES_HILMO = {
    "FID": "str",
    "TUPVA": "str",
    "HILMOID": "int",
    "FD_HASH_Rekisteröinti..numero": "str",
    "YHTEYSTAPA": "str",
    "PALA": "Int64",
}
DTYPES_AVOHILMO = {
    "FID": "str",
    "KAYNTI_ALKOI": "str",
    "AVOHILMOID": "int",
    "FD_HASH_Rekisteröinti..numero": "str",
}


class BaseDataset(NamedTuple):
    label: str
    file_regex: str
    dtypes: dict[str, str]
    visit_date_column: str
    id_column: str
    date_format: str

    @property
    def file_paths(self):
        return tuple(THL_path / file for file in os.listdir(THL_path) if re.search(self.file_regex, file) and "~lock" not in file)

    @property
    def output_path(self):
        return outdir / f"processed_{self.label.lower()}_visits_matteo_{current_date}.csv"

    def process(self, file_path: Path, doctor_df: pd.DataFrame) -> pd.DataFrame:
        print(f"Reading file {file_path.name}")
        base_chunk = pd.read_csv(file_path, sep=";", encoding="latin-1", usecols=self.dtypes.keys(), dtype=self.dtypes)
        base_chunk = base_chunk.rename(
            columns={
                "FID": "PATIENT_ID",
                self.visit_date_column: "VISIT_DATE",
                "FD_HASH_Rekisteröinti..numero": "FD_HASH_CODE",
            }
        )
        # Harmonize date format to YYYY-MM-DD
        base_chunk["VISIT_DATE"] = pd.to_datetime(base_chunk["VISIT_DATE"], format=self.date_format).dt.strftime("%Y-%m-%d")
        # Replace "PUUTTUVA" (missing) values with proper NaN
        base_chunk["FD_HASH_CODE"] = base_chunk["FD_HASH_CODE"].replace("PUUTTUVA", pd.NA)
        # Connect doctor ids to doctor visits
        merged = pd.merge(base_chunk, doctor_df, on="FD_HASH_CODE", how="left")
        merged = merged[["PATIENT_ID", self.id_column, "VISIT_DATE", "FD_HASH_CODE", "DOCTOR_ID", "YHTEYSTAPA", "PALA"]]
        merged.to_csv(self.output_path, mode="a", header=not self.output_path.exists(), encoding="latin-1", index=False)
        print(f"Completed file {file_path.name} -> added to {self.output_path.name}")


# Specifies the base datasets to be used by the script (Hilmo and Avohilmo). Base datasets represent the doctor visits. Comment out the ones
# you don't need to include.
base_datasets = [
    # BaseDataset(
    #     label="Avohilmo",
    #     file_regex=r"AH_\d",
    #     dtypes=DTYPES_AVOHILMO,
    #     visit_date_column="KAYNTI_ALKOI",
    #     id_column="AVOHILMOID",
    #     date_format="%d.%m.%Y %H:%M",
    # ),
    BaseDataset(
        label="Hilmo",
        file_regex=r"HILMO\d",
        dtypes=DTYPES_HILMO,
        visit_date_column="TUPVA",
        id_column="HILMOID",
        date_format="%d.%m.%Y",
    ),
]


if __name__ == "__main__":
    script_start = time.time()

    doctor_data = pd.read_csv(Valvira_path, sep=";", encoding="latin1")
    doctor_data.rename(
        columns={"FID": "DOCTOR_ID", "FD_HASH_Rekisteröinti..numero": "FD_HASH_CODE"},
        inplace=True,
    )
    doctor_data = doctor_data[["DOCTOR_ID", "FD_HASH_CODE"]]
    doctor_data.drop_duplicates(inplace=True)

    for base_dataset in base_datasets:
        print(f"\n=== Processing {base_dataset.label} dataset ===", flush=True)
        base_dataset.output_path.unlink(missing_ok=True)  # Remove old file

        # Loop over all files belonging to the base dataset, for example, FD_2698_THL2023_2698_AH_11.csv, FD_2698_THL2023_2698_AH_12.csv, etc.
        for file_path in base_dataset.file_paths:
            base_dataset.process(file_path, doctor_data)

    print(f"\nFinished processing all files in {format_seconds_to_hms(time.time() - script_start)}.")
