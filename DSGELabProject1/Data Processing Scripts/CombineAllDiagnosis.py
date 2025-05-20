# Info
# This script takes all the pre-process hidiagnosis files (hilmo/avohilmo) and joins them to the respective doctor and procedure files,
# and then push to one unique CSV file

# Libraries
import os
import pandas as pd
import time
import re
from pathlib import Path
from typing import NamedTuple

# Paths
InDir = Path("/media/volume/Projects/mikael/ProcessedData/")
hilmo_file = InDir / "processed_hilmo_20250514.csv"
avohilmo_file = InDir / "processed_avohilmo_20250514.csv"
Valvira_path = "/media/volume/Data/Data_THL_2698_14.02.00_2023/Valvira/FD_2698_Liite 1 THL_2698_14.02.00_2023.csv"
OutDir = "/media/volume/Projects/mikael/ProcessedData/"
output_csv = os.path.join(OutDir, f"AllConnectedDiagnosis_{time.strftime('%Y%m%d')}.csv")
THL_path = Path("/media/volume/Data/Data_THL_2698_14.02.00_2023/THL/")

# Global variables
CHUNK_SIZE = 1_000_000
DTYPES_HILMO = {
    "FID": "str",
    "HILMOID": "int",
    "FD_HASH_Rekisteröinti..numero": "str",
    "TUPVA": "str",
    "KOODI": "str",
}

DTYPES_AVOHILMO = {
    "FID": "str",
    "AVOHILMOID": "int",
    "FD_HASH_Rekisteröinti..numero": "str",
    "KAYNTI_ALKOI": "str",
    "ICD10": "str",
}
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

# Main

# load doctors information
doctor_data = pd.read_csv(Valvira_path, sep=";", encoding="latin1")
doctor_data.rename(
    columns={"FID": "DOCTOR_ID", "FD_HASH_Rekisteröinti..numero": "FD_HASH_CODE"},
    inplace=True,
)
doctor_data = doctor_data[["DOCTOR_ID", "FD_HASH_CODE"]]
doctor_data.drop_duplicates(inplace=True)


def preprocess_procedures(procedure_regex, dtypes, procedure_column, order_column):
    procedure_files = [THL_path / file for file in os.listdir(THL_path) if re.search(procedure_regex, file) and "~lock" not in file]
    df = pd.concat(
        (
            pd.read_csv(
                file,
                sep=";",
                encoding="latin1",
                usecols=dtypes.keys(),
                dtype=dtypes,
            )
            for file in procedure_files
        ),
        ignore_index=True,
    )
    df = df[df[procedure_column].str.match(r"^[A-Z0-9]{5}$", na=False)]  # Filter nomesco codes
    df = df[df[order_column] == 0]  # Filter only main procedure
    df = df.rename(columns={"FID": "PATIENT_ID", procedure_column: "PROCEDURE"})
    return df


# Load procedures
ahp_df = preprocess_procedures(r"AH_TOIMENPITEET_\d", DTYPES_AVOHILMO_PROCEDURES, "TOIMENPIDE", "JARJESTYS")
hp_df = preprocess_procedures(r"H_TOIMENP_\d", DTYPES_HILMO_PROCEDURES, "TOIMP", "N")


class Dataset(NamedTuple):
    label: str
    file: Path
    procedure_df: pd.DataFrame
    dtypes: dict[str, str]
    visit_date_column: str
    id_column: str
    code_column: str
    date_format: str


datasets = [
    Dataset(
        label="Avohilmo",
        file=avohilmo_file,
        procedure_df=ahp_df,
        dtypes=DTYPES_AVOHILMO,
        visit_date_column="KAYNTI_ALKOI",
        id_column="AVOHILMOID",
        code_column="ICD10",
        date_format="%d.%m.%Y %H:%M",
    ),
    Dataset(
        label="Hilmo",
        file=hilmo_file,
        procedure_df=hp_df,
        dtypes=DTYPES_HILMO,
        visit_date_column="TUPVA",
        id_column="HILMOID",
        code_column="KOODI",
        date_format="%d.%m.%Y",
    ),
]

for dataset_idx, dataset in enumerate(datasets):
    # Connect all diagnosis to doctors
    print(f"Processing {dataset.label}")
    start_time = time.time()

    for chunk_index, chunk in enumerate(
        pd.read_csv(
            dataset.file,
            chunksize=CHUNK_SIZE,
            sep=",",
            encoding="latin-1",
            usecols=dataset.dtypes.keys(),
            dtype=dataset.dtypes,
        )
    ):
        print(f"Processing chunk {chunk_index}", end="\r")
        processed_data = chunk.rename(
            columns={
                "FID": "PATIENT_ID",
                "FD_HASH_Rekisteröinti..numero": "FD_HASH_CODE",
                dataset.code_column: "VISIT_CODE",
            }
        )
        processed_data["VISIT_DATE"] = pd.to_datetime(processed_data[dataset.visit_date_column], format=dataset.date_format)
        processed_data["SOURCE"] = dataset.label
        processed_data = pd.merge(processed_data, doctor_data, on="FD_HASH_CODE", how="left")
        processed_data = pd.merge(
            processed_data,
            dataset.procedure_df,
            on=["PATIENT_ID", dataset.id_column],
            how="left",
        )
        processed_data = processed_data[
            [
                "PATIENT_ID",
                "VISIT_DATE",
                "VISIT_CODE",
                "SOURCE",
                "FD_HASH_CODE",
                "DOCTOR_ID",
                "PROCEDURE",
            ]
        ]

        # Append results to CSV
        first_chunk = chunk_index == 0 and dataset_idx == 0
        mode = "w" if first_chunk else "a"
        header = first_chunk
        processed_data.to_csv(output_csv, mode=mode, header=header, index=False)

    end_time = time.time()
    print()
    print(f"Finished processing file in {end_time - start_time:.2f} seconds")
