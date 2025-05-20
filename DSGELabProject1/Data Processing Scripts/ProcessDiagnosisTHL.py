# Info:
# avohilmo base files = 14 (contains accident+cause ICD10 codes)
# avohilmo diagnosis files = 2
# hilmo base files = 5
# hilmo diagnosis files = 5

# This script processes the THL avohilmo and hilmo files in order to add the ICD10 code of the diagnosis to the main THL files.
# The script uses multiprocessing to speed up the process.
# The script takes around 3 hours to process all the files.

# Libraries:
import time
import os
import re
import pandas as pd
import multiprocessing
from pathlib import Path

# Paths
THL_path = Path("/media/volume/Data/Data_THL_2698_14.02.00_2023/THL/")
avohilmo_diagnosis_path = Path("/media/volume/Data/Data_THL_2698_14.02.00_2023_pk1_kayntisyy_folk/THL/")

avohilmo_base = [THL_path / file for file in os.listdir(THL_path) if re.search(r"AH_\d", file) and "~lock" not in file]
avohilmo_diagnosis = [avohilmo_diagnosis_path / file for file in os.listdir(avohilmo_diagnosis_path) if "~lock" not in file]
hilmo_base = [THL_path / file for file in os.listdir(THL_path) if re.search(r"HILMO\d", file) and "~lock" not in file]
hilmo_diagnosis = [THL_path / file for file in os.listdir(THL_path) if re.search(r"H_ICD10_\d", file) and "~lock" not in file]

outdir = Path("/media/volume/Projects/mikael/ProcessedData")
outpath_avohilmo = outdir / f"processed_avohilmo_{time.strftime('%Y%m%d')}.csv"
outpath_hilmo = outdir / f"processed_hilmo_{time.strftime('%Y%m%d')}.csv"

# Global variables
N_CPUs = 5


# Functions
def merge_extra_avohilmo(base_data, inpath, outpath):
    dtypes = {
        "FID": "str",
        "AVOHILMOID": "int",
        "JARJESTYS": "int",
        "LUOKITUS": "str",
        "LUOKITUSLUOKKA": "str",
    }
    df = pd.read_csv(
        inpath,
        sep=";",
        encoding="latin-1",
        usecols=dtypes.keys(),
        dtype=dtypes,
    )
    df = df[(df.JARJESTYS == 0) & (df.LUOKITUSLUOKKA == "icd10")]  # filter only main diagnosis
    merged_data = pd.merge(base_data, df, on=["FID", "AVOHILMOID"], how="inner")
    if not merged_data.empty:
        merged_data = merged_data[
            [
                "FID",
                "FD_HASH_Rekisteröinti..numero",
                "AVOHILMOID",
                "KAYNTI_ALKOI",
                "TAPATURMATYYPPI",
                "ULKOINEN_SYY",
                "LUOKITUS",
            ]
        ]
        merged_data.to_csv(outpath, mode="a", header=False, index=False)


def merge_extra_hilmo(base_data, inpath, outpath):
    dtypes = {
        "FID": "str",
        "HILMOID": "int",
        "KOODI": "str",
        "N": "int",
        "KENTTA": "str",
    }
    df = pd.read_csv(
        inpath,
        sep=";",
        encoding="latin-1",
        usecols=dtypes.keys(),
        dtype=dtypes,
    )
    df = df[(df.N == 0) & (df.KENTTA == "PDGO")]  # filter only main diagnosis
    merged_data = pd.merge(base_data, df, on=["FID", "HILMOID"], how="inner")
    if not merged_data.empty:
        merged_data = merged_data[["FID", "FD_HASH_Rekisteröinti..numero", "HILMOID", "TUPVA", "KOODI"]]
        merged_data.to_csv(outpath, mode="a", header=False, index=False)


# Main:
pd.DataFrame(
    columns=[
        "FID",
        "FD_HASH_Rekisteröinti..numero",
        "AVOHILMOID",
        "KAYNTI_ALKOI",
        "TAPATURMATYYPPI",
        "ULKOINEN_SYY",
        "ICD10",
    ]
).to_csv(outpath_avohilmo, encoding="latin-1", index=False)
for base_file in avohilmo_base:
    start_time = time.time()
    # 1. open file
    dtypes_avohilmo = {
        "FID": "str",
        "FD_HASH_Rekisteröinti..numero": "str",
        "AVOHILMOID": "int",
        "KAYNTI_ALKOI": "str",
        "TAPATURMATYYPPI": "str",
        "ULKOINEN_SYY": "str",
    }
    df = pd.read_csv(
        base_file,
        sep=";",
        encoding="latin-1",
        usecols=dtypes_avohilmo.keys(),
        dtype=dtypes_avohilmo,
    )
    # 2. merge diagnosis info (using AVOHILMOID)
    # - multiprocessing for speed
    # - append to unique output file
    with multiprocessing.Pool(processes=N_CPUs) as pool:
        for extra in avohilmo_diagnosis:
            pool.apply(merge_extra_avohilmo, args=(df, extra, outpath_avohilmo))
    end_time = time.time()
    print(f"Time taken for {base_file.name}: {end_time - start_time} seconds")

pd.DataFrame(columns=["FID", "FD_HASH_Rekisteröinti..numero", "HILMOID", "TUPVA", "KOODI"]).to_csv(outpath_hilmo, encoding="latin-1", index=False)
for base_file in hilmo_base:
    start_time = time.time()
    # 1. open file
    dtypes_hilmo = {
        "FID": "str",
        "FD_HASH_Rekisteröinti..numero": "str",
        "HILMOID": "int",
        "TUPVA": "str",
    }
    df = pd.read_csv(
        base_file,
        sep=";",
        encoding="latin-1",
        usecols=dtypes_hilmo.keys(),
        dtype=dtypes_hilmo,
    )
    # 2. merge diagnosis info (using HILMOID)
    # - multiprocessing for speed
    # - append to unique output file
    with multiprocessing.Pool(processes=N_CPUs) as pool:
        for extra in hilmo_diagnosis:
            pool.apply(merge_extra_hilmo, args=(df, extra, outpath_hilmo))
    pool.close()
    end_time = time.time()
    print(f"Time taken for {base_file.name}: {end_time - start_time} seconds")
