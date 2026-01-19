#### Info:
# This script takes as input a list of doctor ids (cases) and extracts information related to the outcome used in the Difference in Difference analysis.

# INPUT: 
# - id_list: file path to list of doctor IDs to be used (no spouse or offspring)
# - file_path: path of the doctor-patient longitudinal file
# - outdir: directory where the results want to be saved

#### Libraries:
import gzip
import argparse
import pandas as pd

##### Arguments
id_list = "/media/volume/Projects/DSGELabProject1/doctors_20250424.csv"
long_file_path = "/media/volume/Projects/DSGELabProject1/ProcessedData/imputed_prescriptions_20250501152849.csv.gz"
outdir = "/media/volume/Projects/DSGELabProject1/did_pipeline"

#### Main
with open(id_list, 'r') as f:
    ID_LIST = set(line.strip('\n').strip('\"') for line in f)

# Open output file
with open(outdir+'/Outcomes_20250506.csv', 'w') as out_file:
    chunk_size = 1_000_000  # Adjust chunk size depending on memory
    first_chunk = True

    # Read the big file in chunks
    with gzip.open(long_file_path, 'rt') as f:
        for chunk in pd.read_csv(f, chunksize=chunk_size):
            matched_rows = chunk[chunk['DOCTOR_ID'].astype(str).isin(ID_LIST)]
            matched_rows = matched_rows.rename(columns={'PRESCRIPTION_DATE':'DATE'})
            matched_rows = matched_rows[['DOCTOR_ID', 'PATIENT_ID', 'DATE', 'CODE']]

            if not matched_rows.empty:
                # Write header only for the first matching chunk
                matched_rows.to_csv(out_file, index=False, header=first_chunk, mode='a')
                first_chunk = False
