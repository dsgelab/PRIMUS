import pandas as pd
from collections import defaultdict
import sys
from datetime import datetime
import argparse

# Global Vars
chunk_size=1_000_000
input_file = "/media/volume/Projects/mattferr/did_pipeline/Outcomes_ForRatio_20250506.csv"

parser = argparse.ArgumentParser(description="Prepare outcome counts.")
parser.add_argument("--output_file", type=str, default=None, help="Path to output file")
args = parser.parse_args()

if args.output_file is not None:
    output_file = args.output_file
else:
    today_str = datetime.today().strftime('%Y%m%d')
    output_file = f"/media/volume/Projects/DSGELabProject1/DiD_Highthroughput/Version2/GeneratePairs/OutcomeCounts_{today_str}.csv"

# Read in chunks and extract counts of unique id-code combinations
unique_ids = set()
unique_codes = set()
combination_counts = defaultdict(int)

for chunk in pd.read_csv(input_file, chunksize=chunk_size):
    chunk.columns = chunk.columns.str.strip()
    chunk_clean = chunk.dropna(subset=['DOCTOR_ID', 'CODE'])

    # Truncate CODE to 5 chars if longer
    chunk_clean['CODE'] = chunk_clean['CODE'].astype(str).str[:5]

    unique_ids.update(chunk_clean['DOCTOR_ID'].unique())
    unique_codes.update(chunk_clean['CODE'].unique())
    
    for _, row in chunk_clean.iterrows():
        combination_counts[(row['DOCTOR_ID'], row['CODE'])] += 1

sorted_ids = sorted(unique_ids)
sorted_codes = sorted(unique_codes)

with open(output_file, 'w') as f:
    f.write("ID,CODE,COUNT\n")
    for id_val in sorted_ids:
        for code_val in sorted_codes:
            count = combination_counts.get((id_val, code_val), 0)
            f.write(f"{id_val},{code_val},{count}\n")
