#### Info:
# This script takes as input a list of doctor ids (cases) and extracts the relative rows from the doctor-patient longitudinal file.

#TODO:
# can speed up if we use a longitudinal file with only prescriptions (or purchases)

# INPUT: 
# - id_list: file path to list of doctor IDs to be used (no spouse or offspring)
# - register: type of register to be used (e.g. Prescription)
# - outcome_code: ATC code of required outcome
# - long_file_path: path of the doctor-patient longitudinal file
# - outdir: directory where the results want to be saved
# OUTPUT:
# - Outcomes.csv: file with the rows of the doctor-patient longitudinal file where the doctor id is in the list of ids


#### Libraries:
import gzip
import argparse
import pandas as pd

##### Arguments
parser = argparse.ArgumentParser()
parser.add_argument('--id_list', type=str, help='file path to list of doctor IDs to be used')
parser.add_argument('--long_file_path', type=str, default='/media/volume/Projects/DSGELabProject1/doctor_patient_longitudinal_20250220.csv.gz',help='path of the doctor-patient longitudinal file')
parser.add_argument('--outdir', type=str,help='directory where the results want to be saved')
parser.add_argument('--outcome_register', type=str, default='Prescription', help='type of register to be used')
parser.add_argument('--outcome_code', type=str, default=None, help='ATC code of required outcome')
args = parser.parse_args()

print("using the following arguments: ")
print(args)

#### Main
CODE_REGEX = "^" + args.outcome_code # = starts with the code
with open(args.id_list, 'r') as f:
    ID_LIST = set(line.strip('\n').strip('\"') for line in f)

# Open output file
with open(args.outdir+'/Outcomes.csv', 'w') as out_file:
    chunk_size = 1_000_000  # Adjust chunk size depending on memory
    first_chunk = True

    # Read the big file in chunks
    with gzip.open(args.long_file_path, 'rt') as f:
        for chunk in pd.read_csv(f, chunksize=chunk_size):
            # Filter rows where:
            # DOCTOR_ID is in id_list 
            # CODE starts with outcome_code
            # REGISTER is outcome_register
            matched_rows = chunk[chunk['DOCTOR_ID'].astype(str).isin(ID_LIST)]
            assert args.outcome_register in ['Prescription','Purchase'], "Register can be either Prescription or Purchase"
            matched_rows = matched_rows[matched_rows['REGISTER'] == args.outcome_register]
            if not args.outcome_code is None:
                matched_rows = matched_rows[matched_rows['CODE'].astype(str).str.match(CODE_REGEX)]

            if not matched_rows.empty:
                # Write header only for the first matching chunk
                matched_rows.to_csv(out_file, index=False, header=first_chunk, mode='a')
                first_chunk = False
