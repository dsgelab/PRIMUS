#!/usr/bin/env python
"""
Modified script to filter Purchases and Prescriptions by a list of PATIENT_IDs.
This version retains the FD_HASH_CODE column in the output.
The join with doctor data has been removed, and a placeholder for DOCTOR_ID is used.
"""

#### Libraries
import argparse
import os
import time
import pandas as pd
import multiprocessing

#### Paths
Kela_path = "/media/volume/Data/Data_THL_2698_14.02.00_2023/Kela/"
Reseptikeskus_path = "/media/volume/Data/Data_THL_2698_14.02.00_2023/Reseptikeskus/"
processed_THL_path = "/media/volume/Projects/DSGELabProject1/"

##### Arguments
parser = argparse.ArgumentParser()
parser.add_argument('--outdir', type=str, default='/media/volume/Projects/jg/',
                    help='Directory where the results will be saved')
parser.add_argument('--patient_list', type=str,
                    default='/media/volume/Projects/jg/PATlist_prescpurch_investigate.txt',
                    help='File path to list of PATIENT_IDs. If not provided, all patients will be used')
args = parser.parse_args()

print("Using the following arguments:")
print(args)

#### Global variables
N_CPUs = 6

##### Functions
# def remove_missing_hash(data):
#     """Removes rows with missing or invalid hash codes."""
#     N0 = data.shape[0]
#     data = data[(data['FD_HASH_CODE'].notna()) & (data['FD_HASH_CODE'] != "PUUTTUVA")]
#     N1 = data.shape[0] / N0 * 100
#     print('Removing', round(100 - N1, 2), '% rows with missing hash codes')
#     return data

def process_purchases(inpath, patient_ids, outpath):
    """Processes a Kela purchases file and writes the filtered data to the output file."""
    print('Processing (Purchase):', inpath)
    dtypes_kela = {
        'FID': 'str',
        'FD_HASH_Rekisteröinti..numero': 'str',
        'LAAKEMAARAYS_KIRJOITUS_PV': 'str',
        'TOIMITUS_PV': 'str',
        'ATC5_KOODI': 'str'
    }
    df = pd.read_csv(os.path.join(Kela_path, inpath), sep=';', encoding='latin-1',
                     usecols=dtypes_kela.keys(), dtype=dtypes_kela)
    # Rename columns
    df.rename(columns={
        'FID': 'PATIENT_ID', 
        'FD_HASH_Rekisteröinti..numero': 'FD_HASH_CODE',
        'LAAKEMAARAYS_KIRJOITUS_PV': 'PRESCRIPTION_DATE'
    }, inplace=True)
    # Remove rows with missing/invalid hash codes
    # df = remove_missing_hash(df)
    # Filter for the specified patient IDs (if provided)
    if patient_ids is not None:
        df = df[df['PATIENT_ID'].isin(patient_ids)]
    # Add placeholder for DOCTOR_ID
    df['DOCTOR_ID'] = ""
    # Process and create additional columns
    df['REGISTER'] = 'Purchase'
    df['DATE'] = pd.to_datetime(df['TOIMITUS_PV'], format='%Y-%m-%d')
    df['PRESCRIPTION_DATE'] = pd.to_datetime(df['PRESCRIPTION_DATE'], format='%Y-%m-%d')
    df.rename(columns={'ATC5_KOODI': 'CODE'}, inplace=True)
    df['PRIVATE'] = 0
    df['PUBLIC'] = 0
    # Reorder columns to include FD_HASH_CODE
    df = df[['DOCTOR_ID', 'PATIENT_ID', 'FD_HASH_CODE', 'REGISTER', 'DATE', 'PRESCRIPTION_DATE', 'CODE', 'PRIVATE', 'PUBLIC']]
    # Append to output CSV
    df.to_csv(outpath, mode='a', header=False, index=False)

def process_prescriptions(inpath, patient_ids, outpath):
    """Processes a Reseptikeskus prescriptions file and writes the filtered data to the output file."""
    print('Processing (Prescription):', inpath)
    dtypes_reseptikeskus = {
        'FID': 'str',
        'FD_HASH_Rekisteröinti..numero': 'str',
        'DATE_PK': 'str',
        'ATC_CODE': 'str',
        'SECTOR': 'str'
    }
    df = pd.read_csv(os.path.join(Reseptikeskus_path, inpath), sep=';', encoding='latin-1',
                     quotechar='"', escapechar='\\', usecols=dtypes_reseptikeskus.keys(), dtype=dtypes_reseptikeskus)
    # Rename columns
    df.rename(columns={
        'FID': 'PATIENT_ID', 
        'FD_HASH_Rekisteröinti..numero': 'FD_HASH_CODE'
    }, inplace=True)
    # Remove rows with missing/invalid hash codes
    # df = remove_missing_hash(df)
    # Filter for the specified patient IDs (if provided)
    if patient_ids is not None:
        df = df[df['PATIENT_ID'].isin(patient_ids)]
    # Add placeholder for DOCTOR_ID
    df['DOCTOR_ID'] = ""
    # Process and create additional columns
    df['REGISTER'] = 'Prescription'
    df['DATE'] = pd.to_datetime(df['DATE_PK'], format='%Y%m%d')
    df['PRESCRIPTION_DATE'] = pd.to_datetime(df['DATE_PK'], format='%Y%m%d')
    df.rename(columns={'ATC_CODE': 'CODE'}, inplace=True)
    df['PRIVATE'] = df['SECTOR'].apply(lambda x: 1 if x == '2' else 0)
    df['PUBLIC'] = df['SECTOR'].apply(lambda x: 1 if x == '1' else 0)
    # Reorder columns to include FD_HASH_CODE
    df = df[['DOCTOR_ID', 'PATIENT_ID', 'FD_HASH_CODE', 'REGISTER', 'DATE', 'PRESCRIPTION_DATE', 'CODE', 'PRIVATE', 'PUBLIC']]
    # Append to output CSV
    df.to_csv(outpath, mode='a', header=False, index=False)

#### Main ####
# Load patient IDs from file if provided; otherwise, process all patients
if args.patient_list and os.path.exists(args.patient_list):
    with open(args.patient_list, 'r') as file:
        patient_ids = [line.strip() for line in file if line.strip()]
    print(f"Filtering to {len(patient_ids)} patient IDs from {args.patient_list}")
else:
    patient_ids = None
    print("No patient_list file provided or file not found; processing all patients.")

# Get list of files to process
kela_files = [file for file in os.listdir(Kela_path) if 'LAAKEOSTOT' in file and '~lock' not in file]
reseptikeskus_files = [file for file in os.listdir(Reseptikeskus_path) if 'Laakemaaraykset' in file and '~lock' not in file]
print('Found', len(kela_files), 'purchases files')
print('Found', len(reseptikeskus_files), 'prescription files')

# Multiprocessing tasks for longitudinal file generation
tasks = [
    (process_purchases, kela_files),
    (process_prescriptions, reseptikeskus_files)
]

with multiprocessing.Pool(processes=N_CPUs) as pool:
    output_file = os.path.join(args.outdir, f"doctor_patient_prescpurch_patlist_{time.strftime('%Y%m%d')}.csv")
    # Prepare the output file with required columns (including FD_HASH_CODE)
    pd.DataFrame(columns=['DOCTOR_ID', 'PATIENT_ID', 'FD_HASH_CODE', 'REGISTER', 'DATE', 
                          'PRESCRIPTION_DATE', 'CODE', 'PRIVATE', 'PUBLIC']).to_csv(output_file, index=False)
    print(f'Output will be appended to {output_file}')

    for func, files in tasks:
        start_time = time.time()
        for input_file in files:
            pool.apply(func, args=(input_file, patient_ids, output_file))
        end_time = time.time()
        print(f'Time taken for {func.__name__}: {end_time - start_time} seconds')
    pool.close()
    pool.join()
