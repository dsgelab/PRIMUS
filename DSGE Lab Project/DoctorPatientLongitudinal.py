# LAST UPDATED: 2025-02-10
# generate longitudinal view of Doctor-Patient interactions (based on their ID)

import argparse
import os
import re
import time
import pandas as pd
import multiprocessing 

# Paths
Valvira_path = "/media/volume/Data/Data_THL_2698_14.02.00_2023/Valvira/FD_2698_Liite 1 THL_2698_14.02.00_2023.csv"
Kela_path = "/media/volume/Data/Data_THL_2698_14.02.00_2023/Kela/"
Reseptikeskus_path = "/media/volume/Data/Data_THL_2698_14.02.00_2023/Reseptikeskus/"
THL_path = "/media/volume/Data/Data_THL_2698_14.02.00_2023/THL/"

# Functions
def remove_missing_hash(data):
    N0 = data.shape[0]
    data = data[(data['FD_HASH_CODE'].notna()) & (data['FD_HASH_CODE'] != "PUUTTUVA")]
    N1 = data.shape[0] / N0 * 100
    print('removing', round(100 - N1, 2), '% missing hash codes')
    return data

def process_purchases(inpath, doctor_data, outpath):
    #1. read file, only required columns, and rename
    dtypes_kela = {
    'FID':'str',
    'FD_HASH_Rekisteröinti..numero':'str',
    'TOIMITUS_PV':'str',
    'ATC5_KOODI':'str'
    }
    df = pd.read_csv(os.path.join(Kela_path, inpath), sep=';', encoding='latin-1', usecols=dtypes_kela.keys(), dtype=dtypes_kela)
    df.rename(columns={'FID': 'PATIENT_ID', 'FD_HASH_Rekisteröinti..numero': 'FD_HASH_CODE'}, inplace=True)
    #2. remove missing hash keys
    df = remove_missing_hash(df)
    #3. process data
    merged_data = pd.merge(doctor_data, df, on='FD_HASH_CODE', how='inner')
    merged_data['REGISTER'] = 'Purchase'
    merged_data['DATE'] = pd.to_datetime(df['TOIMITUS_PV'], format='%Y-%m-%d')
    merged_data.rename(columns={'ATC5_KOODI': 'CODE'}, inplace=True)
    merged_data = merged_data[['DOCTOR_ID', 'PATIENT_ID', 'REGISTER', 'DATE', 'CODE']]
    #4. output to longitudinal file
    merged_data.to_csv(outpath, mode='a', header=False, index=False)

def process_prescriptions(inpath, doctor_data, outpath):
    #1. read file, only required columns, and rename
    dtypes_reseptikeskus = {
    'FID': 'str',
    'FD_HASH_Rekisteröinti..numero': 'str',
    'DATE_PK':'str',
    'ATC_CODE':'str'
    }
    df = pd.read_csv(os.path.join(Reseptikeskus_path, inpath), sep=';', encoding='latin-1', usecols= dtypes_reseptikeskus.keys(), dtype=dtypes_reseptikeskus)
    df.rename(columns={'FID': 'PATIENT_ID', 'FD_HASH_Rekisteröinti..numero': 'FD_HASH_CODE'}, inplace=True)
    #2. remove missing hash keys
    df = remove_missing_hash(df)
    #3. process data
    merged_data = pd.merge(doctor_data, df, on='FD_HASH_CODE', how='inner')
    merged_data['REGISTER'] = 'Prescription'
    merged_data['DATE'] = pd.to_datetime(df['DATE_PK'], format='%Y%m%d')
    merged_data.rename(columns={'ATC_CODE': 'CODE'}, inplace=True)
    merged_data = merged_data[['DOCTOR_ID', 'PATIENT_ID', 'REGISTER', 'DATE', 'CODE']]
    #4. output to longitudinal file
    merged_data.to_csv(outpath, mode='a', header=False, index=False)

def process_diagnosis_avohilmo(inpath, doctor_data, outpath):
    #1. read file, only required columns, and rename
    dtypes_avohilmo = {
    'FID': 'str',
    'FD_HASH_Rekisteröinti..numero': 'str',
    'KAYNTI_ALKOI':'str',
    'TAPATURMATYYPPI':'str',
    }
    df = pd.read_csv(os.path.join(THL_path, inpath), sep=';', encoding='latin-1', usecols= dtypes_avohilmo.keys(), dtype=dtypes_avohilmo)
    df.rename(columns={'FID': 'PATIENT_ID', 'FD_HASH_Rekisteröinti..numero': 'FD_HASH_CODE'}, inplace=True)
    #2. remove missing hash keys
    df = remove_missing_hash(df)
    #3. process data
    merged_data = pd.merge(doctor_data, df, on='FD_HASH_CODE', how='inner')
    merged_data['REGISTER'] = 'Diagnosis Avohilmo'
    merged_data['DATE'] = pd.to_datetime(df['KAYNTI_ALKOI'], format='%d.%m.%Y %H:%M').dt.date
    merged_data.rename(columns={'TAPATURMATYYPPI': 'CODE'}, inplace=True)
    merged_data = merged_data[['DOCTOR_ID', 'PATIENT_ID', 'REGISTER', 'DATE', 'CODE']]
    #4. output to longitudinal file
    merged_data.to_csv(outpath, mode='a', header=False, index=False)

def process_diagnosis_hilmo(inpath, doctor_data, outpath):
    #1. read file, only required columns, and rename
    dtypes_hilmo = {
    'FID': 'str',
    'FD_HASH_Rekisteröinti..numero': 'str',
    'TUPVA':'str'
    }
    df = pd.read_csv(os.path.join(THL_path, inpath), sep=';', encoding='latin-1', usecols=dtypes_hilmo.keys(), dtype=dtypes_hilmo)
    df.rename(columns={'FID': 'PATIENT_ID', 'FD_HASH_Rekisteröinti..numero': 'FD_HASH_CODE'}, inplace=True)
    #2. remove missing hash keys
    df = remove_missing_hash(df)
    #3. process data
    merged_data = pd.merge(doctor_data, df, on='FD_HASH_CODE', how='inner')
    merged_data['REGISTER'] = 'Diagnosis Hilmo'
    merged_data['DATE'] = pd.to_datetime(df['TUPVA'], format='%d.%m.%Y')
    merged_data['CODE'] ='-'
    merged_data = merged_data[['DOCTOR_ID', 'PATIENT_ID', 'REGISTER', 'DATE', 'CODE']]
    #4. output to longitudinal file
    merged_data.to_csv(outpath, mode='a', header=False, index=False)

# Arguments
parser = argparse.ArgumentParser()
parser.add_argument('--outdir', type=str, default='/media/volume/Projects/mattferr/',help='directory where the results want to be saved')
parser.add_argument('--doctor_list', type=str, default=False, help='file path to list of doctor IDs, if not provided, will use all doctors')
args = parser.parse_args()

print("using the following arguments: ")
print(args)

#### Main ####
# Load doctor data
doctor_data = pd.read_csv(Valvira_path, sep=';', encoding='latin1')
if args.doctor_list:
    with open(args.doctor_list, 'r') as file:
        ID_LIST = [line.strip() for line in file.readlines()]
else:
    ID_LIST = doctor_data['FID'].unique().tolist()
doctor_data = doctor_data[doctor_data['FID'].isin(ID_LIST)]
doctor_data.rename(columns={'FID': 'DOCTOR_ID', 'FD_HASH_Rekisteröinti..numero': 'FD_HASH_CODE'}, inplace=True)
doctor_data = doctor_data[['DOCTOR_ID', 'FD_HASH_CODE']]  
doctor_data.drop_duplicates(inplace=True) 

# Get list of files to process
kela_files = [file for file in os.listdir(Kela_path) if 'LAAKEOSTOT' in file and '~lock' not in file]
reseptikeskus_files = [file for file in os.listdir(Reseptikeskus_path) if 'Laakemaaraykset' in file and '~lock' not in file]
avohilmo_files = [file for file in os.listdir(THL_path) if re.search(r'AH_\d', file) and '~lock' not in file]
hilmo_files = [file for file in os.listdir(THL_path) if re.search(r'HILMO\d', file) and '~lock' not in file]
print('Found', len(kela_files), 'purchases files')
print('Found', len(reseptikeskus_files), 'prescription files')
print('Found', len(avohilmo_files), 'avohilmo files')
print('Found', len(hilmo_files), 'hilmo files')

# Multiprocessing tasks for longitudinal file generation
# RAM = 125 Gb, CPU = 32 cores
N_CPUs = 6

tasks = [
    (process_purchases, kela_files),
    (process_prescriptions, reseptikeskus_files),
    (process_diagnosis_avohilmo, avohilmo_files),
    (process_diagnosis_hilmo, hilmo_files)
]

with multiprocessing.Pool(processes=N_CPUs) as pool:

    output_file = f"{args.outdir}doctor_patient_longitudinal_{time.strftime('%Y%m%d')}.csv"
    # Prepare the output file with required columns
    pd.DataFrame(columns=['DOCTOR_ID', 'PATIENT_ID', 'REGISTER', 'DATE', 'CODE']).to_csv(output_file, index=False)
    print(f'printing to {output_file}')

    for func, files in tasks:
        start_time = time.time()
        for input_file in files:
            pool.apply_async(func, args=(input_file, doctor_data, output_file))
            end_time = time.time()
        print(f'Time taken for {func}: {end_time - start_time} seconds')
    pool.close()
    pool.join()