#### Info
# machine stats: RAM = 125 Gb, CPU = 32 cores

# This script takes as input 4 different types of files: Kela purchases, Reseptikeskus prescriptions, THL avohilmo, and THL hilmo.
# NB: THL files have bee preprocessed using ProcessDiagnosisTHL.py

# This script processes each file type separately and merges them with the doctor data to create a longitudinal file
# which contains information about each doctor patient interaction 

# The script uses multiprocessing to speed up the process.
# The script takes around 3 hours to process all the files and create the longitudinal file.


#### Libraries
import argparse
import os
import re
import time
import pandas as pd
import multiprocessing 

#### Paths
Valvira_path = "/media/volume/Data/Data_THL_2698_14.02.00_2023/Valvira/FD_2698_Liite 1 THL_2698_14.02.00_2023.csv"
Kela_path = "/media/volume/Data/Data_THL_2698_14.02.00_2023/Kela/"
Reseptikeskus_path = "/media/volume/Data/Data_THL_2698_14.02.00_2023/Reseptikeskus/"
processed_THL_path = "/media/volume/Projects/DSGELabProject1/" # using the processed files from ProcessDiagnosisTHL.py

##### Arguments
parser = argparse.ArgumentParser()
parser.add_argument('--outdir', type=str, default='/media/volume/Projects/DSGELabProject1/',help='directory where the results want to be saved')
parser.add_argument('--doctor_list', type=str, default=False, help='file path to list of doctor IDs, if not provided, will use all doctors')
args = parser.parse_args()

print("using the following arguments: ")
print(args)

#### Global variables
N_CPUs = 6

##### Functions
def remove_missing_hash(data):
    N0 = data.shape[0]
    data = data[(data['FD_HASH_CODE'].notna()) & (data['FD_HASH_CODE'] != "PUUTTUVA")]
    N1 = data.shape[0] / N0 * 100
    print('removing', round(100 - N1, 2), '% missing hash codes')
    return data

def process_purchases(inpath, doctor_data, outpath):
    #1. read file, only required columns, and rename
    print('processing: ', inpath)
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
    merged_data['PRIVATE']=0
    merged_data['PUBLIC']=0
    merged_data = merged_data[['DOCTOR_ID', 'PATIENT_ID', 'REGISTER', 'DATE', 'CODE', 'PRIVATE', 'PUBLIC']]
    #4. output to longitudinal file
    merged_data.to_csv(outpath, mode='a', header=False, index=False)

def process_prescriptions(inpath, doctor_data, outpath):
    #1. read file, only required columns, and rename
    print('processing: ', inpath)
    dtypes_reseptikeskus = {
    'FID': 'str',
    'FD_HASH_Rekisteröinti..numero': 'str',
    'DATE_PK':'str',
    'ATC_CODE':'str',
    'SECTOR':'str'
    }
    df = pd.read_csv(os.path.join(Reseptikeskus_path, inpath), sep=';', encoding='latin-1', quotechar='"', escapechar='\\', usecols= dtypes_reseptikeskus.keys(), dtype=dtypes_reseptikeskus)
    df.rename(columns={'FID': 'PATIENT_ID', 'FD_HASH_Rekisteröinti..numero': 'FD_HASH_CODE'}, inplace=True)
    #2. remove missing hash keys
    df = remove_missing_hash(df)
    #3. process data
    merged_data = pd.merge(doctor_data, df, on='FD_HASH_CODE', how='inner')
    merged_data['REGISTER'] = 'Prescription'
    merged_data['DATE'] = pd.to_datetime(df['DATE_PK'], format='%Y%m%d')
    merged_data.rename(columns={'ATC_CODE': 'CODE'}, inplace=True)
    merged_data['PRIVATE'] = df['SECTOR'].apply(lambda x: 1 if x == '2' else 0)
    merged_data['PUBLIC'] = df['SECTOR'].apply(lambda x: 1 if x == '1' else 0)
    merged_data = merged_data[['DOCTOR_ID', 'PATIENT_ID', 'REGISTER', 'DATE', 'CODE', 'PRIVATE', 'PUBLIC']]
    #4. output to longitudinal file
    merged_data.to_csv(outpath, mode='a', header=False, index=False)

def process_diagnosis_avohilmo(inpath, doctor_data, outpath):
    #1. read file, only required columns, and rename
    print('processing: ', inpath)
    dtypes_avohilmo = {
    'FID': 'str',
    'FD_HASH_Rekisteröinti..numero': 'str',
    'KAYNTI_ALKOI':'str',
    'ICD10':'str'
    }
    df = pd.read_csv(os.path.join(processed_THL_path, inpath), sep=',', encoding='latin-1', usecols= dtypes_avohilmo.keys(), dtype=dtypes_avohilmo)
    df.rename(columns={'FID': 'PATIENT_ID', 'FD_HASH_Rekisteröinti..numero': 'FD_HASH_CODE'}, inplace=True)
    #2. remove missing hash keys
    df = remove_missing_hash(df)
    #3. process data
    merged_data = pd.merge(doctor_data, df, on='FD_HASH_CODE', how='inner')
    merged_data['REGISTER'] = 'Diagnosis Avohilmo'
    merged_data['DATE'] = pd.to_datetime(df['KAYNTI_ALKOI'], format='%d.%m.%Y %H:%M').dt.date
    merged_data.rename(columns={'ICD10': 'CODE'}, inplace=True)
    merged_data['PRIVATE']=0
    merged_data['PUBLIC']=0
    merged_data = merged_data[['DOCTOR_ID', 'PATIENT_ID', 'REGISTER', 'DATE', 'CODE','PRIVATE', 'PUBLIC']]
    #4. output to longitudinal file
    merged_data.to_csv(outpath, mode='a', header=False, index=False)

def process_diagnosis_hilmo(inpath, doctor_data, outpath):
    #1. read file, only required columns, and rename
    print('processing: ', inpath)
    dtypes_hilmo = {
    'FID': 'str',
    'FD_HASH_Rekisteröinti..numero': 'str',
    'TUPVA':'str',
    'KOODI':'str'
    }
    df = pd.read_csv(os.path.join(processed_THL_path, inpath), sep=',', encoding='latin-1', usecols=dtypes_hilmo.keys(), dtype=dtypes_hilmo)
    df.rename(columns={'FID': 'PATIENT_ID', 'FD_HASH_Rekisteröinti..numero': 'FD_HASH_CODE'}, inplace=True)
    #2. remove missing hash keys
    df = remove_missing_hash(df)
    #3. process data
    merged_data = pd.merge(doctor_data, df, on='FD_HASH_CODE', how='inner')
    merged_data['REGISTER'] = 'Diagnosis Hilmo'
    merged_data['DATE'] = pd.to_datetime(df['TUPVA'], format='%d.%m.%Y')
    merged_data.rename(columns={'KOODI': 'CODE'}, inplace=True)
    merged_data['PRIVATE']=0
    merged_data['PUBLIC']=0
    merged_data = merged_data[['DOCTOR_ID', 'PATIENT_ID', 'REGISTER', 'DATE', 'CODE','PRIVATE', 'PUBLIC']]
    #4. output to longitudinal file
    merged_data.to_csv(outpath, mode='a', header=False, index=False)

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
avohilmo_files = ['processed_avohilmo_20250218.csv']
hilmo_files = ['processed_hilmo_20250218.csv']
print('Found', len(kela_files), 'purchases files')
print('Found', len(reseptikeskus_files), 'prescription files')
print('Found', len(avohilmo_files), 'avohilmo files')
print('Found', len(hilmo_files), 'hilmo files')

# Multiprocessing tasks for longitudinal file generation
tasks = [
    (process_purchases, kela_files),
    (process_prescriptions, reseptikeskus_files),
    (process_diagnosis_avohilmo, avohilmo_files),
    (process_diagnosis_hilmo, hilmo_files)
]

with multiprocessing.Pool(processes=N_CPUs) as pool:

    output_file = f"{args.outdir}doctor_patient_longitudinal_{time.strftime('%Y%m%d')}.csv"
    # Prepare the output file with required columns
    pd.DataFrame(columns=['DOCTOR_ID', 'PATIENT_ID', 'REGISTER', 'DATE', 'CODE', 'PRIVATE', 'PUBLIC']).to_csv(output_file, index=False)
    print(f'printing to {output_file}')

    for func, files in tasks:
        start_time = time.time()
        for input_file in files:
            pool.apply(func, args=(input_file, doctor_data, output_file))
        end_time = time.time()
        print(f'Time taken for {func}: {end_time - start_time} seconds')
    pool.close()
    pool.join()