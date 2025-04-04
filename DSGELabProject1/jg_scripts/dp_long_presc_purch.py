#### Info
# machine stats: RAM = 125 Gb, CPU = 32 cores

# This script takes as input 2 different types of files: Kela purchases, Reseptikeskus prescriptions.

# This script processes each file type separately and merges them with the doctor data to create a longitudinal file
# which contains information about each doctor patient interaction 

# The script uses multiprocessing to speed up the process.
# The script takes around X hours to process all the files and create the longitudinal file.


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
parser.add_argument('--doctor_list', type=str, default='/media/volume/Projects/DSGELabProject1/doctors_20250220.csv', help='file path to list of doctor IDs, if not provided, will use all doctors')
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
    'LAKKEMAARAYS_KIRJOITUS_PV':'str',
    'TOIMITUS_PV':'str',
    'ATC5_KOODI':'str'
    }
    df = pd.read_csv(os.path.join(Kela_path, inpath), sep=';', encoding='latin-1', usecols=dtypes_kela.keys(), dtype=dtypes_kela)
    df.rename(columns={'FID': 'PATIENT_ID', 'FD_HASH_Rekisteröinti..numero': 'FD_HASH_CODE',
                       'LAKKEMAARAYS_KIRJOITUS_PV': 'PRESCRIPTION_DATE'}, inplace=True)
    #2. remove missing hash keys
    df = remove_missing_hash(df)
    #3. process data
    merged_data = pd.merge(doctor_data, df, on='FD_HASH_CODE', how='inner')
    merged_data['REGISTER'] = 'Purchase'
    merged_data['DATE'] = pd.to_datetime(df['TOIMITUS_PV'], format='%Y-%m-%d')
    merged_data['PRESCRIPTION_DATE'] = pd.to_datetime(df['PRESCRIPTION_DATE'], format='%Y-%m-%d')
    merged_data.rename(columns={'ATC5_KOODI': 'CODE'}, inplace=True)
    merged_data['PRIVATE']=0
    merged_data['PUBLIC']=0
    merged_data = merged_data[['DOCTOR_ID', 'PATIENT_ID', 'REGISTER', 'DATE', 
                               'PRESCRIPTION_DATE', 'CODE', 'PRIVATE', 'PUBLIC']]
    
    # # Filter dates: extract only rows where DATE is between 2014 and 2017
    # start_date = pd.to_datetime("2014-01-01")
    # end_date = pd.to_datetime("2017-12-31")
    # merged_data = merged_data[(merged_data['DATE'] >= start_date) & (merged_data['DATE'] <= end_date)]
    
    
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
    merged_data['PRESCRIPTION_DATE'] = pd.to_datetime(df['DATE_PK'], format='%Y%m%d') # duplicate row to merge both dfs
    merged_data.rename(columns={'ATC_CODE': 'CODE'}, inplace=True)
    merged_data['PRIVATE'] = df['SECTOR'].apply(lambda x: 1 if x == '2' else 0)
    merged_data['PUBLIC'] = df['SECTOR'].apply(lambda x: 1 if x == '1' else 0)
    merged_data = merged_data[['DOCTOR_ID', 'PATIENT_ID', 'REGISTER', 'DATE', 
                               'PRESCRIPTION_DATE', 'CODE', 'PRIVATE', 'PUBLIC']]

    # # Filter dates: extract only rows where DATE is between 2014 and 2017
    # start_date = pd.to_datetime("2014-01-01")
    # end_date = pd.to_datetime("2017-12-31")
    # merged_data = merged_data[(merged_data['DATE'] >= start_date) & (merged_data['DATE'] <= end_date)]
    
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
print('Found', len(kela_files), 'purchases files')
print('Found', len(reseptikeskus_files), 'prescription files')

# Multiprocessing tasks for longitudinal file generation
tasks = [
    (process_purchases, kela_files),
    (process_prescriptions, reseptikeskus_files)
]

with multiprocessing.Pool(processes=N_CPUs) as pool:

    output_file = f"{args.outdir}doctor_patient_prescpurch_{time.strftime('%Y%m%d')}.csv"
    # Prepare the output file with required columns
    pd.DataFrame(columns=['DOCTOR_ID', 'PATIENT_ID', 'REGISTER', 'DATE', 
                          'PRESCRIPTION_DATE', 'CODE', 'PRIVATE', 'PUBLIC']).to_csv(output_file, index=False)
    print(f'printing to {output_file}')

    for func, files in tasks:
        start_time = time.time()
        for input_file in files:
            pool.apply(func, args=(input_file, doctor_data, output_file))
        end_time = time.time()
        print(f'Time taken for {func}: {end_time - start_time} seconds')
    pool.close()
    pool.join()
