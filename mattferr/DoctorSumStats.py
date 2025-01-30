# LAST UPDATED: 2025-01-29

# RAM = 125 Gb 
# CPU = 32 cores

# list of extracted info for each doctor
# - number of total patients
# - number of unique patients
# - number of prescriptions
# - number of diagnosis
# + also calculate the average number of prescriptions and diagnosis per patient

import argparse
import os
import re
import time
import pandas as pd
from multiprocessing import Pool

# Functions

dtypes = {
    'FID': 'str',
    'FD_HASH_Rekisteröinti..numero': 'str'
}

def remove_missing_hash(data):
    N0 = data.shape[0]
    data = data[(data['FD_HASH_CODE'].notna()) & (data['FD_HASH_CODE'] != "PUUTTUVA")]
    N1 = data.shape[0] / N0 * 100
    print('removing', round(100 - N1, 2), '% missing hash codes')
    return data

def extract_unique_IDs(data, ID_LIST):
    ids = data.groupby('DOCTOR_ID')['PATIENT_ID'].apply(list).to_dict()
    ids = {doctor: ids.get(doctor, []) for doctor in ID_LIST}
    return ids

def extract_prescriptions(data, ID_LIST):
    N = data.groupby('DOCTOR_ID')['FD_HASH_CODE'].size()
    N = N.reindex(ID_LIST, fill_value=0)
    return N

def extract_diagnosis(data, ID_LIST):
    N = data.groupby('DOCTOR_ID')['FD_HASH_CODE'].size()
    N = N.reindex(ID_LIST, fill_value=0)
    return N

def process_prescriptions(file, Kela_path, doctor_data, ID_LIST):
    print('Processing file: ' + file)
    t0 = time.time()
    prescription_data = pd.read_csv(os.path.join(Kela_path, file), sep=';', usecols=['FID', 'FD_HASH_Rekisteröinti..numero'], dtype=dtypes, encoding='latin1')
    prescription_data.rename(columns={'FID': 'PATIENT_ID', 'FD_HASH_Rekisteröinti..numero': 'FD_HASH_CODE'}, inplace=True)
    prescription_data = remove_missing_hash(prescription_data)
    merged_data = pd.merge(doctor_data, prescription_data, on='FD_HASH_CODE', how='inner')
    unique_ids = extract_unique_IDs(merged_data, ID_LIST)
    n_prescriptions = extract_prescriptions(merged_data, ID_LIST)
    t1 = time.time()
    print('Time taken: ' + str(round(t1 - t0, 2)) + ' seconds')
    return unique_ids, n_prescriptions

def process_diagnosis(file, THL_path, doctor_data, ID_LIST):
    print('Processing file: ' + file)
    t0 = time.time()
    diagnosis_data = pd.read_csv(os.path.join(THL_path, file), sep=';', usecols=['FID', 'FD_HASH_Rekisteröinti..numero'], dtype=dtypes, encoding='latin1')
    diagnosis_data.rename(columns={'FID': 'PATIENT_ID', 'FD_HASH_Rekisteröinti..numero': 'FD_HASH_CODE'}, inplace=True)
    diagnosis_data = remove_missing_hash(diagnosis_data)
    merged_data = pd.merge(doctor_data, diagnosis_data, on='FD_HASH_CODE', how='inner')
    unique_ids = extract_unique_IDs(merged_data, ID_LIST)
    n_diagnosis = extract_diagnosis(merged_data, ID_LIST)
    t1 = time.time()
    print('Time taken: ' + str(round(t1 - t0, 2)) + ' seconds')
    return unique_ids, n_diagnosis

# Paths
Valvira_path = "/media/volume/Data/Data_THL_2698_14.02.00_2023/Valvira/FD_2698_Liite 1 THL_2698_14.02.00_2023.csv"
Kela_path = "/media/volume/Data/Data_THL_2698_14.02.00_2023/Kela/"
THL_path = "/media/volume/Data/Data_THL_2698_14.02.00_2023/THL/"

# Arguments
parser = argparse.ArgumentParser()
parser.add_argument('--outpath', type=str, default='/media/volume/Projects/mattferr/doctor_info.csv',help='file path where the results want to be saved')
parser.add_argument('--doctor_list', type=str, default=False, help='file path to list of doctor IDs, if not provided, will use all doctors')
args = parser.parse_args()

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
doctor_data = doctor_data[['DOCTOR_ID', 'FD_HASH_CODE']]  # select only important columns
doctor_data.drop_duplicates(inplace=True)

# Prepare output dataframe
summary_stats = doctor_data[['DOCTOR_ID']].copy()
summary_stats.set_index('DOCTOR_ID', inplace=True, drop=False)
doc_index_map = {
    row['DOCTOR_ID']: i
    for i, row in summary_stats.iterrows()
}
# Initialize columns
# 1) UNIQUE_PATIENTS: create a new set for each row (avoid using [set()] * n)
summary_stats['UNIQUE_PATIENTS'] = [set() for _ in range(len(summary_stats))]
# 2) Numeric columns default to 0
summary_stats['N_PRESCRIPTIONS'] = 0
summary_stats['N_DIAGNOSIS'] = 0

# Get list of files to process
prescription_files = [file for file in os.listdir(Kela_path) if 'LAAKEOSTOT' in file and '~lock' not in file]
avohilmo_files = [file for file in os.listdir(THL_path) if re.search(r'AH_\d', file) and '~lock' not in file]
avohilmo_files1 = avohilmo_files[:len(avohilmo_files)//2]
avohilmo_files2 = avohilmo_files[len(avohilmo_files)//2:]
hilmo_files = [file for file in os.listdir(THL_path) if re.search(r'HILMO\d', file) and '~lock' not in file]
print('Found', len(prescription_files), 'prescription files')
print('Found', len(avohilmo_files), 'avohilmo files')
print('Found', len(hilmo_files), 'hilmo files')

# Use multiprocessing to process files in parallel 
N_CPUs = 6

# PROCESS KELA FILES
print('Processing Kela files')
with Pool(processes=N_CPUs) as pool:  
    results = pool.starmap(process_prescriptions, [(file, Kela_path, doctor_data, ID_LIST) for file in prescription_files])
    pool.close()
    pool.join()
# Aggregate results from all processes
print('Aggregating results')
for unique_ids, n_prescriptions in results: 
    for doctor_id, patients in unique_ids.items():
        idx = doc_index_map.get(doctor_id)
        summary_stats.at[idx, 'UNIQUE_PATIENTS'].update(patients)
    summary_stats['N_PRESCRIPTIONS'] = summary_stats['N_PRESCRIPTIONS'].add(n_prescriptions, fill_value=0)

# PROCESS THL FILES
print('Processing Avohilmo pt.1')
with Pool(processes=N_CPUs) as pool:  
    results = pool.starmap(process_diagnosis, [(file, THL_path, doctor_data, ID_LIST) for file in avohilmo_files1])
    pool.close()
    pool.join()
# Aggregate results from all processes
print('Aggregating results')
for unique_ids, n_diagnosis in results:
    for doctor_id, patients in unique_ids.items():
        idx = doc_index_map.get(doctor_id)
        summary_stats.at[idx, 'UNIQUE_PATIENTS'].update(patients)
    summary_stats['N_DIAGNOSIS'] = summary_stats['N_DIAGNOSIS'].add(n_diagnosis, fill_value=0)

print('Processing Avohilmo pt.2')
with Pool(processes=N_CPUs) as pool:  
    results = pool.starmap(process_diagnosis, [(file, THL_path, doctor_data, ID_LIST) for file in avohilmo_files2])
    pool.close()
    pool.join()
# Aggregate results from all processes
print('Aggregating results')
for unique_ids, n_diagnosis in results:
    for doctor_id, patients in unique_ids.items():
        idx = doc_index_map.get(doctor_id)
        summary_stats.at[idx, 'UNIQUE_PATIENTS'].update(patients)
    summary_stats['N_DIAGNOSIS'] = summary_stats['N_DIAGNOSIS'].add(n_diagnosis, fill_value=0)

print('Processing Hilmo')
with Pool(processes=N_CPUs) as pool:  
    results = pool.starmap(process_diagnosis, [(file, THL_path, doctor_data, ID_LIST) for file in hilmo_files])
    pool.close()
    pool.join()
# Aggregate results from all processes
print('Aggregating results')
for unique_ids, n_diagnosis in results:
    for doctor_id, patients in unique_ids.items():
        idx = doc_index_map.get(doctor_id)
        summary_stats.at[idx, 'UNIQUE_PATIENTS'].update(patients)
    summary_stats['N_DIAGNOSIS'] = summary_stats['N_DIAGNOSIS'].add(n_diagnosis, fill_value=0)

# Finalize results
print('finalizing results')
summary_stats['N_UNIQUE_PATIENTS'] = summary_stats['UNIQUE_PATIENTS'].apply(lambda x: len(x))
summary_stats.drop(columns=['UNIQUE_PATIENTS'], inplace=True)
summary_stats['AVG_PRESCRIPTIONS'] = summary_stats['N_PRESCRIPTIONS'] / summary_stats['N_UNIQUE_PATIENTS']
summary_stats['AVG_DIAGNOSIS'] = summary_stats['N_DIAGNOSIS'] / summary_stats['N_UNIQUE_PATIENTS']

# Save results
summary_stats.to_csv(args.outpath, index=False)
