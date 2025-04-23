# Info
# This script takes all the pre-process hidiagnosis files (hilmo/avohilmo) and joins them to the respective doctor, and then push to one unique CSV file

#### Libraries
import os
import pandas as pd
import time

#### Paths
InDir = "/media/volume/Projects/DSGELabProject1/ProcessedData/"
hilmo_file = "/media/volume/Projects/DSGELabProject1/ProcessedData/processed_hilmo_20250218.csv"
avohilmo_file = "/media/volume/Projects/DSGELabProject1/ProcessedData/processed_avohilmo_20250218.csv"
Valvira_path = "/media/volume/Data/Data_THL_2698_14.02.00_2023/Valvira/FD_2698_Liite 1 THL_2698_14.02.00_2023.csv"
OutDir = "/media/volume/Projects/DSGELabProject1/ProcessedData/"
output_csv = os.path.join(OutDir, 'AllConnectedDiagnosis_20250421.csv')

#### Global variables
CHUNK_SIZE = 1_000_000
DTYPES_HILMO  = {
    'FID': 'str',
    'FD_HASH_Rekisteröinti..numero': 'str',
    'TUPVA':'str',
    'KOODI':'str'
}

DTYPES_AVOHILMO  = {
    'FID': 'str',
    'FD_HASH_Rekisteröinti..numero': 'str',
    'KAYNTI_ALKOI':'str',
    'ICD10':'str'
}


#### Main

# load doctors information 
doctor_data = pd.read_csv(Valvira_path, sep=';', encoding='latin1')
doctor_data.rename(columns={'FID': 'DOCTOR_ID', 'FD_HASH_Rekisteröinti..numero': 'FD_HASH_CODE'}, inplace=True)
doctor_data = doctor_data[['DOCTOR_ID', 'FD_HASH_CODE']]  
doctor_data.drop_duplicates(inplace=True) 

# Connect all diagnosis to doctors
print(f"Processing Hilmo")
start_time = time.time()
    
for chunk_index, chunk in enumerate(pd.read_csv(hilmo_file, chunksize=CHUNK_SIZE, sep=',', encoding='latin-1', usecols=DTYPES_HILMO.keys(), dtype=DTYPES_HILMO)):

    processed_data = chunk.rename(columns={'FID':'PATIENT_ID', 'FD_HASH_Rekisteröinti..numero':'FD_HASH_CODE', 'KOODI':'ICD10_CODE'})
    processed_data['DIAGNOSIS_DATE'] = pd.to_datetime(processed_data['TUPVA'], format='%d.%m.%Y')
    processed_data['SOURCE'] = 'Hilmo'
    processed_data = processed_data[['PATIENT_ID','DIAGNOSIS_DATE','ICD10_CODE','SOURCE','FD_HASH_CODE']]
    processed_data = pd.merge(processed_data, doctor_data, on='FD_HASH_CODE', how='left')

    # Append results to CSV
    mode = 'w' if chunk_index == 0 else 'a'
    header = True if chunk_index == 0 else False
    processed_data.to_csv(output_csv, mode=mode, header=header, index=False)

end_time = time.time()
print(f"Finished processing file in {end_time - start_time:.2f} seconds")

print(f"Processing AvoHilmo")
start_time = time.time()
    
for chunk_index, chunk in enumerate(pd.read_csv(avohilmo_file, chunksize=CHUNK_SIZE, sep=',', encoding='latin-1', usecols=DTYPES_AVOHILMO.keys(), dtype=DTYPES_AVOHILMO)):

    processed_data = chunk.rename(columns={'FID':'PATIENT_ID', 'FD_HASH_Rekisteröinti..numero':'FD_HASH_CODE', 'ICD10':'ICD10_CODE'})
    processed_data['DIAGNOSIS_DATE'] = pd.to_datetime(processed_data['KAYNTI_ALKOI'], format='%d.%m.%Y %H:%M')
    processed_data['SOURCE'] = 'Avohilmo'
    processed_data = processed_data[['PATIENT_ID','DIAGNOSIS_DATE','ICD10_CODE','SOURCE','FD_HASH_CODE']]
    processed_data = pd.merge(processed_data, doctor_data, on='FD_HASH_CODE', how='left')

    # Append results to CSV
    mode = 'w' if chunk_index == 0 else 'a'
    header = True if chunk_index == 0 else False
    processed_data.to_csv(output_csv, mode=mode, header=header, index=False)

end_time = time.time()
print(f"Finished processing file in {end_time - start_time:.2f} seconds")