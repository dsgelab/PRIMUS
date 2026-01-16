# Info
# This script takes all (yearly) files from the Kela register and joins them to the respective doctor, and then push to one unique CSV file

#### Libraries
import os
import pandas as pd
import time

#### Paths
KelaDir = "/media/volume/Data/Data_THL_2698_14.02.00_2023/Kela/"
kela_files = [file for file in os.listdir(KelaDir) if 'LAAKEOSTOT' in file and '~lock' not in file]
Valvira_path = "/media/volume/Data/Data_THL_2698_14.02.00_2023/Valvira/FD_2698_Liite 1 THL_2698_14.02.00_2023.csv"
OutDir = "/media/volume/Projects/DSGELabProject1/ProcessedData/"
output_csv = os.path.join(OutDir, 'AllConnectedPurchases_20250421.csv')

#### Global variables
CHUNK_SIZE = 1_000_000
DTYPES  = {
    'FID': 'str',
    'FD_HASH_Rekisteröinti..numero': 'str',
    'TOIMITUS_PV': 'str',
    'ATC5_KOODI': 'str',
    'LAAKEMAARAYS_KIRJOITUS_PV': 'str',
}

#### Main

# load doctors information 
doctor_data = pd.read_csv(Valvira_path, sep=';', encoding='latin1')
doctor_data.rename(columns={'FID': 'DOCTOR_ID', 'FD_HASH_Rekisteröinti..numero': 'FD_HASH_CODE'}, inplace=True)
doctor_data = doctor_data[['DOCTOR_ID', 'FD_HASH_CODE']]  
doctor_data.drop_duplicates(inplace=True) 

# Connect all purchases to doctors
for i, file in enumerate(kela_files):

    file_path = os.path.join(KelaDir, file)
    print(f"Processing file: {file}")
    start_time = time.time()
    
    for chunk_index, chunk in enumerate(pd.read_csv(file_path, chunksize=CHUNK_SIZE, sep=';', encoding='latin-1', usecols=DTYPES.keys(), dtype=DTYPES)):

        processed_data = chunk.rename(columns={'FID':'PATIENT_ID', 'FD_HASH_Rekisteröinti..numero':'FD_HASH_CODE', 'ATC5_KOODI':'ATC_CODE'})
        processed_data['PURCHASE_DATE'] = pd.to_datetime(processed_data['TOIMITUS_PV'], format='%Y-%m-%d')
        processed_data['PRESCRIPTION_DATE'] = pd.to_datetime(processed_data['LAAKEMAARAYS_KIRJOITUS_PV'], format='%Y-%m-%d')
        processed_data = processed_data[['PATIENT_ID', 'PURCHASE_DATE', 'ATC_CODE', 'PRESCRIPTION_DATE', 'FD_HASH_CODE']]
        processed_data = pd.merge(processed_data, doctor_data, on='FD_HASH_CODE', how='left')

        # Append results to CSV
        mode = 'w' if i == 0 and chunk_index == 0 else 'a'
        header = True if i == 0 and chunk_index == 0 else False
        processed_data.to_csv(output_csv, mode=mode, header=header, index=False)
    
    end_time = time.time()
    print(f"Finished processing file in {end_time - start_time:.2f} seconds")