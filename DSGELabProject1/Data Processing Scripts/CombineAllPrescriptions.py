# Info
# This script takes all (yearly) files from the Reseptikeskus register and joins them to the respective doctor, and then push to one unique CSV file

#### Libraries
import os
import pandas as pd
import time

#### Paths
InDir = "/media/volume/Projects/DSGELabProject1/ProcessedData/CleanedPrescriptions/"
reseptikeskus_files = [file for file in os.listdir(InDir)]
Valvira_path = "/media/volume/Data/Data_THL_2698_14.02.00_2023/Valvira/FD_2698_Liite 1 THL_2698_14.02.00_2023.csv"
OutDir = "/media/volume/Projects/DSGELabProject1/ProcessedData/"
output_csv = os.path.join(OutDir, 'AllConnectedPrescriptions_20250421.csv')
log_filepath = "/media/volume/Projects/DSGELabProject1/Logs/FaultyLinesPrescriptions_20250421.txt"

#### Global variables
CHUNK_SIZE = 1_000_000

#### Main

# load doctors information 
doctor_data = pd.read_csv(Valvira_path, sep=';', encoding='latin1')
doctor_data.rename(columns={'FID': 'DOCTOR_ID', 'FD_HASH_Rekisteröinti..numero': 'FD_HASH_CODE'}, inplace=True)
doctor_data = doctor_data[['DOCTOR_ID', 'FD_HASH_CODE']]  
doctor_data.drop_duplicates(inplace=True) 

# Connect all prescriptions to doctors
for i, file in enumerate(reseptikeskus_files):

    file_path = os.path.join(InDir, file)
    print(f"Processing file: {file}")
    start_time = time.time()
    file_faulty_dates = 0

    # If file contains '2018' in its name, adjust DTYPES accordingly
    if '2018' in file:
        DTYPES = {
            'FID': 'str',
            'FD_HASH_Rekisterointinumero': 'str',
            'DATE_PK': 'str',
            'ATC_CODE': 'str',
            'SECTOR': 'str',
            'CITY': 'str',
        }
    if '2018' in file:
        for chunk_index, chunk in enumerate(pd.read_csv(file_path, chunksize=CHUNK_SIZE, sep=';', encoding='latin-1', usecols=DTYPES.keys(), dtype=DTYPES)):
            processed_data = chunk.rename(columns={'FID':'PATIENT_ID', 'FD_HASH_Rekisterointinumero':'FD_HASH_CODE'})
            valid_dates_before = chunk['DATE_PK'].count()
            processed_data['PRESCRIPTION_DATE'] = pd.to_datetime(processed_data['DATE_PK'], format='%Y%m%d', errors='coerce')
            valid_dates_after = processed_data['PRESCRIPTION_DATE'].count()
            file_faulty_dates += valid_dates_before - valid_dates_after
            processed_data = processed_data[['PATIENT_ID', 'PRESCRIPTION_DATE', 'ATC_CODE', 'SECTOR', 'CITY', 'FD_HASH_CODE']]
            processed_data = pd.merge(processed_data, doctor_data, on='FD_HASH_CODE', how='left')

            # Append results to CSV
            mode = 'w' if i == 0 and chunk_index == 0 else 'a'
            header = True if i == 0 and chunk_index == 0 else False
            processed_data.to_csv(output_csv, mode=mode, header=header, index=False)

    else:
        DTYPES = {
            'FID': 'str',
            'FD_HASH_Rekisteröinti..numero': 'str',
            'DATE_PK': 'str',
            'ATC_CODE': 'str',
            'SECTOR': 'str',
            'CITY': 'str'
        }
        for chunk_index, chunk in enumerate(pd.read_csv(file_path, chunksize=CHUNK_SIZE, sep=';', encoding='latin-1', usecols=DTYPES.keys(), dtype=DTYPES)):

            processed_data = chunk.rename(columns={'FID':'PATIENT_ID', 'FD_HASH_Rekisteröinti..numero':'FD_HASH_CODE'})
            valid_dates_before = chunk['DATE_PK'].count()
            processed_data['PRESCRIPTION_DATE'] = pd.to_datetime(processed_data['DATE_PK'], format='%Y%m%d', errors='coerce')
            valid_dates_after = processed_data['PRESCRIPTION_DATE'].count()
            file_faulty_dates += valid_dates_before - valid_dates_after
            processed_data = processed_data[['PATIENT_ID', 'PRESCRIPTION_DATE', 'ATC_CODE', 'SECTOR', 'CITY', 'FD_HASH_CODE']]
            processed_data = pd.merge(processed_data, doctor_data, on='FD_HASH_CODE', how='left')

            # Append results to CSV
            mode = 'w' if i == 0 and chunk_index == 0 else 'a'
            header = True if i == 0 and chunk_index == 0 else False
            processed_data.to_csv(output_csv, mode=mode, header=header, index=False)

    end_time = time.time()
    print(f"Finished processing file in: {end_time - start_time:.2f} seconds")
    print(f"Faulty dates found: {file_faulty_dates}")
