#### Info
# This script takes all Combined files (Purcahse, Prescription & Diagnosis) and join them in a longitudinal format.
# Will also remove missing data

#### Libraries
import os
import time
import pandas as pd

#### Paths
ValviraFile= "/media/volume/Data/Data_THL_2698_14.02.00_2023/Valvira/FD_2698_Liite 1 THL_2698_14.02.00_2023.csv"
PurchaseFile = "/media/volume/Projects/DSGELabProject1/ProcessedData/AllConnectedPurchases_20250421.csv"
PrescriptionFile = "/media/volume/Projects/DSGELabProject1/ProcessedData/AllConnectedPrescriptions_20250421.csv"
DiagnosisFile = "/media/volume/Projects/DSGELabProject1/ProcessedData/AllConnectedDiagnosis_20250421.csv" 
OutFile = f"/media/volume/Projects/DSGELabProject1/ProcessedData/doctor_patient_longitudinal_{time.strftime('%Y%m%d')}.csv"

#### Global variables
CHUNK_SIZE = 1_000_000

##### Functions
def process_purchases(data):
    #1. remove rows with missing data
    data = data[data.DOCTOR_ID.notna() & data.DOCTOR_ID != ""]
    #3. prepare data for output
    data['REGISTER'] = 'Purchase'
    data.rename(columns={'PURCHASE_DATE':'DATE', 'ATC_CODE':'CODE'}, inplace=True)
    data['PRIVATE']=0
    data['PUBLIC']=0
    data['CITY']=""
    data = data[['DOCTOR_ID','PATIENT_ID','REGISTER','DATE','CODE','PRIVATE','PUBLIC','CITY']]
    return data 


def process_prescriptions(data):
    #1. remove rows with missing data
    data = data[data.DOCTOR_ID.notna() & data.DOCTOR_ID != ""]
    #3. prepare data for output
    data['REGISTER'] = 'Prescription'
    data.rename(columns={'PRESCRIPTION_DATE':'DATE', 'ATC_CODE':'CODE'}, inplace=True)
    data['PRIVATE'] = data['SECTOR'].apply(lambda x: 1 if x == '2' else 0)
    data['PUBLIC'] = data['SECTOR'].apply(lambda x: 1 if x == '1' else 0)
    data = data[['DOCTOR_ID','PATIENT_ID','REGISTER','DATE','CODE','PRIVATE','PUBLIC','CITY']]
    return data 


def process_diagnosis(data):
    #1. remove rows with missing data
    data = data[data.DOCTOR_ID.notna() & data.DOCTOR_ID != ""]
    #3. prepare data for output
    data.loc[data['SOURCE'] == 'Hilmo', 'REGISTER'] = 'Diagnosis Hilmo'
    data.loc[data['SOURCE'] == 'Avohilmo', 'REGISTER'] = 'Diagnosis Avohilmo'
    data.rename(columns={'DIAGNOSIS_DATE':'DATE', 'ICD10_CODE':'CODE'}, inplace=True)
    data['PRIVATE']=0
    data['PUBLIC']=0
    data['CITY']=""
    data = data[['DOCTOR_ID','PATIENT_ID','REGISTER','DATE','CODE','PRIVATE','PUBLIC','CITY']]
    return data 

#### Main ####
file_index = 0  
for file in [PurchaseFile, PrescriptionFile, DiagnosisFile]:
    if file == PurchaseFile:
        process_func = process_purchases
    elif file == PrescriptionFile:
        process_func = process_prescriptions
    elif file == DiagnosisFile:
        process_func = process_diagnosis

    print(f"Processing {file}")
    start_time = time.time()
    
    # Read the data in chunks
    chunk_index = 0  
    for chunk in pd.read_csv(file, chunksize=CHUNK_SIZE, sep=',', encoding='latin-1'):
        processed_data = process_func(chunk)
        
        # Append results to CSV
        mode = 'w' if file_index == 0 and chunk_index == 0 else 'a'
        header = True if file_index == 0 and chunk_index == 0 else False
        processed_data.to_csv(OutFile, mode=mode, header=header, index=False)
        
        chunk_index += 1  

    end_time = time.time()
    file_index += 1
    print(f"Finished processing in {end_time - start_time:.2f} seconds")