#### Info
# This script takes all Combined files (Purcahse, Prescription & Diagnosis) and join them in a longitudinal format.
# Will also remove missing data

#### Libraries
import os
import time
import pandas as pd

#### Paths
# using imputed prescriptions, therefore we will have info for the period: 1998 - 2022
# only using prescriptions and diagnosis, so we have a proxy for visit outcomes
PrescriptionFile = "/media/volume/Projects/DSGELabProject1/ProcessedData/imputed_prescriptions_20250501152849.csv.gz"
DiagnosisFile = "/media/volume/Projects/DSGELabProject1/ProcessedData/AllConnectedDiagnosis_20250528.csv" 
OutFile = f"/media/volume/Projects/DSGELabProject1/ProcessedData/doctor_patient_longitudinal_VISIT_{time.strftime('%Y%m%d')}.csv"

#### Global variables
CHUNK_SIZE = 1_000_000

##### Functions
def process_prescriptions(data):
    #1. remove rows with missing data
    data = data[(data.DOCTOR_ID.notna()) & (data.DOCTOR_ID != "")]
    #2. prepare data for output
    if not data.empty:
        data['REGISTER'] = 'Prescription'
        data.rename(columns={'PRESCRIPTION_DATE':'DATE'}, inplace=True)
        data = data[['DOCTOR_ID','PATIENT_ID','REGISTER','DATE','CODE']]
    return data 

def process_diagnosis(data):
    #1. remove rows with missing data
    data = data[(data.DOCTOR_ID.notna()) & (data.DOCTOR_ID != "")]
    #2. prepare data for output
    if not data.empty:
        data.loc[data['SOURCE'] == 'Hilmo', 'REGISTER'] = 'Diagnosis Hilmo'
        data.loc[data['SOURCE'] == 'Avohilmo', 'REGISTER'] = 'Diagnosis Avohilmo'
        data.rename(columns={'VISIT_DATE':'DATE', 'ICD10_CODE':'CODE'}, inplace=True)
        data = data[['DOCTOR_ID','PATIENT_ID','REGISTER','DATE','CODE']]
    return data

#### Main ####
file_index = 0  
for file in [PrescriptionFile, DiagnosisFile]:
    if file == PrescriptionFile:
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