# Info
# This script takes all (yearly) files from the Kela register and joins them in one unique file which is tan saved as CSV and Parquet
# No doctor is connected, keeping all available information as it is

#### Libraries
import os
import pandas as pd
import pyarrow as pa
import time
import pyarrow.parquet as pq

#### Paths
KelaDir = "/media/volume/Data/Data_THL_2698_14.02.00_2023/Kela/"
OutDir = "/media/volume/Projects/DSGELabProject1/ProcessedData/"
output_csv = os.path.join(OutDir, 'AllPurchases_20250417.csv')
output_parquet = os.path.join(OutDir, 'AllPurchases_20250417.parquet')
kela_files = [file for file in os.listdir(KelaDir) if 'LAAKEOSTOT' in file and '~lock' not in file]

#### Global variables
CHUNK_SIZE = 1_000_000
DTYPES  = {
    'FID': 'str',
    'FD_HASH_Rekisteröinti..numero': 'str',
    'TOIMITUS_PV': 'str',
    'ATC5_KOODI': 'str'
}

#### Main
for i, file in enumerate(kela_files):
    file_path = os.path.join(KelaDir, file)
    print(f"Processing file: {file}")
    start_time = time.time()
    
    for chunk_index, chunk in enumerate(pd.read_csv(file_path, chunksize=CHUNK_SIZE, sep=';', encoding='latin-1', usecols=DTYPES.keys(), dtype=DTYPES)):

        processed_data = chunk.rename(columns={'FID':'PATIENT_ID', 'FD_HASH_Rekisteröinti..numero':'FD_HASH_CODE', 'ATC5_KOODI':'CODE'})
        processed_data['DATE'] = pd.to_datetime(processed_data['TOIMITUS_PV'], format='%Y-%m-%d')
        processed_data = processed_data[['PATIENT_ID', 'DATE', 'CODE', 'FD_HASH_CODE']]
        
        # Append results to CSV
        mode = 'w' if i == 0 and chunk_index == 0 else 'a'
        header = True if i == 0 and chunk_index == 0 else False
        processed_data.to_csv(output_csv, mode=mode, header=header, index=False)
        
        # For Parquet, collect all processed chunks and save at the end
        if i == 0 and chunk_index == 0:
            all_processed = processed_data
        else:
            all_processed = pd.concat([all_processed, processed_data])
    
    end_time = time.time()
    print(f"Finished processing file in {end_time - start_time:.2f} seconds")

# Save to Parquet
table = pa.Table.from_pandas(all_processed)
pq.write_table(table, output_parquet)