#### Info:
# avohilmo base files = 14 (contains accident+cause ICD10 codes)
# avohilmo diagnosis files = 2 
# hilmo base files = 5
# hilmo diagnosis files = 5

#### Libraries:
import time
import os
import re
import pandas as pd
import multiprocessing 
from datetime import datetime

#### Paths
THL_path = "/media/volume/Data/Data_THL_2698_14.02.00_2023/THL/"
avohilmo_base = [file for file in os.listdir(THL_path) if re.search(r'AH_\d', file) and '~lock' not in file]
avohilmo_diagnosis = [file for file in os.listdir(THL_path) if re.search(r'AH_pitkadiag_\d', file) and '~lock' not in file]
hilmo_base = [file for file in os.listdir(THL_path) if re.search(r'HILMO\d', file) and '~lock' not in file]
hilmo_diagnosis = [file for file in os.listdir(THL_path) if re.search(r'H_ICD10_\d', file) and '~lock' not in file]

today = datetime.today().strftime('%d%m%y')
outpath_avohilmo = f'/media/volume/Projects/DSGELabProject1/processed_avohilmo_{today}.csv'
outpath_hilmo = f'/media/volume/Projects/DSGELabProject1/processed_hilmo_{today}.csv'

#### Global variables
N_CPUs = 5

#### Functions
def remove_missing_hash(data):
    N0 = data.shape[0]
    data = data[(data['FD_HASH_Rekisteröinti..numero'].notna()) & (data['FD_HASH_Rekisteröinti..numero'] != "PUUTTUVA")]
    N1 = data.shape[0] / N0 * 100
    print('removing', round(100 - N1, 2), '% missing hash codes')
    return data

def merge_extra_avohilmo(base_data, inpath, outpath):
    dtypes = {'FID': 'str','AVOHILMOID':'int','ICD10':'str'}
    df = pd.read_csv(os.path.join(THL_path, inpath), sep=';', encoding='latin-1', usecols= dtypes.keys(), dtype=dtypes)
    merged_data = pd.merge(base_data, df, on='AVOHILMOID', how='inner')
    merged_data.to_csv(outpath, mode='a', header=False, index=False)

def merge_extra_hilmo(base_data, inpath, outpath):
    dtypes = {'FID': 'str','AVOHILMOID':'int','KOODI':'str','N':'int','KENTTA':str}
    df = pd.read_csv(os.path.join(THL_path, inpath), sep=';', encoding='latin-1', usecols= dtypes.keys(), dtype=dtypes)
    df = df[(df.N==0) & (df.KENTTA=='PDGO')] # filter only main diagnosis 
    merged_data = pd.merge(base_data, df, on='HILMOID', how='inner')
    merged_data.to_csv(outpath, mode='a', header=False, index=False)

#### Main:
for base_file in avohilmo_base:
    # 1. open file
    dtypes_avohilmo = {
    'FID': 'str',
    'FD_HASH_Rekisteröinti..numero': 'str',
    'AVOHILMOID':'int',
    'KAYNTI_ALKOI':'str',
    'TAPATURMATYYPPI':'str',
    'ULKOINEN_SYY':'str'
    }
    df = pd.read_csv(os.path.join(THL_path, base_file), sep=';', encoding='latin-1', usecols= dtypes_avohilmo.keys(), dtype=dtypes_avohilmo)
    #2. remove missing hash keys
    df = remove_missing_hash(df)
    # 3. merge diagnosis info (using AVOHILMOID)
    # - multiprocessing for speed 
    # - append to unique output file
    pd.DataFrame(columns=['FID', 'FD_HASH_Rekisteröinti', 'AVOHILMOID', 'KAYNTI_ALKOI', 'TAPATURMATYYPPI','ULKOINEN_SYY','ICD10']).to_csv(outpath_avohilmo, index=False)
    with multiprocessing.Pool(processes=N_CPUs) as pool:
        start_time = time.time()
        for extra in avohilmo_diagnosis:
            pool.apply_async(merge_extra_avohilmo, args=(df, extra, outpath_avohilmo))
        end_time = time.time()
        print(f'Time taken for {base_file}: {end_time - start_time} seconds')
    pool.close()
    pool.join()

for base_file in hilmo_base:
    # 1. open file
    dtypes_hilmo = {
    'FID': 'str',
    'FD_HASH_Rekisteröinti..numero': 'str',
    'HILMOID':'int',
    'TUPVA':'str'
    }
    df = pd.read_csv(os.path.join(THL_path, base_file), sep=';', encoding='latin-1', usecols= dtypes_hilmo.keys(), dtype=dtypes_hilmo)
    #2. remove missing hash keys
    df = remove_missing_hash(df)
    # 3. merge diagnosis info (using HILMOID)
    # - multiprocessing for speed 
    # - append to unique output file
    pd.DataFrame(columns=['FID', 'FD_HASH_Rekisteröinti', 'HILMOID', 'TUPVA','KOODI']).to_csv(outpath_hilmo, index=False)
    with multiprocessing.Pool(processes=N_CPUs) as pool:
        start_time = time.time()
        for extra in hilmo_diagnosis:
            pool.apply_async(merge_extra_hilmo, args=(df, extra, outpath_hilmo))
        end_time = time.time()
        print(f'Time taken for {base_file}: {end_time - start_time} seconds')
    pool.close()
    pool.join()
