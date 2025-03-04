#### Info:
# This script processes the THL avohilmo and hilmo files in order to add the ICD10 code of the diagnosis to the main THL files.

# contrary to ProcessDiagnosisTHL.py, this script does not remove missing hash codes anw will be used as a reference for diagnosis info.
# the script uses multiprocessing to speed up the process

#### Libraries:
import time
import os
import re
import pandas as pd
import multiprocessing 
import argparse

##### Arguments
parser = argparse.ArgumentParser()
parser.add_argument('--id_list', type=str, default=False, help='file path to list of IDs, if not provided, will use everyone')
parser.add_argument('--outdir', type=str, default='/media/volume/Projects/DSGELabProject1/',help='directory where the results want to be saved')
args = parser.parse_args()

print("using the following arguments: ")
print(args)

#### Paths
THL_path = "/media/volume/Data/Data_THL_2698_14.02.00_2023/THL/"
avohilmo_base = [file for file in os.listdir(THL_path) if re.search(r'AH_\d', file) and '~lock' not in file]
avohilmo_diagnosis = [file for file in os.listdir(THL_path) if re.search(r'AH_pitkadiag_\d', file) and '~lock' not in file]
hilmo_base = [file for file in os.listdir(THL_path) if re.search(r'HILMO\d', file) and '~lock' not in file]
hilmo_diagnosis = [file for file in os.listdir(THL_path) if re.search(r'H_ICD10_\d', file) and '~lock' not in file]

outpath_avohilmo = f"{args.outdir}processed_avohilmo_v2_{time.strftime('%Y%m%d')}.csv"
outpath_hilmo = f"{args.outdir}processed_hilmo_v2_{time.strftime('%Y%m%d')}.csv"

#### Global variables
N_CPUs = 5

#### Functions
def merge_extra_avohilmo(base_data, inpath, outpath):
    dtypes = {'FID': 'str','AVOHILMOID':'int','JARJESTYS':'int','ICD10':'str'}
    df = pd.read_csv(os.path.join(THL_path, inpath), sep=';', encoding='latin-1', usecols= dtypes.keys(), dtype=dtypes)
    df = df[df.JARJESTYS==0] # filter only main diagnosis
    merged_data = pd.merge(base_data, df, on=['FID', 'AVOHILMOID'], how='inner')
    if not merged_data.empty:
        merged_data = merged_data[['FID','KAYNTI_ALKOI','ICD10']]
        merged_data.rename(columns={'FID': 'PATIENT_ID', 'KAYNTI_ALKOI': 'DATE', 'ICD10': 'CODE'}, inplace=True)
        merged_data.to_csv(outpath, mode='a', header=False, index=False)

def merge_extra_hilmo(base_data, inpath, outpath):
    dtypes = {'FID': 'str','HILMOID':'int','KOODI':'str','N':'int','KENTTA':'str'}
    df = pd.read_csv(os.path.join(THL_path, inpath), sep=';', encoding='latin-1', usecols= dtypes.keys(), dtype=dtypes)
    df = df[(df.N==0) & (df.KENTTA=='PDGO')] # filter only main diagnosis 
    merged_data = pd.merge(base_data, df, on=['FID', 'HILMOID'], how='inner')
    if not merged_data.empty:
        merged_data = merged_data[['FID','TUPVA', 'KOODI']]
        merged_data.rename(columns={'FID': 'PATIENT_ID', 'TUPVA': 'DATE', 'KOODI': 'CODE'}, inplace=True)
        merged_data.to_csv(outpath, mode='a', header=False, index=False)

#### Main:
pd.DataFrame(columns=['PATIENT_ID','DATE', 'CODE']).to_csv(outpath_avohilmo, encoding='latin-1', index=False)
for base_file in avohilmo_base:
    start_time = time.time()
    # 1. open file
    dtypes_avohilmo = {
    'FID': 'str',
    'AVOHILMOID':'int',
    'KAYNTI_ALKOI':'str',
    }
    df = pd.read_csv(os.path.join(THL_path, base_file), sep=';', encoding='latin-1', usecols= dtypes_avohilmo.keys(), dtype=dtypes_avohilmo)
    # 2. filter IDs if required
    if args.id_list:
        with open(args.id_list, 'r') as file:
            ID_LIST = [line.strip() for line in file.readlines()]
        df = df[df['FID'].isin(ID_LIST)]
    # 3. merge diagnosis info (using AVOHILMOID)
    # - multiprocessing for speed 
    # - append to unique output file
    with multiprocessing.Pool(processes=N_CPUs) as pool:
        for extra in avohilmo_diagnosis:
            pool.apply(merge_extra_avohilmo, args=(df, extra, outpath_avohilmo))
    pool.close()
    end_time = time.time()
    print(f'Time taken for {base_file}: {end_time - start_time} seconds')

pd.DataFrame(columns=['PATIENT_ID','DATE', 'CODE']).to_csv(outpath_hilmo, encoding='latin-1', index=False)
for base_file in hilmo_base:
    start_time = time.time()
    # 1. open file
    dtypes_hilmo = {
    'FID': 'str',
    'HILMOID':'int',
    'TUPVA':'str'
    }
    df = pd.read_csv(os.path.join(THL_path, base_file), sep=';', encoding='latin-1', usecols= dtypes_hilmo.keys(), dtype=dtypes_hilmo)
    # 2. filter IDs if required
    if args.id_list:
        with open(args.id_list, 'r') as file:
            ID_LIST = [line.strip() for line in file.readlines()]
        df = df[df['FID'].isin(ID_LIST)]
    # 3. merge diagnosis info (using HILMOID)
    # - multiprocessing for speed 
    # - append to unique output file
    with multiprocessing.Pool(processes=N_CPUs) as pool:
        for extra in hilmo_diagnosis:
            pool.apply(merge_extra_hilmo, args=(df, extra, outpath_hilmo))
    pool.close()
    end_time = time.time()
    print(f'Time taken for {base_file}: {end_time - start_time} seconds')
