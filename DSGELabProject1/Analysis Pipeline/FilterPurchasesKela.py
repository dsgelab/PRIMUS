#### Info:
# This script processes the Kela purchase files in order extract info about the list of doctors.
# the script uses multiprocessing to speed up the process

# INPUT:
# - id_list: file path to list of doctor IDs to be used
# - outdir: directory where the results want to be saved
# OUTPUT:
# - filtered_purchases_kela file

#### Libraries:
import time
import os
import re
import pandas as pd
import multiprocessing 
import argparse

##### Arguments
parser = argparse.ArgumentParser()
parser.add_argument('--id_list', type=str, default=False, help='file path to list of IDs')
parser.add_argument('--outdir', type=str, default='/media/volume/Projects/DSGELabProject1',help='directory where the results want to be saved')
args = parser.parse_args()

print("using the following arguments: ")
print(args)

#### Paths
Kela_path = "/media/volume/Data/Data_THL_2698_14.02.00_2023/Kela/"
Kela_files = [file for file in os.listdir(Kela_path) if 'LAAKEOSTOT' in file and '~lock' not in file]
outpath = f"{args.outdir}/filtered_purchases_kela_{time.strftime('%Y%m%d')}.csv"

#### Global variables
N_CPUs = 5

#### Functions
def process_purchases(inpath, outpath):
    #1. read file, only required columns, and rename
    print('processing: ', inpath)
    dtypes_kela = {
    'FID':'str',
    'TOIMITUS_PV':'str',
    'ATC5_KOODI':'str'
    }
    df = pd.read_csv(os.path.join(Kela_path, inpath), sep=';', encoding='latin-1', usecols=dtypes_kela.keys(), dtype=dtypes_kela)
    df.rename(columns={'FID': 'PATIENT_ID', 'TOIMITUS_PV': 'DATE','ATC5_KOODI':'CODE'}, inplace=True)
    df['DATE'] = pd.to_datetime(df['DATE'], format='%Y-%m-%d', errors='coerce')
    df.to_csv(outpath, mode='a', header=False, index=False)


#### Main
start_time = time.time()
pd.DataFrame(columns=['PATIENT_ID','DATE', 'CODE']).to_csv(outpath, encoding='latin-1', index=False)
with multiprocessing.Pool(processes=N_CPUs) as pool:
    for file in Kela_files:
        pool.apply(process_purchases, args=(file, outpath))
end_time = time.time()
print(f'Time taken to process all purchases : {end_time - start_time} seconds')





