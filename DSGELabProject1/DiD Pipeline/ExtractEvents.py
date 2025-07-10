#### Info:
# This script processes the Filtered THL or Kela files in order to extract the cases (based on required code)

# INPUT:
# - id_list: file path to list of IDs to be used (doctors or doctors+spouse+offspring)
# - outdir: directory where the results want to be saved
# - event_code: code of the required event, ICD10 or ATC
# OUTPUT:
# - data_cases.csv - contains CODE, DATE and ID of the cases
# - ID_cases.csv + ID_controls.csv - containing the list of cases and controls

#### Libraries:
import pandas as pd
import argparse

##### Arguments
parser = argparse.ArgumentParser()
parser.add_argument('--id_list', type=str, help='file path to list of IDs to be used')
parser.add_argument('--inpath', type=str, help='directory where the input file is located')
parser.add_argument('--outdir', type=str,help='directory where the results want to be saved')
parser.add_argument('--event_register', type=str,help='code of the required event, ICD10 or ATC')
parser.add_argument('--event_code', type=str,help='code of the required event, ICD10 or ATC')
args = parser.parse_args()

print("using the following arguments: ")
print(args)

#### Main
CODE_REGEX = args.event_code # regex for the event code
with open(args.id_list, 'r') as file:
    ID_LIST = [line.strip() for line in file.readlines()]

# Process CSV in chunks to reduce RAM usage
chunk_size = 1_000_000  
events_list = []

for chunk in pd.read_csv(args.inpath, chunksize=chunk_size):
    # fix column names
    if args.event_register == "Diag": chunk.rename(columns={'ICD10_CODE': 'CODE', 'VISIT_DATE': 'DATE'}, inplace=True)
    if args.event_register == "Purch": chunk.rename(columns={'PURCHASE_DATE': 'DATE'}, inplace=True)
    # filter requested IDs and Event cases
    chunk = chunk[chunk['PATIENT_ID'].isin(ID_LIST)]
    chunk['EVENT'] = chunk['CODE'].astype(str).str.match(CODE_REGEX, na=False).astype(int)
    events_list.append(chunk[chunk['EVENT'] == 1])

# keep only the first event per individual
if events_list:
    events_df = pd.concat(events_list)
    events_df = events_df.sort_values('DATE').drop_duplicates('PATIENT_ID', keep='first')
    events_df.to_csv(args.outdir+"/Events.csv", index=False)
else:
    print('No events found for the given criteria.')
    pd.DataFrame().to_csv(args.outdir+"/Events.csv", index=False)

