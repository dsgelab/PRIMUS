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

df = pd.read_csv(args.inpath)
# filter requested IDs and Event codes
df = df[df['PATIENT_ID'].isin(ID_LIST)]
df['EVENT'] = df['CODE'].astype(str).str.match(CODE_REGEX, na=False).astype(int)
# save list of controls
controls = df[df['EVENT'] == 0]['PATIENT_ID'].unique()
pd.Series(controls).to_csv(args.outdir+"/ID_controls.csv", index=False, header=False)
#save list of cases
df_filtered = df[df['EVENT'] == 1]
pd.Series(df_filtered['PATIENT_ID'].unique()).to_csv(args.outdir+"/ID_cases.csv", index=False, header=False)
# only use the first event per individual (if multiple events)
if args.event_register == "Purch": df_filtered.rename(columns={'PURCHASE_DATE': 'DATE'}, inplace=True)
df_filtered = df_filtered.sort_values('DATE').drop_duplicates('PATIENT_ID', keep='first')
df_filtered.to_csv(args.outdir+"/Events.csv", index=False)

