# Define global variables
INPUT_CSV="/media/volume/Projects/DSGELabProject1/ProcessedData/AllConnectedPurchases_20250421.csv"
OUTPUT_CSV="/media/volume/Projects/DSGELabProject1/ProcessedData/AllConnectedPurchases_FirstEvents_20250421.csv"
TMPDIR="/media/volume"  # Temp files go here if RAM runs out

# Step 1: filter columns and write header
awk -F, '
BEGIN {OFS=","; print "PATIENT_ID,PURCHASE_DATE,ATC_CODE"}
NR > 1 {
    print $1, $2, $3
}' "$INPUT_CSV" > step1.csv

# Step 2: sort by PATIENT_ID, then ATC_CODE, then PURCHASE_DATE
# This ensures we get the earliest date for each patient-ATC combination
sort --parallel=$(nproc) -S75% --temporary-directory="$TMPDIR" -t, -k1,1 -k3,3 -k2,2 step1.csv > step2.csv

# Step 3: keep first occurrence of each ATC_CODE per PATIENT_ID
awk -F, '
BEGIN {OFS=","; print "PATIENT_ID,PURCHASE_DATE,ATC_CODE"}
NR > 1 && !seen[$1","$3]++ {
    print
}' step2.csv > "$OUTPUT_CSV"

# Cleanup
rm step1.csv step2.csv