#### Info 
# This script extracts summary information about visits for each available patient in the dataset,
# starting from the output from Doctor_Patient_Longitudinal.py

# Start timer
START_TIME=$(date +%s)

# Input and output files
INPUT_FILE="/media/volume/Projects/DSGELabProject1/doctor_patient_longitudinal_20250220.csv.gz"
OUTPUT_FILE="/media/volume/Projects/DSGELabProject1/patient_doctor_summary_20250408.csv"

# Run AWK to process the file
zcat "$INPUT_FILE" | awk '
BEGIN {
    FS = ","
    print "PATIENT_ID,TotalPrescriptions,TotalHilmoDiagnosis,TotalAvohilmoDiagnosis,TotalPurchases,TotalDoctors,SelfPrescriptions,SelfHilmoDiagnosis,SelfAvohilmoDiagnosis"
}
NR == 1 { next }
{
    did = $1
    pid = $2
    reg = $3
    
    if (reg == "Prescription") {
        presc[pid]++
        if (did == pid) self_presc[pid]++
    } else if (reg == "Diagnosis Hilmo") {
        hilmo[pid]++
        if (did == pid) self_hilmo[pid]++
    } else if (reg == "Diagnosis Avohilmo") {
        avo[pid]++
        if (did == pid) self_avo[pid]++
    } else if (reg == "Purchase") {
        purch[pid]++
    }
    
    docs[pid][did] = 1
    patients[pid] = 1
}

END {
    for (pid in patients) {
        doc_count = 0
        for (d in docs[pid]) doc_count++
        
        printf "%s,%d,%d,%d,%d,%d,%d,%d,%d\n", 
            pid, 
            presc[pid] ? presc[pid] : 0, 
            hilmo[pid] ? hilmo[pid] : 0,
            avo[pid] ? avo[pid] : 0,
            purch[pid] ? purch[pid] : 0,
            doc_count,
            self_presc[pid] ? self_presc[pid] : 0,
            self_hilmo[pid] ? self_hilmo[pid] : 0,
            self_avo[pid] ? self_avo[pid] : 0
    }
}' 

# End timer
END_TIME=$(date +%s)
ELAPSED_TIME=$((END_TIME - START_TIME))

echo "Summary saved to $OUTPUT_FILE"
echo "Execution time: $ELAPSED_TIME seconds"