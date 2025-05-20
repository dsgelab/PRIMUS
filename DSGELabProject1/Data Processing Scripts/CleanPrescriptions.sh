for file in /media/volume/Data/Data_THL_2698_14.02.00_2023/Reseptikeskus/*Laakemaaraykset*; do
    echo "Processing file: $(basename "$file")"
    time0=$(date +%s)
    if [ -f "$file" ]; then
        outfile="/media/volume/Projects/DSGELabProject1/ProcessedData/CleanedPrescriptions/$(basename "$file")"
        awk -F';' '
        BEGIN { OFS=";"; bad=0 }
        {
            if (NR == 1) cols = NF;
            if (NF != cols) {
                bad++;
                next;
            }
            for (i = 1; i <= NF; i++) {
                gsub(/["\\]/, "", $i);
            }
            print $0;
        }
        END {
            if (bad > 0) print "Skipped lines due to inconsistent field count: " bad > "/dev/stderr";
        }
        ' "$file" > "$outfile"
    fi
    elapsed=$(($(date +%s) - time0))
    echo "Time taken to process: $elapsed seconds"
done
