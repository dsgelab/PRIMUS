#!/bin/bash
# Define file paths
SCRIPT_DIR="/c:/Users/Jakob German/Documents/GitHub/PRIMUS/DSGELabProject1/jg_scripts"
INPUT_FILE_PATH="$SCRIPT_DIR/input/input_file.csv"
OUTPUT_FILE_PATH="$SCRIPT_DIR/output/output_$(date +%Y%m%d_%H%M%S).csv.gz"
LOG_FILE_PATH="$SCRIPT_DIR/logs/script_log_$(date +%Y%m%d_%H%M%S).log"

# Ensure output and log directories exist
mkdir -p "$(dirname "$OUTPUT_FILE_PATH")"
mkdir -p "$(dirname "$LOG_FILE_PATH")"

# Log file setup
exec > >(tee -a "$LOG_FILE_PATH") 2>&1

echo "$(date '+%Y-%m-%d %H:%M:%S') - Script started."

# Check for input file argument
if [ "$#" -ne 1 ]; then
    echo "$(date '+%Y-%m-%d %H:%M:%S') - Error: No input file provided."
    echo "Usage: $0 <input_file.csv>"
    exit 1
fi

INPUT_FILE="$1"

# Check if input file exists
if [ ! -f "$INPUT_FILE" ]; then
    echo "$(date '+%Y-%m-%d %H:%M:%S') - Error: Input file '$INPUT_FILE' not found."
    exit 1
fi

# Output file setup
OUTPUT_FILE="output_$(date +%Y%m%d_%H%M%S).csv.gz"

echo "$(date '+%Y-%m-%d %H:%M:%S') - Processing input file: $INPUT_FILE"

# Extract header and determine column positions
HEADER=$(head -n 1 "$INPUT_FILE")
IFS=',' read -r -a COLUMNS <<< "$HEADER"

# Find column indices
for i in "${!COLUMNS[@]}"; do
    case "${COLUMNS[$i]}" in
        "ATC_CODE") ATC_CODE_INDEX=$((i + 1)) ;;
        "COLUMN_2") COLUMN_2_INDEX=$((i + 1)) ;; # Replace COLUMN_2 with actual column name
        "COLUMN_3") COLUMN_3_INDEX=$((i + 1)) ;; # Replace COLUMN_3 with actual column name
        # Add more cases as needed
    esac
done

if [ -z "$ATC_CODE_INDEX" ] || [ -z "$COLUMN_2_INDEX" ] || [ -z "$COLUMN_3_INDEX" ]; then
    echo "$(date '+%Y-%m-%d %H:%M:%S') - Error: Required columns not found in the header."
    exit 1
fi

# Process the file
awk -F, -v OFS=, -v atc="$ATC_CODE_INDEX" -v col2="$COLUMN_2_INDEX" -v col3="$COLUMN_3_INDEX" '
NR == 1 {
    $atc = "CODE"
    print $1, $col2, $2, $4, $3, $5 > "temp_header.csv"
    next
}
{
    key = $1 FS $4 FS $3
    if (!seen[key]++) print $0 > "temp_data.csv"
}' "$INPUT_FILE"

# Combine header and filtered data
cat temp_header.csv temp_data.csv | gzip > "$OUTPUT_FILE"

# Cleanup temporary files
rm -f temp_header.csv temp_data.csv

echo "$(date '+%Y-%m-%d %H:%M:%S') - Output file created: $OUTPUT_FILE"
echo "$(date '+%Y-%m-%d %H:%M:%S') - Script completed."