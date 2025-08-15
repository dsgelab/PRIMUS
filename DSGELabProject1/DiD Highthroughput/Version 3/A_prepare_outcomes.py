# Libraries
import pandas as pd
from collections import defaultdict
from datetime import datetime
import argparse

# Configuration
CHUNK_SIZE = 1_000_000
INPUT_FILE = "/media/volume/Projects/mattferr/did_pipeline/Outcomes_ForRatio_20250506.csv"

# Helper function to get expected months
def get_expected_months(min_year, min_month, max_year, max_month):
    """Generate all (year, month) tuples between min and max dates (inclusive)"""
    months = []
    year, month = min_year, min_month
    
    while (year, month) <= (max_year, max_month):
        months.append((year, month))
        if month == 12:
            year += 1
            month = 1
        else:
            month += 1
    
    return set(months)


# Parse arguments
parser = argparse.ArgumentParser(description="Prepare outcome counts with date analysis.")
parser.add_argument("--output_file", type=str, default=None, help="Path to output file")
args = parser.parse_args()

# Set output file path
if args.output_file:
    output_file = args.output_file
else:
    today_str = datetime.today().strftime('%Y%m%d')
    output_file = f"/media/volume/Projects/DSGELabProject1/DiD_Highthroughput/Version2/GeneratePairs/OutcomeCounts_{today_str}.csv"

print(f"Processing {INPUT_FILE}...")
print(f"Output will be saved to: {output_file}")

# Data containers
unique_ids = set()
unique_codes = set()
combination_counts = defaultdict(int)
id_code_observed_months = defaultdict(set)  # Store (year, month) tuples for each (ID, CODE) combination

# Process file in chunks
chunk_count = 0
for chunk in pd.read_csv(INPUT_FILE, chunksize=CHUNK_SIZE):
    chunk_count += 1
    print(f"Processing chunk {chunk_count}...")
    
    # Clean column names and remove missing data
    chunk.columns = chunk.columns.str.strip()
    chunk = chunk.dropna(subset=['DOCTOR_ID', 'CODE', 'DATE'])
    
    if chunk.empty:
        continue
        
    # Process data types
    chunk['CODE'] = chunk['CODE'].astype(str).str[:5]  # Truncate to 5 chars
    chunk['DATE'] = pd.to_datetime(chunk['DATE'], errors='coerce')
    chunk = chunk.dropna(subset=['DATE'])  # Remove invalid dates
    
    # Extract year-month for vectorized processing
    chunk['YEAR'] = chunk['DATE'].dt.year
    chunk['MONTH'] = chunk['DATE'].dt.month
    
    # Update collections
    unique_ids.update(chunk['DOCTOR_ID'].unique())
    unique_codes.update(chunk['CODE'].unique())
    
    # Count ID-CODE combinations and track observed months per ID-CODE combination
    for (doctor_id, code), group in chunk.groupby(['DOCTOR_ID', 'CODE']):
        combination_counts[(doctor_id, code)] += len(group)
        
        # Track observed months for this specific ID-CODE combination
        month_tuples = set(zip(group['YEAR'], group['MONTH']))
        id_code_observed_months[(doctor_id, code)].update(month_tuples)

print(f"Processed {chunk_count} chunks.")
print(f"Found {len(unique_ids)} unique IDs and {len(unique_codes)} unique codes.")

# Calculate date statistics for each ID-CODE combination
print("Calculating date statistics for each ID-CODE combination...")
id_code_date_stats = {}

for doctor_id in unique_ids:
    for code in unique_codes:
        combination_key = (doctor_id, code)
        
        if combination_key in id_code_observed_months and id_code_observed_months[combination_key]:
            observed_months = id_code_observed_months[combination_key]
            
            # Find min and max year-month for this specific ID-CODE combination
            min_year, min_month = min(observed_months)
            max_year, max_month = max(observed_months)
            
            # Calculate expected and available months
            min_year_month = f"{min_year:04d}-{min_month:02d}"
            max_year_month = f"{max_year:04d}-{max_month:02d}"
            expected_months = get_expected_months(min_year, min_month, max_year, max_month)
            expected_year_month = len(expected_months)
            available_year_month = len(observed_months)
            all_info_available = 1 if available_year_month == expected_year_month else 0
            
            id_code_date_stats[combination_key] = {
                'min_year_month': min_year_month,
                'max_year_month': max_year_month,
                'expected_year_month': expected_year_month,
                'available_year_month': available_year_month,
                'all_info_available': all_info_available
            }
        else:
            # No observations for this ID-CODE combination
            id_code_date_stats[combination_key] = {
                'min_year_month': '',
                'max_year_month': '',
                'expected_year_month': 0,
                'available_year_month': 0,
                'all_info_available': 0
            }

# Write output
print("Writing output file...")
sorted_ids = sorted(unique_ids)
sorted_codes = sorted(unique_codes)

with open(output_file, 'w') as f:
    # Write header
    f.write("ID,CODE,COUNT,MIN_YEAR_MONTH,MAX_YEAR_MONTH,EXPECTED_YEAR_MONTH,AVAILABLE_YEAR_MONTH,ALL_INFO_AVAILABLE\n")
    
    # Write data
    for id_val in sorted_ids:
        for code_val in sorted_codes:
            combination_key = (id_val, code_val)
            count = combination_counts.get(combination_key, 0)
            date_stats = id_code_date_stats[combination_key]
            
            f.write(f"{id_val},{code_val},{count},"
                    f"{date_stats['min_year_month']},"
                    f"{date_stats['max_year_month']},"
                    f"{date_stats['expected_year_month']},"
                    f"{date_stats['available_year_month']},"
                    f"{date_stats['all_info_available']}\n")

print(f"Output saved successfully to: {output_file}")
print(f"Total rows written: {len(sorted_ids) * len(sorted_codes)}")