#!/usr/bin/env python3

import os
import pandas as pd
import glob

# Configuration
BASE_PATH = "/media/volume/Projects/DSGELabProject1/DiD_Experiment/Version5"  # Change to your base directory
OUTPUT_FILE = "/media/volume/Projects/DSGELabProject1/DiD_Experiment/Version5/CombinedResults_20250722.csv"  # Change to your output path
TARGET_DATES = ["20250722", "20250723"]

# Find matching directories and combine CSV files
combined_data = []
pattern = os.path.join(BASE_PATH, "DiD_Experiments", "Experiment_*")

for dir_path in glob.glob(pattern):
    if any(date in dir_path for date in TARGET_DATES):
        results_file = os.path.join(dir_path, "results_summary.csv")
        if os.path.exists(results_file):
            df = pd.read_csv(results_file)
            df['source_experiment'] = os.path.basename(dir_path)
            combined_data.append(df)
            print(f"Added: {os.path.basename(dir_path)}")

# Save combined results
if combined_data:
    final_df = pd.concat(combined_data, ignore_index=True)
    final_df.to_csv(OUTPUT_FILE, index=False)
    print(f"Combined {len(combined_data)} files -> {OUTPUT_FILE}")
else:
    print("No files found!")