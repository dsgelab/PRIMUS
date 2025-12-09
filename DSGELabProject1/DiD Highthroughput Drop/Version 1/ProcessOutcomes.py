"""
This script processes the prescription file to calculate total prescriptions per doctor.
"""

import polars as pl
import argparse
from pathlib import Path

def process_outcomes(id_list_path, outcome_file, outdir):
    """
    Streamlined processing approach to count total prescriptions per doctor.
    """
    # ===== LOAD CONFIGURATION DATA =====
    print("Loading ID list...")
    with open(id_list_path, 'r') as f:
        id_list = [line.strip() for line in f.readlines()]
    
    print(f"Processing prescriptions for {len(id_list)} IDs")
    
    # ===== SETUP BASE DATAFRAME WITH LAZY LOADING =====
    outcome_df = pl.scan_csv(outcome_file)
    
    # ===== APPLY FILTERS AND PREPARE DATA =====
    if "PRESCRIPTION_DATE" in outcome_df.columns and "DATE" not in outcome_df.columns:
        outcome_df = outcome_df.rename({"PRESCRIPTION_DATE": "DATE"})

    outcome_df = outcome_df.filter(
        (pl.col("DOCTOR_ID").is_in(id_list)) &
        (pl.col("DOCTOR_ID") != pl.col("PATIENT_ID")) &  # Exclude self-prescriptions
        (pl.col("DATE").is_not_null())
    ).with_columns(
        pl.col("DATE").str.to_date(strict=False)
    ).filter(
        pl.col("DATE") >= pl.date(1998, 1, 1)  # QC: Filter from 1998 onwards
    ).with_columns([
        ((pl.col("DATE").dt.year() - 1998) * 12 + pl.col("DATE").dt.month()).alias("MONTH")  # Months since 1998 (0, .. , 300)
    ])
    
    # ===== COUNT PRESCRIPTIONS PER DOCTOR BY MONTH =====
    prescription_counts = outcome_df.group_by(["DOCTOR_ID", "MONTH"]).agg([
        pl.len().alias("N")
    ]).with_columns([
        (1998 + (pl.col("MONTH") - 1) // 12).alias("YEAR")  # Convert month back to year
    ]).collect()
    
    # ===== SAVE RESULTS =====
    output_path = Path(outdir) / "processed_outcomes.parquet"
    prescription_counts.write_parquet(output_path)
    print(f"Outcomes saved to {output_path}")

def main():
    parser = argparse.ArgumentParser(description='Process outcomes for DiD analysis - Memory Efficient Version')
    parser.add_argument('--id_list', required=True, help='Path to list of IDs')
    parser.add_argument('--outcome_file', required=True, help='Path to outcome file')
    parser.add_argument('--outdir', required=True, help='Output directory')
    
    args = parser.parse_args()
    
    # ===== CREATE OUTPUT DIRECTORY =====
    Path(args.outdir).mkdir(parents=True, exist_ok=True)
    
    # ===== EXECUTE PROCESSING =====
    print("Using streamlined processing method...")
    process_outcomes(
        args.id_list,
        args.outcome_file,
        args.outdir
    )

if __name__ == "__main__":
    main()