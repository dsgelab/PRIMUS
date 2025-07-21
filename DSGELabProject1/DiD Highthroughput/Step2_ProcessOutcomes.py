"""
This script processes the prescription file to create monthly aggregates for all outcome codes
and stores them in a parquet file for efficient access.
"""

import polars as pl
import argparse
import os
from pathlib import Path

def process_outcomes(id_list_path, outcome_file, outcome_codes_file, outdir):
    """
    Process prescription file using lazy loading to create monthly aggregates for all outcome codes.
    """
    print("Loading outcome codes...")
    with open(outcome_codes_file, 'r') as f:
        outcome_codes = [line.strip() for line in f.readlines()]
    
    print("Loading ID list...")
    with open(id_list_path, 'r') as f:
        id_list = [line.strip() for line in f.readlines()]
    
    print(f"Processing {len(outcome_codes)} outcome codes for {len(id_list)} IDs")
    
    # Use lazy loading - this doesn't load data into memory yet
    outcome_df = pl.scan_csv(outcome_file)
    
    # Build the entire query lazily:
    # - filter doctors in list
    # - filter out self-prescriptions
    # - ensure CODE and DATE are not null
    # - create MONTH column
    print("Filtering and preparing data...") 
    outcome_df = outcome_df.filter(
        (pl.col("DOCTOR_ID").is_in(id_list)) &
        (pl.col("DOCTOR_ID") != pl.col("PATIENT_ID")) &
        (pl.col("CODE").is_not_null()) &
        (pl.col("DATE").is_not_null())
    ).with_columns(
        pl.col("DATE").str.to_date(strict=False)
    ).filter(
        pl.col("DATE") >= pl.date(1998, 1, 1)
    ).with_columns(
        ((pl.col("DATE").dt.year() - 1998) * 12 + pl.col("DATE").dt.month()).alias("MONTH")
    )
    
    # Create outcome indicators for each code
    outcome_columns = []
    for outcome_code in outcome_codes:
        print(f" - Adding {outcome_code} to lazy query")
        col_name = f"N_{outcome_code}"
        outcome_df = outcome_df.with_columns(
            pl.when(pl.col("CODE").str.starts_with(outcome_code).fill_null(False))
            .then(1)
            .otherwise(0)
            .alias(col_name)
        )
        outcome_columns.append(col_name)
    
    # Aggregate by DOCTOR_ID and MONTH (still lazy)
    agg_exprs = [pl.sum(col).alias(col) for col in outcome_columns]
    agg_exprs.append(pl.len().alias("N_total"))
    monthly_outcomes = outcome_df.group_by(["DOCTOR_ID", "MONTH"]).agg(agg_exprs)
    
    # Calculate ratios
    for outcome_code in outcome_codes:
        ni_col = f"N_{outcome_code}"
        ratio_col = f"Y_{outcome_code}"
        monthly_outcomes = monthly_outcomes.with_columns(
            pl.when(pl.col("N_total") == 0)
            .then(None)
            .otherwise(pl.col(ni_col) / pl.col("N_total"))
            .alias(ratio_col)
        )
    
    # Add YEAR column 
    monthly_outcomes = monthly_outcomes.with_columns((1998 + (pl.col("MONTH") - 1) // 12).alias("YEAR"))

    # Execute the query and save results
    print("Executing aggregation query...")
    monthly_outcomes = monthly_outcomes.collect()
    output_path = Path(outdir) / "processed_outcomes.parquet"
    monthly_outcomes.write_parquet(output_path)
    print(f"Outcomes saved to {output_path}")


def main():
    parser = argparse.ArgumentParser(description='Process outcomes for DiD analysis')
    parser.add_argument('--id_list', required=True, help='Path to list of IDs')
    parser.add_argument('--outcome_file', required=True, help='Path to outcome file')
    parser.add_argument('--outcome_codes', required=True, help='Path to outcome codes file')
    parser.add_argument('--outdir', required=True, help='Output directory')
    
    args = parser.parse_args()
    
    # Create output directory if it doesn't exist
    Path(args.outdir).mkdir(parents=True, exist_ok=True)
    
    process_outcomes(
        args.id_list,
        args.outcome_file,
        args.outcome_codes,
        args.outdir
    )

if __name__ == "__main__":
    main()