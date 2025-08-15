"""
This script processes diagnosis and/or purchase files to extract events for all codes in event_codes.txt
and stores them in a parquet file for efficient access.
"""

import polars as pl
import argparse
import os
from pathlib import Path

def process_events(id_list_path, diagnosis_file, purchases_file, event_codes_file, outdir):
    """
    Process events from diagnosis and purchase files for all event codes and ID of interest.
    """
    print("Loading event codes...")
    with open(event_codes_file, 'r') as f:
        event_codes = [line.strip() for line in f if line.strip()]

    print("Loading ID list...")
    with open(id_list_path, 'r') as f:
        id_list = [line.strip() for line in f.readlines()]

    print(f"Processing {len(event_codes)} event codes for {len(id_list)} IDs")

    all_events = []

    # Process diagnosis events
    print("Processing diagnosis events...")
    diag_event_codes = [ec for ec in event_codes if ec.startswith("Diag_")]

    if not diag_event_codes:
        print("No diagnosis event codes requested, skipping step")

    elif os.path.exists(diagnosis_file):
        diag_df = pl.read_csv(diagnosis_file)
        diag_df = diag_df.rename({"ICD10_CODE": "CODE", "VISIT_DATE": "DATE"})
        diag_df = diag_df.filter(pl.col("PATIENT_ID").is_in(id_list))

        for event_code in diag_event_codes:
            code_pattern = event_code.replace("Diag_", "", 1)
            print(f" - Processing diagnosis starting with {code_pattern}")

            # Filter events where CODE starts with code_pattern
            event_df = diag_df.filter(pl.col("CODE").str.starts_with(code_pattern).fill_null(False))
            if len(event_df) > 0:
                # Keep only first event per patient
                event_df = event_df.sort("DATE").group_by("PATIENT_ID").first()
                event_df = event_df.with_columns(pl.lit("Diag").alias("SOURCE"))
                # Select only necessary columns
                event_df = event_df.select(["PATIENT_ID", "CODE", "DATE", "SOURCE"])
                all_events.append(event_df)

    # Process purchase events
    print("Processing purchase events...")
    purch_event_codes = [ec for ec in event_codes if ec.startswith("Purch_")]

    if not purch_event_codes:
        print("No purchase event codes requested, skipping step")

    elif os.path.exists(purchases_file):
        # Use lazy loading for large files
        purch_df_lazy = pl.scan_csv(purchases_file)
        purch_df_lazy = purch_df_lazy.rename({"ATC_CODE": "CODE", "PURCHASE_DATE": "DATE"})
        purch_df_lazy = purch_df_lazy.filter(pl.col("PATIENT_ID").is_in(id_list))

        for event_code in purch_event_codes:
            code_pattern = event_code.replace("Purch_", "", 1)
            print(f" - Processing purchase starting with {code_pattern}")

            # Filter events where CODE starts with code_pattern
            event_df_lazy = purch_df_lazy.filter(pl.col("CODE").str.starts_with(code_pattern).fill_null(False))
            # Keep only first event per patient
            event_df_lazy = event_df_lazy.sort("DATE").group_by("PATIENT_ID").first()
            event_df_lazy = event_df_lazy.with_columns(pl.lit("Purch").alias("SOURCE"))
            event_df = event_df_lazy.collect()
            if len(event_df) > 0:
                # Select only necessary columns
                event_df = event_df.select(["PATIENT_ID", "CODE", "DATE", "SOURCE"])
                all_events.append(event_df)
    
    # Combine all events
    if all_events:
        print("Combining all events...")
        combined_events = pl.concat(all_events)
        final_events = combined_events.select([
            "PATIENT_ID",
            "CODE", 
            "DATE",
            "SOURCE"
        ])
        
        # Save to parquet
        output_path = Path(outdir) / "processed_events.parquet"
        final_events.write_parquet(output_path)
        print(f"Events saved to {output_path}")
        
    else:
        print("No events found")

def main():
    parser = argparse.ArgumentParser(description='Process events for DiD analysis')
    parser.add_argument('--id_list', required=True, help='Path to list of IDs')
    parser.add_argument('--diagnosis_file', required=True, help='Path to diagnosis file')
    parser.add_argument('--purchases_file', required=True, help='Path to purchases file')
    parser.add_argument('--event_codes', required=True, help='Path to event codes file')
    parser.add_argument('--outdir', required=True, help='Output directory')

    args = parser.parse_args()

    # Create output directory if it doesn't exist
    Path(args.outdir).mkdir(parents=True, exist_ok=True)

    process_events(
        args.id_list,
        args.diagnosis_file,
        args.purchases_file,
        args.event_codes,
        args.outdir
    )

if __name__ == "__main__":
    main()