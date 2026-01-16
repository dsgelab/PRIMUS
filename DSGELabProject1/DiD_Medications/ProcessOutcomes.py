"""
This script processes the prescription file to create yearly aggregates for all outcome codes
using either a temporary file-based chunked approach or streamlined approach.
"""

import polars as pl
import argparse
import os
from pathlib import Path
import gc
import shutil

def process_outcomes_chunked_with_temp(id_list_path, outcome_file, outcome_codes_file, outdir, temp_dir, chunk_size=50000):
    """
    Process prescription file using chunked processing with temporary files for maximum memory efficiency.
    """
    # ===== LOAD CONFIGURATION DATA =====
    print("Loading outcome codes...")
    with open(outcome_codes_file, 'r') as f:
        outcome_codes = [line.strip() for line in f.readlines()]
    
    print("Loading ID list...")
    with open(id_list_path, 'r') as f:
        id_list = set(line.strip() for line in f.readlines())  # Use set for faster lookup
    
    print(f"Processing {len(outcome_codes)} outcome codes for {len(id_list)} IDs")
    
    # ===== SETUP TEMPORARY DIRECTORY STRUCTURE =====
    temp_path = Path(temp_dir)
    temp_path.mkdir(parents=True, exist_ok=True)
    # Create subdirectory for each outcome code
    for outcome_code in outcome_codes: 
        (temp_path / outcome_code).mkdir(exist_ok=True)
    # Directory for general counts
    (temp_path / "general").mkdir(exist_ok=True)
    # Directory for first/last year tracking
    (temp_path / "first_last").mkdir(exist_ok=True)
    print(f"Temporary files will be stored in: {temp_path}")
    
    # ===== GET TOTAL ROW COUNT FOR PROGRESS TRACKING =====
    print("Counting total rows...")
    total_rows = pl.scan_csv(outcome_file).select(pl.len()).collect().item()
    print(f"Total rows to process: {total_rows}")
    
    # ===== PROCESS FILE IN CHUNKS =====
    processed_rows = 0
    chunk_num = 0
    reader = pl.read_csv_batched(outcome_file, batch_size=chunk_size)
    
    while True:
        try:
            chunk_df = reader.next_batches(1)
            if not chunk_df:
                break
            
            chunk_df = chunk_df[0]
            chunk_num += 1
            processed_rows += len(chunk_df)
            
            process_and_save_chunk(chunk_df, id_list, outcome_codes, temp_path, chunk_num)
            
            del chunk_df  
            gc.collect()
            
        except StopIteration:
            break
        except Exception as e:
            print(f"Error processing chunk {chunk_num}: {e}")
            continue
    
    print(f"Finished processing {chunk_num} chunks")
    
    # ===== COMBINE ALL TEMPORARY FILES =====
    print("Combining temporary files...")
    final_result = combine_temp_files(temp_path, outcome_codes)
    
    # ===== SAVE FINAL RESULT =====
    output_path = Path(outdir) / "processed_outcomes.parquet"
    try:
        final_result.write_parquet(output_path)
        print(f"Final outcomes saved to {output_path}")
        
        # ===== CLEANUP TEMPORARY DIRECTORY ONLY IF OUTPUT SUCCESS =====
        if output_path.exists() and output_path.stat().st_size > 0:  # Check file exists and is not empty
            print("Cleaning up temporary files...")
            shutil.rmtree(temp_path)
            print("Temporary files cleaned up")
        else:
            print(f"Warning: Output file not created successfully. Temporary files preserved in: {temp_path}")
            
    except Exception as e:
        print(f"Error saving final result: {e}")
        print(f"Temporary files preserved in: {temp_path}")
        raise

def process_and_save_chunk(chunk_df, id_list, outcome_codes, temp_path, chunk_num):
    """
    Process a single chunk and save intermediate results to temporary files.
    """
    try:
        # ===== FILTER AND PREPARE CHUNK DATA =====
        if "PRESCRIPTION_DATE" in chunk_df.columns and "DATE" not in chunk_df.columns:
            chunk_df = chunk_df.rename({"PRESCRIPTION_DATE": "DATE"})

        filtered_df = chunk_df.filter(
            (pl.col("DOCTOR_ID").is_in(list(id_list))) &
            (pl.col("DOCTOR_ID") != pl.col("PATIENT_ID")) &
            (pl.col("CODE").is_not_null()) &
            (pl.col("DATE").is_not_null())
        )
        
        if len(filtered_df) == 0:
            return

        filtered_df = filtered_df.with_columns(
            pl.col("DATE").str.to_date(strict=False)
        ).filter(
            pl.col("DATE") >= pl.date(1998, 1, 1)
        )

        if len(filtered_df) == 0:
            return
        
        filtered_df = filtered_df.with_columns([
            pl.col("DATE").dt.year().alias("YEAR")
        ])
                
        # ===== PROCESS EACH OUTCOME CODE SEPARATELY =====
        for outcome_code in outcome_codes:
            outcome_df = filtered_df.filter(
                pl.col("CODE").str.starts_with(outcome_code).fill_null(False)
            )
            
            if len(outcome_df) > 0:
                outcome_agg = outcome_df.group_by(["DOCTOR_ID", "YEAR"]).agg([
                    pl.len().alias("N_outcome")
                ]).with_columns([
                    pl.lit(outcome_code).alias("CODE")
                ])
                
                temp_file = temp_path / outcome_code / f"chunk_{chunk_num:04d}.parquet"
                outcome_agg.write_parquet(temp_file)
                
                # ===== SAVE FIRST/LAST YEAR DATA =====
                first_last_agg = outcome_df.group_by("DOCTOR_ID").agg([
                    pl.min("YEAR").alias("first_year"),
                    pl.max("YEAR").alias("last_year")
                ]).with_columns([
                    pl.lit(outcome_code).alias("CODE")
                ])
                
                temp_first_last_file = temp_path / "first_last" / f"{outcome_code}_chunk_{chunk_num:04d}.parquet"
                first_last_agg.write_parquet(temp_first_last_file)
        
        # ===== PROCESS GENERAL COUNTS =====
        general_agg = filtered_df.group_by(["DOCTOR_ID", "YEAR"]).agg([
            pl.len().alias("N_general")
        ])
        
        temp_file = temp_path / "general" / f"chunk_{chunk_num:04d}.parquet"
        general_agg.write_parquet(temp_file)
        
    except Exception as e:
        print(f"Error processing chunk {chunk_num}: {e}")
        return

def combine_temp_files(temp_path, outcome_codes):
    """
    Combine all temporary files into final result (yearly aggregation).
    """
    print("Loading and combining general counts...")
    general_files = list((temp_path / "general").glob("*.parquet"))
    if not general_files:
        raise ValueError("No general count files found!")
    
    general_dfs = [pl.read_parquet(f) for f in general_files]
    combined_general = pl.concat(general_dfs)
    
    final_general = combined_general.group_by(["DOCTOR_ID", "YEAR"]).agg([
        pl.sum("N_general").alias("N_general")
    ])
    
    final_result = final_general  # Initialize output file with general counts
    
    print("Loading and combining first/last year data...")
    first_last_data = {}
    
    for outcome_code in outcome_codes:
        first_last_files = list((temp_path / "first_last").glob(f"{outcome_code}_chunk_*.parquet"))
        
        if first_last_files:
            first_last_dfs = [pl.read_parquet(f) for f in first_last_files]
            combined_first_last = pl.concat(first_last_dfs)
            
            final_first_last = combined_first_last.group_by("DOCTOR_ID").agg([
                pl.min("first_year").alias(f"first_year_{outcome_code}"),
                pl.max("last_year").alias(f"last_year_{outcome_code}")
            ])
            
            first_last_data[outcome_code] = final_first_last
    
    # ===== PROCESS EACH OUTCOME CODE =====
    for outcome_code in outcome_codes:
        outcome_dir = temp_path / outcome_code
        outcome_files = list(outcome_dir.glob("*.parquet"))
        
        if outcome_files:
            outcome_dfs = [pl.read_parquet(f) for f in outcome_files]
            combined_outcome = pl.concat(outcome_dfs)
            
            final_outcome = combined_outcome.group_by(["DOCTOR_ID", "YEAR"]).agg([
                pl.sum("N_outcome").alias(f"N_{outcome_code}")
            ])
            
            final_result = final_result.join(
                final_outcome,
                on=["DOCTOR_ID", "YEAR"],
                how="left"
            )
            
            final_result = final_result.with_columns(
                pl.col(f"N_{outcome_code}").fill_null(0)
            )
        else:
            print(f"No data found for {outcome_code}, adding zero column")
            final_result = final_result.with_columns(
                pl.lit(0).alias(f"N_{outcome_code}")
            )
    
    # ===== ADD FIRST/LAST YEAR COLUMNS =====
    print("Adding first/last year columns...")
    for outcome_code in outcome_codes:
        if outcome_code in first_last_data:
            final_result = final_result.join(
                first_last_data[outcome_code],
                on="DOCTOR_ID",
                how="left"
            )
        else:
            final_result = final_result.with_columns([
                pl.lit(None).alias(f"first_year_{outcome_code}"),
                pl.lit(None).alias(f"last_year_{outcome_code}")
            ])
    
    # ===== CALCULATE OUTCOME RATIOS =====
    print("Calculating outcome ratios...")
    for outcome_code in outcome_codes:
        n_col = f"N_{outcome_code}"
        y_col = f"Y_{outcome_code}"
        
        final_result = final_result.with_columns(
            pl.when(pl.col("N_general") == 0)
            .then(None)
            .otherwise(pl.col(n_col) / pl.col("N_general"))
            .alias(y_col)
        )
    
    print(f"Final result shape: {final_result.shape}")
    return final_result

def process_outcomes_streamlined(id_list_path, outcome_file, outcome_codes_file, outdir):
    """
    Streamlined processing approach for smaller files or fewer outcome codes (yearly aggregation).
    """
    print("Loading outcome codes...")
    with open(outcome_codes_file, 'r') as f:
        outcome_codes = [line.strip() for line in f.readlines()]
    
    print("Loading ID list...")
    with open(id_list_path, 'r') as f:
        id_list = [line.strip() for line in f.readlines()]
    
    print(f"Processing {len(outcome_codes)} outcome codes for {len(id_list)} IDs")
    
    # ===== SETUP BASE DATAFRAME =====
    outcome_df = pl.scan_csv(outcome_file)
    
    if "PRESCRIPTION_DATE" in outcome_df.columns and "DATE" not in outcome_df.columns:
        outcome_df = outcome_df.rename({"PRESCRIPTION_DATE": "DATE"})

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
        pl.col("DATE").dt.year().alias("YEAR")
    )
    
    # ===== CALCULATE FIRST/LAST YEARS =====
    print("Calculating first/last years for each outcome code...")
    first_last_data = {}
    
    for outcome_code in outcome_codes:
        print(f"Processing first/last for outcome code: {outcome_code}")
        outcome_specific_df = outcome_df.filter(
            pl.col("CODE").str.starts_with(outcome_code).fill_null(False)
        )
        
        first_last_agg = outcome_specific_df.group_by("DOCTOR_ID").agg([
            pl.min("YEAR").alias(f"first_year_{outcome_code}"),
            pl.max("YEAR").alias(f"last_year_{outcome_code}")
        ]).collect()
        
        if len(first_last_agg) > 0:
            first_last_data[outcome_code] = first_last_agg
    
    # ===== PROCESS OUTCOME CODES =====
    batch_size = min(10, len(outcome_codes))
    all_results = []
    
    for i in range(0, len(outcome_codes), batch_size):
        batch_codes = outcome_codes[i:i+batch_size]
        print(f"Processing outcome codes batch {i//batch_size + 1}/{(len(outcome_codes)-1)//batch_size + 1}")
        
        batch_df = outcome_df
        outcome_columns = []
        for outcome_code in batch_codes:
            col_name = f"N_{outcome_code}"
            batch_df = batch_df.with_columns(
                pl.when(pl.col("CODE").str.starts_with(outcome_code).fill_null(False))
                .then(1)
                .otherwise(0)
                .alias(col_name)
            )
            outcome_columns.append(col_name)
        
        agg_exprs = [pl.sum(col).alias(col) for col in outcome_columns]
        if i == 0:
            agg_exprs.append(pl.len().alias("N_total"))
        
        batch_result = batch_df.group_by(["DOCTOR_ID", "YEAR"]).agg(agg_exprs).collect()
        all_results.append(batch_result)
        
        del batch_df
        gc.collect()
    
    # ===== COMBINE RESULTS =====
    print("Combining batch results...")
    if len(all_results) == 1:
        yearly_outcomes = all_results[0]
    else:
        yearly_outcomes = all_results[0]
        for batch_result in all_results[1:]:
            yearly_outcomes = yearly_outcomes.join(
                batch_result, 
                on=["DOCTOR_ID", "YEAR"], 
                how="outer"
            )
    
    # ===== ADD FIRST/LAST YEAR COLUMNS =====
    print("Adding first/last year columns...")
    for outcome_code in outcome_codes:
        if outcome_code in first_last_data:
            yearly_outcomes = yearly_outcomes.join(
                first_last_data[outcome_code],
                on="DOCTOR_ID",
                how="left"
            )
        else:
            yearly_outcomes = yearly_outcomes.with_columns([
                pl.lit(None).alias(f"first_year_{outcome_code}"),
                pl.lit(None).alias(f"last_year_{outcome_code}")
            ])
    
    # ===== CALCULATE RATIOS =====
    for outcome_code in outcome_codes:
        ni_col = f"N_{outcome_code}"
        ratio_col = f"Y_{outcome_code}"
        yearly_outcomes = yearly_outcomes.with_columns(
            pl.when(pl.col("N_total") == 0)
            .then(None)
            .otherwise(pl.col(ni_col) / pl.col("N_total"))
            .alias(ratio_col)
        )
    
    # ===== SAVE RESULTS =====
    output_path = Path(outdir) / "processed_outcomes.parquet"
    yearly_outcomes.write_parquet(output_path)
    print(f"Outcomes saved to {output_path}")

def main():
    parser = argparse.ArgumentParser(description='Process outcomes for DiD analysis - Yearly Aggregation')
    parser.add_argument('--id_list', required=True)
    parser.add_argument('--outcome_file', required=True)
    parser.add_argument('--outcome_codes', required=True)
    parser.add_argument('--outdir', required=True)
    parser.add_argument('--temp_dir', help='Temporary directory for intermediate files (required for chunked method)')
    parser.add_argument('--chunk_size', type=int, default=50000)
    parser.add_argument('--method', choices=['chunked', 'streamlined'], default='chunked')
    
    args = parser.parse_args()
    
    Path(args.outdir).mkdir(parents=True, exist_ok=True)
    
    if args.method == 'chunked':
        if not args.temp_dir:
            raise ValueError("--temp_dir is required when using chunked method")
        print("Using chunked yearly processing method...")
        process_outcomes_chunked_with_temp(
            args.id_list,
            args.outcome_file,
            args.outcome_codes,
            args.outdir,
            args.temp_dir,
            args.chunk_size
        )
    else:
        print("Using streamlined yearly processing method...")
        process_outcomes_streamlined(
            args.id_list,
            args.outcome_file,
            args.outcome_codes,
            args.outdir
        )

if __name__ == "__main__":
    main()
