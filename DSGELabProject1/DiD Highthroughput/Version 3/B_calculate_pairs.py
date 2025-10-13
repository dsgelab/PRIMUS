import pandas as pd
import argparse
import time
import csv

def analyze_events_outcomes(events, outcomes, doctor_ids):
    print("Starting analysis...")
    start_time = time.time()
    
    doctor_ids_set = set(doctor_ids)
    print(f"Processing {len(doctor_ids)} doctor IDs")
    
    # Filter events once upfront
    filtered_events = events[events['ID'].isin(doctor_ids_set)]
    
    # Pre-filter outcomes for relevant IDs and COUNT >= 50
    relevant_outcomes = outcomes[
        (outcomes['ID'].isin(doctor_ids_set)) & 
        (outcomes['COUNT'] >= 50)
    ]
    
    # Group filtered events by CODE to get unique IDs for each event code
    event_groups = filtered_events.dropna(subset=['CODE', 'ID']).groupby('CODE')['ID'].apply(set).to_dict()
    
    # Group relevant outcomes by CODE and get unique IDs for each outcome code
    outcome_groups = relevant_outcomes.dropna(subset=['CODE', 'ID']).groupby('CODE')['ID'].apply(set).to_dict()
    
    results = []
    
    # Filter out NaN values from unique codes
    event_codes = filtered_events['CODE'].dropna().unique()
    outcome_codes = relevant_outcomes['CODE'].dropna().unique()
    
    total_pairs = len(event_codes) * len(outcome_codes)
    print(f"Processing {len(event_codes)} event codes × {len(outcome_codes)} outcome codes = {total_pairs} pairs")
    
    # For each event-outcome pair, calculate intersections
    processed = 0
    for event_code in event_codes:
        event_ids = event_groups[event_code]
        n_cases = len(event_ids)
        
        for outcome_code in outcome_codes:
            # Count intersection of event_ids and outcome_ids (with COUNT >= 50)
            outcome_ids = outcome_groups.get(outcome_code, set())
            n_cases_50_outcomes = len(event_ids & outcome_ids)
            
            results.append({
                'event_code': event_code,
                'outcome_code': outcome_code,
                'N_CASES': n_cases,
                'N_CASES_50_OUTCOMES': n_cases_50_outcomes
            })
            
            processed += 1
            if processed % 10_000 == 0:
                elapsed = time.time() - start_time
                print(f"Processed {processed:,}/{total_pairs:,} pairs ({processed/total_pairs*100:.1f}%) - {elapsed:.1f}s")
    
    elapsed = time.time() - start_time
    print(f"Analysis complete: {len(results)} results in {elapsed:.1f}s")
    
    return results

# Main
parser = argparse.ArgumentParser(description="Analyze event-outcome pairs.")
parser.add_argument('--events', required=True, help='Path to event file')
parser.add_argument('--outcomes', required=True, help='Path to outcome file')
parser.add_argument('--doctor_list', required=True, help='Path to list of ids file')
parser.add_argument('--output', required=True, help='Path to output results file')
args = parser.parse_args()

# Read files
events = pd.read_csv(args.events, dtype=str, encoding='utf-8', on_bad_lines='skip', quoting=csv.QUOTE_NONE, engine='python')
print(f"Loaded {len(events)} events")

outcomes = pd.read_csv(args.outcomes, dtype=str, encoding='utf-8', on_bad_lines='skip', quoting=csv.QUOTE_NONE, engine='python')
print(f"Loaded {len(outcomes)} outcomes")

# Clean and convert COUNT column to integer
print("Converting COUNT column to numeric...")
original_count = len(outcomes)

# Convert COUNT to numeric (int), warn if NaNs are present
outcomes['COUNT'] = pd.to_numeric(outcomes['COUNT'], errors='coerce')
n_nans = outcomes['COUNT'].isna().sum()
if n_nans > 0:
    print(f"Warning: {n_nans} rows have invalid COUNT values and will not be converted to int.")
    # Only convert non-NaN values to int
    outcomes.loc[outcomes['COUNT'].notna(), 'COUNT'] = outcomes.loc[outcomes['COUNT'].notna(), 'COUNT'].astype(int)
else:
    outcomes['COUNT'] = outcomes['COUNT'].astype(int)
print(f"Final outcomes dataset: {len(outcomes)} rows (COUNT column: int or NaN)")

with open(args.doctor_list) as f:
    doctor_ids = [line.strip() for line in f if line.strip()]

# Analyze event-outcome pairs
results = analyze_events_outcomes(events, outcomes, doctor_ids)

print(f"Saving {len(results)} results to {args.output}")
# Save results
pd.DataFrame(results).to_csv(args.output, index=False)