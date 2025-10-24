"""
This script cleans and consolidates all individual JSON output into one CSV for R analysis.
Working directory should be set as right-to-counsel-nudge folder.
Make sure extract_json.py has already been run.
"""

import json
import pandas as pd
from pathlib import Path
from collections import defaultdict

#find all JSON files
json_folder = Path("data/extracted_json")
all_json_files = list(json_folder.rglob("*.json"))

if len(all_json_files) == 0:
    print("No JSON files found")
    print("Did you run extract_json.py?!?")
    exit()

print(f"Found {len(all_json_files)} JSON files\n")

#group by case folder
case_data = defaultdict(list)

for json_file in all_json_files:
    try:
        with open(json_file, "r", encoding="utf-8") as f:
            data = json.load(f)
            
        #add metadata
        data["_source_file"] = json_file.name
        case_folder = json_file.parent.name
        case_data[case_folder].append(data)
        
    except Exception as e:
        print(f"⚠️  Could not load {json_file.name}: {e}")

print(f"Found {len(case_data)} unique case folders\n")

#convert to dataframe
all_cases = [
    {**data_point, "case_folder": folder}
    for folder, docs in case_data.items()
    for data_point in docs
]

df = pd.DataFrame(all_cases)

#reorder columns: data fields first, then metadata
data_cols = [col for col in df.columns if not col.startswith("_")]
meta_cols = [col for col in df.columns if col.startswith("_")]
df = df[data_cols + meta_cols]

#save .csv
output_file = Path("data/consolidated_cases.csv")
df.to_csv(output_file, index=False)

print(f"\n✓ Saved {len(df)} cases to {output_file}")
print(f"\nColumns ({len(df.columns)}):")
print(f"  {', '.join(data_cols[:8])}...")
print(f"\nSummary Statistics:")
print(f"  Cases with case_number: {df['case_number'].ne('').sum()}")
print(f"  Cases with appearance_date: {df['appearance_date'].ne('').sum()}")
print(f"  Cases with writ issued: {(df['writ'] == 'Yes').sum()}")
print(f"  Cases with dismissal: {(df['dismissal'] == 'Yes').sum()}")
print(f"  Cases with rep appointed: {(df['rep_appointed'] == 'Yes').sum()}")
print(df[['case_number', 'appearance_date', 'writ', 'dismissal', 'rep_appointed']].head())