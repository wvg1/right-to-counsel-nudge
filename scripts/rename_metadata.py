from pathlib import Path
import re

def sanitize_filename(name: str) -> str:
    """Sanitize filename by replacing spaces and punctuation with underscores."""
    name = re.sub(r'[\s\W]+', '_', name)
    name = name.strip('_')
    return name.lower()

def rename_metadata_files(metadata_folder, ocr_folder):
    """Rename metadata JSON files to match txt files."""
    metadata_folder = Path(metadata_folder)
    ocr_folder = Path(ocr_folder)
    
    renamed_count = 0
    
    for case_folder in sorted(metadata_folder.iterdir()):
        if not case_folder.is_dir():
            continue
            
        case_number = case_folder.name
        txt_folder = ocr_folder / case_number
        
        if not txt_folder.exists():
            continue
        
        # Get all txt file names (without extension)
        txt_files = {f.stem: f.name for f in txt_folder.glob("*.txt")}
        
        for json_file in case_folder.glob("*.json"):
            json_stem = json_file.stem
            
            # Skip if already renamed (no case prefix)
            if not json_stem.startswith(f"{case_number}_"):
                continue
            
            # Remove case number prefix
            doc_part = json_stem[len(case_number)+1:]
            
            # Sanitize to match txt file naming
            doc_sanitized = sanitize_filename(doc_part)
            
            # Check if matching txt file exists
            if doc_sanitized in txt_files:
                new_name = f"{doc_sanitized}.json"
                new_path = json_file.parent / new_name
                
                if not new_path.exists():
                    json_file.rename(new_path)
                    print(f"Renamed: {json_file.name} → {new_name}")
                    renamed_count += 1
    
    print(f"\nTotal renamed: {renamed_count}")

if __name__ == "__main__":
    rename_metadata_files("data/ocr metadata", "data/ocr output")