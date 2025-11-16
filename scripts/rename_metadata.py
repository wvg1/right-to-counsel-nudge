from pathlib import Path

def rename_metadata_files(metadata_folder, ocr_folder):
    """Rename metadata files to match the renamed txt files."""
    metadata_folder = Path(metadata_folder)
    ocr_folder = Path(ocr_folder)
    
    for case_folder in metadata_folder.iterdir():
        if not case_folder.is_dir():
            continue
            
        case_number = case_folder.name
        txt_folder = ocr_folder / case_number
        
        if not txt_folder.exists():
            continue
            
        # Get list of txt files (without extension)
        txt_names = {f.stem for f in txt_folder.glob("*.txt")}
        
        # Rename corresponding json files
        for json_file in case_folder.glob("*.json"):
            json_stem = json_file.stem
            
            # Remove case number prefix if present
            if json_stem.startswith(f"{case_number}_"):
                base_name = json_stem[len(case_number)+1:]
                # Try to find matching txt file
                if base_name in txt_names:
                    new_name = f"{base_name}.json"
                    new_path = json_file.parent / new_name
                    if not new_path.exists():
                        json_file.rename(new_path)
                        print(f"Renamed: {json_file.name} → {new_name}")

if __name__ == "__main__":
    rename_metadata_files("data/ocr metadata", "data/ocr output")