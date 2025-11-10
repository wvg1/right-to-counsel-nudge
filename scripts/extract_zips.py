"""
extract_zips.py

Extracts zipped case folders and optionally relabels PDFs with case folder prefixes.
"""

import os
import zipfile
from pathlib import Path

def extract_zips(zip_folder, extract_to):
    """Extract all zip files in zip_folder into extract_to, each in its own case folder."""
    zip_folder = Path(zip_folder)
    extract_to = Path(extract_to)

    extract_to.mkdir(parents=True, exist_ok=True)

    zip_files = sorted(list(zip_folder.glob("*.zip")))
    if not zip_files:
        print(f"No zip files found in {zip_folder}")
        return

    print(f"Found {len(zip_files)} zip files.\n")

    successful, failed = 0, 0

    for i, zip_path in enumerate(zip_files, start=1):
        case_number = zip_path.stem
        case_folder = extract_to / case_number
        print(f"[{i}/{len(zip_files)}] Extracting: {zip_path.name}")

        try:
            case_folder.mkdir(parents=True, exist_ok=True)
            with zipfile.ZipFile(zip_path, 'r') as zip_ref:
                zip_ref.extractall(case_folder)
            print(f"  Extracted successfully to {case_folder}\n")
            successful += 1
        except Exception as e:
            print(f"  ERROR extracting {zip_path.name}: {e}\n")
            failed += 1

    print("Extraction complete.")
    print(f"  Successful: {successful}/{len(zip_files)}")
    print(f"  Failed: {failed}/{len(zip_files)}")
    print(f"  All files extracted to: {extract_to}\n")


def relabel_pdfs(root_folder):
    """Rename all PDFs to include their case folder name as a prefix."""
    root_folder = Path(root_folder)
    all_pdfs = list(root_folder.rglob("*.pdf"))

    if not all_pdfs:
        print("No PDFs found for relabeling.")
        return

    print(f"Relabeling {len(all_pdfs)} PDF files...")

    for pdf_path in all_pdfs:
        case_folder = pdf_path.parent.name
        new_name = f"{case_folder}_{pdf_path.name}"
        new_path = pdf_path.parent / new_name

        if pdf_path.name.startswith(f"{case_folder}_"):
            continue  # already renamed

        try:
            pdf_path.rename(new_path)
            print(f"Renamed: {pdf_path.name} → {new_name}")
        except Exception as e:
            print(f"Could not rename {pdf_path.name}: {e}")

    print("Relabeling complete.\n")


if __name__ == "__main__":
    # example usage
    zip_folder = r"data/test_zips"
    extracted_folder = r"data/test_extracted"

    print("STEP 1: Extracting zip files")
    print("="*70)
    extract_zips(zip_folder, extracted_folder)

    print("\nSTEP 1.5: Relabeling PDFs with case folder names")
    print("="*70)
    relabel_pdfs(extracted_folder)
