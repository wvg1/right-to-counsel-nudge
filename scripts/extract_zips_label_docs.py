import os
import zipfile
from pathlib import Path
from bs4 import BeautifulSoup
import re

def sanitize_filename(name: str) -> str:
    """Sanitize filename by replacing spaces and punctuation with underscores."""
    # Replace spaces and punctuation with underscores
    name = re.sub(r'[\s\W]+', '_', name)
    # Remove leading/trailing underscores
    name = name.strip('_')
    return name.lower()

def unique_path(path: Path) -> Path:
    """If path exists, append a counter before the extension to make it unique."""
    counter = 1
    new_path = path
    while new_path.exists():
        new_path = path.with_name(f"{path.stem}{counter}{path.suffix}")
        counter += 1
    return new_path

def extract_and_label_zips(zip_folder, extract_to):
    zip_folder = Path(zip_folder)
    extract_to = Path(extract_to)
    extract_to.mkdir(parents=True, exist_ok=True)

    zip_files = sorted(zip_folder.glob("*.zip"))
    if not zip_files:
        print("No zip files found.")
        return

    successful = 0
    failed = 0

    for i, zip_path in enumerate(zip_files, start=1):
        print(f"[{i}/{len(zip_files)}] Processing {zip_path.name}")

        try:
            with zipfile.ZipFile(zip_path, 'r') as zip_ref:
                case_number = zip_path.stem
                case_folder = extract_to / case_number
                case_folder.mkdir(parents=True, exist_ok=True)
                zip_ref.extractall(case_folder)
                print(f"  Extracted to {case_folder}")

            html_files = list(case_folder.glob("*.htm")) + list(case_folder.glob("*.html"))
            if not html_files:
                print(f"  No HTML file found in {case_folder}, skipping renaming PDFs.")
                continue
            html_file = html_files[0]

            with open(html_file, "r", encoding="utf-8") as f:
                soup = BeautifulSoup(f, "html.parser")

            # create mapping from PDF filename to (sanitized) document name
            pdf_mapping = {}
            for a_tag in soup.find_all("a"):
                href = a_tag.get("href")
                if href and href.lower().endswith(".pdf"):
                    doc_name = a_tag.get_text(strip=True).replace('\xa0', ' ')
                    pdf_mapping[Path(href).name.lower()] = sanitize_filename(doc_name)

            # rename PDFs
            for pdf_file in case_folder.glob("*.pdf"):
                pdf_name = pdf_file.name.lower()
                if pdf_name in pdf_mapping:
                    new_name = f"{pdf_mapping[pdf_name]}.pdf"
                    new_path = unique_path(pdf_file.parent / new_name)
                    pdf_file.rename(new_path)
                    print(f"    Renamed: {pdf_file.name} → {new_path.name}")

            successful += 1

        except Exception as e:
            print(f"  ERROR processing {zip_path.name}: {e}\n")
            failed += 1

    print("\nExtraction & labeling complete")
    print(f"  Successful: {successful}/{len(zip_files)}")
    print(f"  Failed: {failed}/{len(zip_files)}")
    print(f"  All files extracted to: {extract_to}\n")

if __name__ == "__main__":
    zip_folder = r"data/RCT Case documents"
    extracted_folder = r"data/extracted documents"

    print("STEP 1: Extracting and labeling zip files")
    extract_and_label_zips(zip_folder, extracted_folder)