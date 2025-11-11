"This script is to rename .txt files generated during OCR processing, if necessary"

from pathlib import Path
from bs4 import BeautifulSoup
import re

def sanitize_filename(name: str) -> str:
    """Sanitize filename by replacing spaces and punctuation with underscores."""
    # replace spaces and punctuation with underscores
    name = re.sub(r'[\s\W]+', '_', name)
    # remove leading/trailing underscores
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

def rename_ocr_txt_files(ocr_folder, extracted_folder):
    ocr_folder = Path(ocr_folder)
    extracted_folder = Path(extracted_folder)

    if not ocr_folder.exists():
        print(f"OCR folder not found: {ocr_folder}")
        return

    case_folders = sorted([d for d in ocr_folder.iterdir() if d.is_dir()])
    if not case_folders:
        print("No case folders found in OCR output.")
        return

    successful = 0
    failed = 0

    for i, case_folder in enumerate(case_folders, start=1):
        case_number = case_folder.name
        print(f"[{i}/{len(case_folders)}] Processing {case_number}")

        try:
            # find corresponding HTML file in extracted documents
            extracted_case_folder = extracted_folder / case_number
            if not extracted_case_folder.exists():
                print(f"  No corresponding folder in extracted documents, skipping.")
                continue

            html_files = list(extracted_case_folder.glob("*.htm")) + list(extracted_case_folder.glob("*.html"))
            if not html_files:
                print(f"  No HTML file found, skipping.")
                continue
            html_file = html_files[0]

            with open(html_file, "r", encoding="utf-8") as f:
                soup = BeautifulSoup(f, "html.parser")

            # create mapping from original PDF filename to sanitized document name
            pdf_mapping = {}
            for a_tag in soup.find_all("a"):
                href = a_tag.get("href")
                if href and href.lower().endswith(".pdf"):
                    doc_name = a_tag.get_text(strip=True).replace('\xa0', ' ')
                    original_pdf_name = Path(href).name.lower()
                    pdf_mapping[original_pdf_name] = sanitize_filename(doc_name)

            # rename TXT files in OCR folder
            renamed_count = 0
            for txt_file in case_folder.glob("*.txt"):
                # extract original PDF name from txt filename
                # format: "19-2-09796-1_affidavit of non-military service.txt"
                txt_stem = txt_file.stem
                
                # try to extract PDF name by removing case number prefix
                if txt_stem.startswith(f"{case_number}_"):
                    original_doc_part = txt_stem[len(case_number)+1:]  # remove "case_number_"
                    # try to find matching PDF in mapping (with .pdf extension)
                    potential_pdf_names = [
                        f"{original_doc_part}.pdf",
                        original_doc_part.lower() + ".pdf"
                    ]
                    
                    matched = False
                    for pdf_name in potential_pdf_names:
                        if pdf_name in pdf_mapping:
                            new_name = f"{pdf_mapping[pdf_name]}.txt"
                            new_path = unique_path(txt_file.parent / new_name)
                            txt_file.rename(new_path)
                            print(f"    Renamed: {txt_file.name} → {new_path.name}")
                            renamed_count += 1
                            matched = True
                            break
                    
                    if not matched:
                        # fallback: just sanitize the document part without case number
                        new_name = f"{sanitize_filename(original_doc_part)}.txt"
                        new_path = unique_path(txt_file.parent / new_name)
                        txt_file.rename(new_path)
                        print(f"    Renamed (fallback): {txt_file.name} → {new_path.name}")
                        renamed_count += 1

            print(f"  Renamed {renamed_count} txt files")
            successful += 1

        except Exception as e:
            print(f"  ERROR processing {case_number}: {e}\n")
            failed += 1

    print("\nOCR txt file renaming complete")
    print(f"  Successful: {successful}/{len(case_folders)}")
    print(f"  Failed: {failed}/{len(case_folders)}")

if __name__ == "__main__":
    ocr_folder = r"data/ocr output"
    extracted_folder = r"data/extracted documents"

    print("Renaming OCR txt files")
    rename_ocr_txt_files(ocr_folder, extracted_folder)