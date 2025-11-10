import os
import json
import re
from pathlib import Path
from typing import Optional, Dict, List, Any
import pickle

from data_from_llm import data_from_llm


def get_extracted_docs_path() -> Path:
    """Get path to extracted documents directory."""
    return Path.home() / "Documents" / "right-to-counsel-nudge" / "data" / "extracted documents"


def get_pdf_files(base_path: Path) -> List[Dict[str, str]]:
    """
    Scan extracted documents directory and return list of PDFs with metadata.
    
    Returns:
        List of dicts with keys: 'full_path', 'filename', 'case_id', 'text'
    """
    pdf_files = []
    
    if not base_path.exists():
        print(f"Error: Directory not found: {base_path}")
        return pdf_files
    
    for case_folder in sorted(base_path.iterdir()):
        if not case_folder.is_dir():
            continue
        
        case_id = case_folder.name
        
        for pdf_file in sorted(case_folder.glob("*.pdf")):
            # Extract text from PDF filename (assuming OCR or metadata extraction)
            # For now, we'll use the filename as the text identifier
            pdf_files.append({
                'full_path': str(pdf_file),
                'filename': pdf_file.name,
                'case_id': case_id,
                'text': None  # Will be populated when needed
            })
    
    return pdf_files


def filter_documents(pdf_files: List[Dict[str, str]]) -> List[Dict[str, str]]:
    """
    Filter PDFs to include only those matching inclusion criteria 
    and not matching exclusion criteria.
    """
    doc_names_agreement_include = ["cr2a", "agreement"]
    doc_names_agreement_excluded = ["motion", "lease", "rental", "declaration"]
    
    filtered = []
    
    for doc in pdf_files:
        filename_lower = doc['filename'].lower()
        
        # Check inclusion: must contain at least one inclusion keyword
        include = any(keyword in filename_lower for keyword in doc_names_agreement_include)
        
        # Check exclusion: must not contain any exclusion keywords
        exclude = any(keyword in filename_lower for keyword in doc_names_agreement_excluded)
        
        if include and not exclude:
            filtered.append(doc)
    
    return filtered


def read_pdf_text(pdf_path: str, ocr_output_folder: Path) -> Optional[str]:
    """
    Read OCR'd text from the OCR output folder.
    Matches the PDF path structure to find corresponding .txt file.
    """
    try:
        # Convert PDF path to corresponding OCR text path
        pdf_path = Path(pdf_path)
        relative_path = pdf_path.relative_to(get_extracted_docs_path())
        txt_path = ocr_output_folder / relative_path.with_suffix(".txt")
        
        if not txt_path.exists():
            print(f"  ⚠ OCR text not found: {txt_path}")
            return None
        
        with open(txt_path, 'r', encoding='utf-8') as f:
            text = f.read()
        
        if not text or len(text.strip()) < 50:
            print(f"  ⚠ OCR text too short or empty")
            return None
        
        return text
        
    except Exception as e:
        print(f"  Error reading OCR text: {e}")
        return None


def clean_fences(s: str) -> str:
    """Remove leading/trailing code fences from JSON strings."""
    return re.sub(r"^```[a-zA-Z]*\s*|\s*```$", "", s).strip()


def validate_json(s: str) -> bool:
    """Validate that string is valid JSON."""
    try:
        json.loads(s)
        return True
    except json.JSONDecodeError:
        return False


def merge_dupe_vars(obj: Dict[str, Any]) -> Dict[str, Any]:
    """Merge duplicate fields in a dict, combining values into lists."""
    if not obj or not isinstance(obj, dict):
        return obj
    
    out = {}
    for key, val in obj.items():
        if isinstance(val, list):
            # Flatten and deduplicate
            out[key] = list(set(str(v) for v in val if v))
        else:
            out[key] = val
    
    return out


def parse_agreement_doc(llm_response: str) -> Optional[Dict[str, Any]]:
    """
    Parse LLM response for agreement document.
    
    Returns:
        Dict with extracted fields or None if parsing fails
    """
    # Clean fences and parse JSON
    cleaned = clean_fences(llm_response)
    
    if not validate_json(cleaned):
        print(f"Invalid JSON response")
        return None
    
    try:
        data = json.loads(cleaned)
        data = merge_dupe_vars(data)
        return data
    except Exception as e:
        print(f"Error parsing JSON: {e}")
        return None


def extract_agreement_data(pdf_docs: List[Dict[str, str]], prompt: str, ocr_output_folder: Path, test_mode: bool = False, n_test: int = 15) -> List[Dict[str, Any]]:
    """
    Extract agreement data from filtered PDF documents using LLM.
    
    Args:
        pdf_docs: List of filtered PDF document dicts
        prompt: LLM prompt for extraction
        ocr_output_folder: Path to folder containing OCR'd text files
        test_mode: If True, only process first n_test documents
        n_test: Number of documents to process in test mode
    
    Returns:
        List of extracted data dictionaries with case info
    """
    results = []
    
    if test_mode:
        pdf_docs = pdf_docs[:n_test]
    
    for idx, doc in enumerate(pdf_docs, 1):
        print(f"Processing {idx}/{len(pdf_docs)}: {doc['case_id']} - {doc['filename']}")
        
        # Read OCR'd text
        text = read_pdf_text(doc['full_path'], ocr_output_folder)
        
        if not text:
            print(f"  ⚠ No text extracted")
            continue
        
        try:
            # Call LLM to extract data
            llm_response = data_from_llm(prompt, text)
            parsed = parse_agreement_doc(llm_response)
            
            if parsed:
                # Add source document information
                parsed['row_id'] = idx
                parsed['case_id'] = doc['case_id']
                parsed['filename'] = doc['filename']
                parsed['full_path'] = doc['full_path']
                results.append(parsed)
                print(f"  ✓ Extracted")
            else:
                print(f"  ✗ Failed to parse response")
                
        except Exception as e:
            print(f"  ✗ Error: {e}")
            continue
    
    return results


def save_results(results: List[Dict[str, Any]], output_path: str) -> None:
    """Save extracted data to pickle and CSV files."""
    import pandas as pd
    
    # Save pickle
    with open(output_path, 'wb') as f:
        pickle.dump(results, f)
    print(f"Results saved to: {output_path}")
    
    # Convert to CSV
    csv_path = output_path.replace('.pkl', '.csv')
    
    # Flatten the data for CSV (convert lists to comma-separated strings)
    flattened = []
    for result in results:
        flat_result = result.copy()
        # Convert list fields to semicolon-separated strings
        for key in ['plaintiff_names', 'plaintiff_attorneys', 'defendant_names', 'defendant_attorneys']:
            if key in flat_result and isinstance(flat_result[key], list):
                flat_result[key] = '; '.join(flat_result[key]) if flat_result[key] else ''
        flattened.append(flat_result)
    
    df = pd.DataFrame(flattened)
    df.to_csv(csv_path, index=False, encoding='utf-8')
    print(f"Results also saved to: {csv_path}")


def main():
    # Define base path
    base_path = get_extracted_docs_path()
    ocr_output_folder = Path.home() / "Documents" / "right-to-counsel-nudge" / "data" / "ocr output"
    
    print(f"Scanning: {base_path}")
    print(f"Reading OCR from: {ocr_output_folder}\n")
    
    if not ocr_output_folder.exists():
        print(f"ERROR: OCR output folder not found at {ocr_output_folder}")
        print("Please run OCR processing first using your OCR script.")
        return
    
    # Get all PDFs
    all_pdfs = get_pdf_files(base_path)
    print(f"Found {len(all_pdfs)} total PDF files")
    
    # Filter documents
    filtered_pdfs = filter_documents(all_pdfs)
    print(f"Filtered to {len(filtered_pdfs)} documents matching criteria (cr2a or agreement, excluding motion/lease/rental/declaration)\n")
    
    # Define LLM prompt (same as R script)
    prompt_agreement = '''
You are an assistant that reads documents filed in Washington State unlawful detainer (eviction) cases and extracts fields.

Return ONLY valid raw JSON and nothing else. No prose, no code fences.

Schema:
{
  "document_file_date": "string",
  "case_number": "string",
  "agreement": "Yes" | "No",
  "tenant_move": "Yes" | "No",
  "plaintiff_names": ["string"],
  "plaintiff_attorneys": ["string"],
  "defendant_names": ["string"],
  "defendant_attorneys": ["string"],
  "confidence": {
    "document_file_date": 0.0,
    "case_number": 0.0,
    "agreement": 0.0,
    "tenant_move": 0.0,
    "plaintiff_names": 0.0,
    "plaintiff_attorneys": 0.0,
    "defendant_names": 0.0,
    "defendant_attorneys": 0.0
  }
}

CONFIDENCE SCORES
- For each field, provide a confidence score from 0.0 to 1.0.
- 1.0 = highly confident (explicit, clear evidence in document)
- 0.7-0.9 = moderately confident (clear but with minor ambiguity)
- 0.4-0.6 = low-moderate confidence (some evidence but uncertain interpretation)
- 0.0-0.3 = low confidence (little evidence, mostly guessing, or field is empty)
- Empty fields should generally have confidence 0.0 unless there is explicit evidence of absence.

STRICT RULES
- If a STRING field is unknown, set it to "" (empty string).
- Do NOT guess. Prefer "" to an invented value.
- Use only ASCII. Trim leading/trailing spaces; collapse internal whitespace to single spaces.
- No newlines in any field. No trailing commas in JSON.

DOCUMENT FILE DATE
- Set "document_file_date" based on when the document was filed in the court, rather than when it was signed.
- Search priority (stop at the first match that fits):
1) A clerk/e-filing stamp or header with words like "FILED", "E-FILED", "ACCEPTED", "ENTERED", "RECEIVED", "SUBMITTED", near a date/time (often on page 1, top-right).
2) A docket/header watermark showing a file/entry date.
- Ignore signature dates, notary acknowledgments, certificate of service/mailing dates, "DATED this …" lines, and any dates inside the narrative body.
- If only a 2-digit year is present, assume 2000–2099 (e.g., 9/5/24 → 2024-09-05).
- Normalize "document_file_date" to ISO "YYYY-MM-DD".
- Accept common variants (e.g., "9/5/24", "09/05/2024", "September 5, 2024") and normalize to "YYYY-MM-DD".

CASE NUMBER
- Extract the WA Superior Court case number matching regex: ^\\d{2}-\\d-\\d{5}-\\d$.
- If the digits appear with other separators (e.g., "23 2 04870 4"), normalize to "23-2-04870-4".
- If you cannot form exactly 9 digits in that pattern with high confidence, set "case_number": "".

AGREEMENT
- "Yes" if the document describes an agreement between the plaintiff and defendant sides in the case, otherwise "No".
- For the purposes of this field, leases and rental agreements made prior to eviction cases should result in "No".

TENANT MOVE
- "Yes" if the agreement in the document requires the defendant (tenant) to leave, vacate, move out of the property, otherwise "No".
- For example, an agreement to end the tenancy would be "Yes" while an agreement to continue the tenancy would be "No".

NAMES
- Provide full names only (no labels like "Plaintiff:"/"Defendant:"). Split multiple persons into separate array elements.
- Remove tokens like "aka", "dba" and keep the primary legal name.

Return only the JSON object described above.
'''
    
    # Extract data from filtered documents (test mode)
    print("Running in TEST MODE (first 15 documents)...\n")
    results = extract_agreement_data(filtered_pdfs, prompt_agreement, ocr_output_folder, test_mode=True, n_test=15)
    
    # Save results
    output_path = Path.home() / "Documents" / "right-to-counsel-nudge" / "data" / "llm_data_agreement.pkl"
    save_results(results, str(output_path))
    
    print(f"\nProcessed {len(results)} documents successfully")


if __name__ == "__main__":
    main()