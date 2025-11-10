"""
ocr_processing.py

Processes PDFs extracted from case folders using Azure Document Intelligence.
Extracted text and metadata are saved to separate folders.
"""

import os
import time
import json
from pathlib import Path
from dotenv import load_dotenv

from azure.core.credentials import AzureKeyCredential
from azure.ai.documentintelligence import DocumentIntelligenceClient
from azure.ai.documentintelligence.models import AnalyzeDocumentRequest

# Load environment variables from .env
load_dotenv()

# Azure Document Intelligence client setup
endpoint = os.getenv("AZURE_DI_ENDPOINT")
key = os.getenv("AZURE_DI_KEY")

if not endpoint or not key:
    raise SystemExit("Set AZURE_DI_ENDPOINT and AZURE_DI_KEY in .env")

client = DocumentIntelligenceClient(endpoint, AzureKeyCredential(key))


def extract_text(pdf_path):
    """Extract text from a single PDF using Azure Document Intelligence and return OCR metadata."""
    with open(pdf_path, "rb") as f:
        pdf_bytes = f.read()

    poller = client.begin_analyze_document(
        "prebuilt-read",  # using prebuilt read model
        AnalyzeDocumentRequest(bytes_source=pdf_bytes)
    )

    result = poller.result()

    text = result.content if result.content else ""
    ocr_quality_score = calculate_ocr_quality(result)
    ocr_notes = generate_ocr_notes(result)

    return text, ocr_quality_score, ocr_notes


def calculate_ocr_quality(result):
    """Assess OCR quality based on word-level confidence (0.0-1.0)."""
    if not result.pages:
        return 0.0

    confidence_scores = [
        word.confidence
        for page in result.pages if hasattr(page, 'words') and page.words
        for word in page.words if hasattr(word, 'confidence') and word.confidence is not None
    ]

    if confidence_scores:
        return round(sum(confidence_scores) / len(confidence_scores), 2)

    # fallback if no confidence metrics
    if result.content and len(result.content.strip()) > 100:
        return 0.75
    return 0.5


def generate_ocr_notes(result):
    """Generate notes about potential OCR issues."""
    notes = []
    if not result.pages:
        return "No pages detected in document"

    for i, page in enumerate(result.pages, 1):
        if hasattr(page, 'lines') and not page.lines:
            notes.append(f"Page {i} appears blank")

    if result.content and len(result.content.strip()) < 100:
        notes.append("Minimal text extracted - document may be scanned image")

    if not result.content:
        notes.append("No text content extracted")

    if len(result.pages) > 50:
        notes.append(f"Large document ({len(result.pages)} pages) - may contain mixed quality")

    return "; ".join(notes) if notes else ""


def process_all_pdfs(root_folder, output_folder, metadata_folder):
    """Process all PDFs in root_folder, save extracted text and metadata."""
    root_folder = Path(root_folder)
    output_folder = Path(output_folder)
    metadata_folder = Path(metadata_folder)

    output_folder.mkdir(parents=True, exist_ok=True)
    metadata_folder.mkdir(parents=True, exist_ok=True)

    all_pdfs = list(root_folder.rglob("*.pdf"))
    total_pdfs = len(all_pdfs)
    print(f"Found {total_pdfs} PDFs to process.\n")

    successful, failed, failed_files, quality_scores = 0, 0, [], []

    for i, pdf_path in enumerate(all_pdfs, start=1):
        case_number = pdf_path.parent.name
        print(f"[{i}/{total_pdfs}] Processing {case_number}/{pdf_path.name}")

        try:
            text, ocr_quality_score, ocr_notes = extract_text(pdf_path)
            quality_scores.append(ocr_quality_score)
            time.sleep(0.5)  # avoid throttling

            # Prepare output paths
            relative_path = pdf_path.relative_to(root_folder)
            output_path = output_folder / relative_path.with_suffix(".txt")
            metadata_path = metadata_folder / relative_path.with_suffix(".json")
            output_path.parent.mkdir(parents=True, exist_ok=True)
            metadata_path.parent.mkdir(parents=True, exist_ok=True)

            # Save text
            with open(output_path, "w", encoding="utf-8") as f:
                f.write(text)

            # Save metadata
            metadata = {
                "source_file": str(pdf_path),
                "case_number": case_number,
                "relative_path": str(relative_path),
                "ocr_quality_score": ocr_quality_score,
                "ocr_notes": ocr_notes
            }
            with open(metadata_path, "w", encoding="utf-8") as f:
                json.dump(metadata, f, indent=2)

            print(f"  Saved successfully (quality: {ocr_quality_score})\n")
            successful += 1

        except Exception as e:
            print(f"ERROR processing {pdf_path.name}: {e}\n")
            failed += 1
            failed_files.append((pdf_path, str(e)))

    # Summary
    print("Processing complete.")
    print(f"  Successful: {successful}/{total_pdfs}")
    print(f"  Failed: {failed}/{total_pdfs}")
    if quality_scores:
        avg_quality = sum(quality_scores) / len(quality_scores)
        print(f"  Average OCR quality: {avg_quality:.2f}")

    # Save error log if needed
    if failed_files:
        error_log = output_folder / "ERRORS.txt"
        with open(error_log, "w", encoding="utf-8") as f:
            f.write(f"Failed to process {len(failed_files)} files:\n\n")
            for pdf_path, error in failed_files:
                f.write(f"{pdf_path}\n  Error: {error}\n\n")
        print(f"  Error log saved: {error_log}")


if __name__ == "__main__":
    extracted_folder = r"data/test_extracted"
    output_folder = r"data/test_output"
    metadata_folder = r"data/test_metadata"

    print("STEP 2: Processing PDFs with OCR")
    print("="*70)
    process_all_pdfs(extracted_folder, output_folder, metadata_folder)
