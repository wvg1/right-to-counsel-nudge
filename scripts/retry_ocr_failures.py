"""
Retry OCR on failed documents from error log.
"""
import os
import re
import time
import json
from pathlib import Path
from dotenv import load_dotenv

from azure.core.credentials import AzureKeyCredential
from azure.ai.documentintelligence import DocumentIntelligenceClient
from azure.ai.documentintelligence.models import AnalyzeDocumentRequest

#load environment variables
load_dotenv()

#create client
endpoint = os.getenv("AZURE_DI_ENDPOINT")
key = os.getenv("AZURE_DI_KEY")

#verify endpoint format
if not endpoint or not key:
    raise SystemExit("Set AZURE_DI_ENDPOINT and AZURE_DI_KEY in .env file")

print(f"Using endpoint: {endpoint}")

client = DocumentIntelligenceClient(endpoint, AzureKeyCredential(key))

def extract_text(path):
    """Extract text from PDF using Azure Document Intelligence"""
    with open(path, "rb") as f:
        pdf_bytes = f.read()

    poller = client.begin_analyze_document(
        "prebuilt-read",
        AnalyzeDocumentRequest(bytes_source=pdf_bytes) 
    )

    result = poller.result()

    text = result.content if result.content else ""
    ocr_quality_score = calculate_ocr_quality(result)
    ocr_notes = generate_ocr_notes(result)
    
    return text, ocr_quality_score, ocr_notes

def calculate_ocr_quality(result):
    """Calculate OCR quality score from 0.0 to 1.0"""
    if not result.pages:
        return 0.0
    
    confidence_scores = []
    for page in result.pages:
        if hasattr(page, 'words') and page.words:
            for word in page.words:
                if hasattr(word, 'confidence') and word.confidence is not None:
                    confidence_scores.append(word.confidence)
    
    if confidence_scores:
        return round(sum(confidence_scores) / len(confidence_scores), 2)
    
    if result.content and len(result.content.strip()) > 100:
        return 0.75
    
    return 0.5

def generate_ocr_notes(result):
    """notes about OCR quality"""
    notes = []
    
    if not result.pages:
        return "No pages detected"
    
    for i, page in enumerate(result.pages, 1):
        if hasattr(page, 'lines') and not page.lines:
            notes.append(f"Page {i} blank")
    
    if result.content and len(result.content.strip()) < 100:
        notes.append("Minimal text - may be scanned image")
    
    if not result.content:
        notes.append("No text extracted")
    
    return "; ".join(notes) if notes else ""

def parse_error_log(error_log_path):
    """extract failed file paths from error log"""
    error_log_path = Path(error_log_path)
    
    if not error_log_path.exists():
        raise FileNotFoundError(f"Error log not found: {error_log_path}")
    
    with open(error_log_path, 'r', encoding='utf-8') as f:
        content = f.read()
    
    #extract all file paths (lines that contain .pdf)
    failed_files = []
    for line in content.split('\n'):
        if '.pdf' in line and not line.strip().startswith('Error:'):
            # Clean the path
            path = line.strip()
            if path and Path(path).exists():
                failed_files.append(Path(path))
    
    return failed_files

def retry_failed_pdfs(error_log_path, output_folder, metadata_folder, batch_size=100, delay_between_batches=60):
    """retry OCR on all failed PDFs from error log with batching and exponential backoff"""
    output_folder = Path(output_folder)
    metadata_folder = Path(metadata_folder)
    
    output_folder.mkdir(parents=True, exist_ok=True)
    metadata_folder.mkdir(parents=True, exist_ok=True)
    
    #parse error log
    print("Parsing error log...")
    failed_files = parse_error_log(error_log_path)
    total = len(failed_files)
    
    print(f"Found {total} failed files to retry\n")
    print(f"Processing in batches of {batch_size} with {delay_between_batches}s delays\n")
    
    if total == 0:
        print("No valid file paths found in error log")
        return
    
    #track progress
    successful = 0
    still_failed = 0
    still_failed_files = []
    quality_scores = []
    
    #process in batches
    for batch_num, i in enumerate(range(0, total, batch_size), start=1):
        batch = failed_files[i:i+batch_size]
        print(f"\n{'='*70}")
        print(f"BATCH {batch_num}: Processing files {i+1}-{min(i+batch_size, total)} of {total}")
        print(f"{'='*70}\n")
        
        #process each file in batch
        for j, pdf_path in enumerate(batch, start=1):
            case_folder = pdf_path.parent.name
            global_index = i + j
            
            print(f"[{global_index}/{total}] {case_folder}/{pdf_path.name}")
            
            retry_count = 0
            max_retries = 3
            
            while retry_count < max_retries:
                try:
                    #extract text
                    text, ocr_quality_score, ocr_notes = extract_text(pdf_path)
                    quality_scores.append(ocr_quality_score)
                    time.sleep(1)  # Slower rate limiting
                    
                    #determine relative path from case_folders_extracted
                    root_folder = Path("data/case_folders_extracted")
                    relative_path = pdf_path.relative_to(root_folder)
                    
                    #create output paths
                    output_path = output_folder / relative_path
                    output_path = output_path.with_suffix(".txt")
                    
                    metadata_path = metadata_folder / relative_path
                    metadata_path = metadata_path.with_suffix(".json")
                    
                    #create directories
                    output_path.parent.mkdir(parents=True, exist_ok=True)
                    metadata_path.parent.mkdir(parents=True, exist_ok=True)
                    
                    #save text
                    with open(output_path, "w", encoding="utf-8") as f:
                        f.write(text)
                    
                    #save metadata
                    metadata = {
                        "source_file": str(pdf_path),
                        "case_folder": case_folder,
                        "relative_path": str(relative_path),
                        "ocr_quality_score": ocr_quality_score,
                        "ocr_notes": ocr_notes,
                        "retry_attempt": True,
                        "retry_count": retry_count + 1
                    }
                    with open(metadata_path, "w", encoding="utf-8") as f:
                        json.dump(metadata, f, indent=2)
                    
                    print(f"✓ Success (quality: {ocr_quality_score})\n")
                    successful += 1
                    break  #success, exit retry loop
                    
                except Exception as e:
                    retry_count += 1
                    if retry_count < max_retries:
                        wait_time = 2 ** retry_count  #exponential backoff: 2, 4, 8 seconds
                        print(f"  Attempt {retry_count} failed: {str(e)[:50]}...")
                        print(f"  Retrying in {wait_time}s...")
                        time.sleep(wait_time)
                    else:
                        print(f"✗ FAILED after {max_retries} attempts: {e}\n")
                        still_failed += 1
                        still_failed_files.append((pdf_path, str(e)))
        
        #delay between batches (except after last batch)
        if i + batch_size < total:
            print(f"\nBatch complete. Waiting {delay_between_batches}s before next batch...")
            time.sleep(delay_between_batches)
    
    #print summary
    print("="*70)
    print(f"Retry complete:")
    print(f"  Successful: {successful}/{total}")
    print(f"  Still failed: {still_failed}/{total}")
    
    if quality_scores:
        avg_quality = sum(quality_scores) / len(quality_scores)
        print(f"  Average OCR quality: {avg_quality:.2f}")
    
    #save new error log for remaining failures
    if still_failed_files:
        new_error_log = output_folder / "ERRORS_RETRY.txt"
        with open(new_error_log, "w", encoding="utf-8") as f:
            f.write(f"Still failed after retry: {len(still_failed_files)} files\n\n")
            for pdf_path, error in still_failed_files:
                f.write(f"{pdf_path}\n")
                f.write(f"  Error: {error}\n\n")
        print(f"  New error log: {new_error_log}")
    else:
        print("  All files processed successfully!")

if __name__ == "__main__":
    #define paths
    error_log_path = "data/extracted_texts/ERRORS.txt"
    output_folder = "data/extracted_texts"
    metadata_folder = "data/ocr_metadata"
    
    print("OCR RETRY SCRIPT")
    print("="*70)
    print(f"Error log: {error_log_path}")
    print("Strategy: Conservative batched retry with delays")
    print()
    
    #conservative settings to avoid rate limits
    retry_failed_pdfs(
        error_log_path, 
        output_folder, 
        metadata_folder,
        batch_size=150,           
        delay_between_batches=120
    )