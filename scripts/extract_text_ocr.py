"""
This script performs OCR on a set of PDFs using Azure Document Intelligence.
PDFs should be in zipped case folders in right-to-counsel-nudge/data/RCT Case Documents.
Each zip file is extracted into its own folder named after the case number.
Set Documents/right-to-counsel-nudge as working directory before running.

"""
import os
import sys
import zipfile
import time
import json
from pathlib import Path
from dotenv import load_dotenv

from azure.core.credentials import AzureKeyCredential
from azure.ai.documentintelligence import DocumentIntelligenceClient
from azure.ai.documentintelligence.models import AnalyzeDocumentRequest

#load environment variables from .env file
load_dotenv()

endpoint = os.getenv("ENDPOINT_URL")
deployment = os.getenv("DEPLOYMENT_NAME")
subscription_key = os.getenv("AZURE_OPENAI_API_KEY")

#extract all zip files into case-specific folders
def extract_zips(zip_folder, extract_to):
    """extract zips in zip_folder to extract_to directory, each zip into its own case folder"""
    zip_folder = Path(zip_folder)
    extract_to = Path(extract_to)

    #create new directory if needed
    extract_to.mkdir(parents=True, exist_ok=True)

    #identify all zip files
    zip_files = sorted(list(zip_folder.glob("*.zip")))
    if not zip_files:
        print(f"no zip files found")
        return
    
    print(f"found {len(zip_files)} zip files\n")

    successful = 0
    failed = 0
    
    for i, zip_path in enumerate(zip_files, start=1):
        #extract case number from zip filename (e.g., "23-2-94948-1" from "23-2-94948-1.zip")
        case_number = zip_path.stem
        case_folder = extract_to / case_number
        
        print(f"[{i}/{len(zip_files)}] extracting: {zip_path.name}")
        try:
            #create case-specific folder
            case_folder.mkdir(parents=True, exist_ok=True)
            
            with zipfile.ZipFile(zip_path, 'r') as zip_ref:
                zip_ref.extractall(case_folder)
            print(f"  extracted successfully to {case_number}/\n")
            successful += 1
        except Exception as e:
            print(f"  ERROR: {e}\n")
            failed += 1

    print(f"Extraction complete")
    print(f"  Successful: {successful}/{len(zip_files)}")
    print(f"  Failed: {failed}/{len(zip_files)}")
    print(f"  All files extracted to: {extract_to}\n")   

#create document intelligence client
endpoint = os.getenv("AZURE_DI_ENDPOINT")
key = os.getenv("AZURE_DI_KEY")
if not endpoint or not key:
    raise SystemExit("Set AZURE_DI_ENDPOINT and AZURE_DI_KEY.")
client = DocumentIntelligenceClient(endpoint, AzureKeyCredential(key))

#extract text from PDFs with quality assessment
def extract_text(path):
    with open(path, "rb") as f:
        pdf_bytes = f.read()

    #send PDF to Azure
    poller = client.begin_analyze_document(
        "prebuilt-read",  #pick model
        AnalyzeDocumentRequest(bytes_source=pdf_bytes) 
    )

    result = poller.result()

    #extract text from result
    if result.content:
        text = result.content
    else:
        text = ""
    
    #calculate OCR quality score based on result properties
    ocr_quality_score = calculate_ocr_quality(result)
    ocr_notes = generate_ocr_notes(result)
    
    return text, ocr_quality_score, ocr_notes

#calculate OCR quality score from 0.0 to 1.0 using word-level confidence
def calculate_ocr_quality(result):
    """assess OCR quality based on Azure Document Intelligence word-level confidence."""
    
    if not result.pages:
        return 0.0
    
    confidence_scores = []
    
    #extract confidence from words across all pages
    for page in result.pages:
        if hasattr(page, 'words') and page.words:
            for word in page.words:
                if hasattr(word, 'confidence') and word.confidence is not None:
                    confidence_scores.append(word.confidence)
    
    #use confidence scores if available
    if confidence_scores:
        avg_confidence = sum(confidence_scores) / len(confidence_scores)
        return round(avg_confidence, 2)
    
    #fallback: estimate based on content
    if result.content and len(result.content.strip()) > 100:
        return 0.75  #assume reasonable quality if substantial text extracted
    
    return 0.5  #low confidence if no metrics available

#generate OCR quality notes
def generate_ocr_notes(result):
    """generate notes about OCR quality and potential issues"""
    notes = []
    
    if not result.pages:
        return "No pages detected in document"
    
    page_count = len(result.pages)
    
    #check for blank pages or low content
    for i, page in enumerate(result.pages, 1):
        if hasattr(page, 'lines') and not page.lines:
            notes.append(f"Page {i} appears blank")
    
    #check overall content length
    if result.content and len(result.content.strip()) < 100:
        notes.append("Minimal text extracted - document may be scanned image")
    
    if not result.content:
        notes.append("No text content extracted")
    
    if page_count > 50:
        notes.append(f"Large document ({page_count} pages) - may contain mixed quality")
    
    return "; ".join(notes) if notes else ""

#find all PDFs in case folders recursively
def process_all_pdfs(root_folder, output_folder, metadata_folder):
    """process all PDFs in case folders and save extracted text with metadata"""
    root_folder = Path(root_folder)
    output_folder = Path(output_folder)
    metadata_folder = Path(metadata_folder)
    
    #create output folders if they don't exist
    output_folder.mkdir(parents=True, exist_ok=True)
    metadata_folder.mkdir(parents=True, exist_ok=True)
    
    #find all PDFs recursively
    all_pdfs = list(root_folder.rglob("*.pdf"))
    total_pdfs = len(all_pdfs)
    
    print(f"Found {total_pdfs} PDF files")
    
    #count unique case folders
    case_folders = set(pdf.parent.name for pdf in all_pdfs)
    print(f"Found {len(case_folders)} case folders\n")
    
    #track progress 
    successful = 0
    failed = 0
    failed_files = []
    quality_scores = []
    
    #loop through each PDF 
    for i, pdf_path in enumerate(all_pdfs, start=1):
        case_number = pdf_path.parent.name
        
        print(f"[{i}/{total_pdfs}] {case_number}/{pdf_path.name}")
        
        try:
            #extract text using our function
            text, ocr_quality_score, ocr_notes = extract_text(pdf_path)
            quality_scores.append(ocr_quality_score)
            time.sleep(0.5)
            
            #create output path preserving case number folder structure
            relative_path = pdf_path.relative_to(root_folder)
            output_path = output_folder / relative_path
            output_path = output_path.with_suffix(".txt")
            
            #create metadata path
            metadata_path = metadata_folder / relative_path
            metadata_path = metadata_path.with_suffix(".json")
            
            #create subfolders if needed
            output_path.parent.mkdir(parents=True, exist_ok=True)
            metadata_path.parent.mkdir(parents=True, exist_ok=True)
            
            #save the extracted text
            with open(output_path, "w", encoding="utf-8") as f:
                f.write(text)
            
            #save metadata
            metadata = {
                "source_file": str(pdf_path),
                "case_number": case_number,
                "relative_path": str(relative_path),
                "ocr_quality_score": ocr_quality_score,
                "ocr_notes": ocr_notes
            }
            with open(metadata_path, "w", encoding="utf-8") as f:
                json.dump(metadata, f, indent=2)
            
            print(f"saved (quality: {ocr_quality_score})\n")
            successful += 1
        except Exception as e:
            #log errors and continue
            print(f"ERROR: {e}\n")
            failed += 1
            failed_files.append((pdf_path, str(e)))
    
    #print final summary
    print(f"Processing complete")
    print(f"  Successful: {successful}/{total_pdfs}")
    print(f"  Failed: {failed}/{total_pdfs}")
    print(f"  Output folder: {output_folder}")
    print(f"  Metadata folder: {metadata_folder}")
    
    if quality_scores:
        avg_quality = sum(quality_scores) / len(quality_scores)
        print(f"  Average OCR quality: {avg_quality:.2f}")
        
        #print quality distribution
        low_quality = sum(1 for score in quality_scores if score < 0.7)
        medium_quality = sum(1 for score in quality_scores if 0.7 <= score < 0.9)
        high_quality = sum(1 for score in quality_scores if score >= 0.9)
        
        print(f"\n  Quality Distribution:")
        print(f"    High (≥0.9): {high_quality} ({high_quality/len(quality_scores)*100:.1f}%)")
        print(f"    Medium (0.7-0.89): {medium_quality} ({medium_quality/len(quality_scores)*100:.1f}%)")
        print(f"    Low (<0.7): {low_quality} ({low_quality/len(quality_scores)*100:.1f}%)")
    
    #save error log if there were failures
    if failed_files:
        error_log = output_folder / "ERRORS.txt"
        with open(error_log, "w", encoding="utf-8") as f:
            f.write(f"Failed to process {len(failed_files)} files:\n\n")
            for pdf_path, error in failed_files:
                f.write(f"{pdf_path}\n")
                f.write(f"  Error: {error}\n\n")
        print(f"  Error log saved: {error_log}")
    
if __name__ == "__main__":
    #define paths
    zip_folder = r"data\RCT Case documents"
    extracted_folder = r"data\case_folders_extracted"
    output_folder = r"data\extracted_texts"
    metadata_folder = r"data\ocr_metadata"
    
    #extract all zip files
    print("STEP 1: Extracting zip files")
    print("="*70)
    extract_zips(zip_folder, extracted_folder)
    
    #process all PDFs
    print("\nSTEP 2: Processing PDFs with OCR")
    print("="*70)
    process_all_pdfs(extracted_folder, output_folder, metadata_folder)