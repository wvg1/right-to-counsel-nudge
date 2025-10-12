"""
This script performs OCR on a set of PDFs using Azure Document Intelligence.
"""

import os
import sys
from pathlib import Path

from azure.core.credentials import AzureKeyCredential
from azure.ai.documentintelligence import DocumentIntelligenceClient
from azure.ai.documentintelligence.models import AnalyzeDocumentRequest

#create document intelligence client
endpoint = os.getenv("AZURE_DI_ENDPOINT")
key = os.getenv("AZURE_DI_KEY")
if not endpoint or not key:
    raise SystemExit("Set AZURE_DI_ENDPOINT and AZURE_DI_KEY.")
client = DocumentIntelligenceClient(endpoint, AzureKeyCredential(key))

#extract text from PDFs with a defined path
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
        text = ""  #return empty string if no text
        print("No text found")
    
    return text

#find all PDFs in case folders recursively
def process_all_pdfs(root_folder, output_folder):
    """process all PDFs in case folders and save extracted text."""
    root_folder = Path(root_folder)
    output_folder = Path(output_folder)
    
    #create output folder if it doesn't exist
    output_folder.mkdir(parents=True, exist_ok=True)
    
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
    
    #loop through each PDF 
    for i, pdf_path in enumerate(all_pdfs, start=1):
        case_folder = pdf_path.parent.name
        
        print(f"[{i}/{total_pdfs}] {case_folder}/{pdf_path.name}")
        
        try:
            #extract text using our function
            text = extract_text(pdf_path)
            
            #create output path preserving folder structure
            relative_path = pdf_path.relative_to(root_folder)
            output_path = output_folder / relative_path
            output_path = output_path.with_suffix(".txt")
            
            #create subfolder if needed
            output_path.parent.mkdir(parents=True, exist_ok=True)
            
            #save the extracted text with metadata
            with open(output_path, "w", encoding="utf-8") as f:
                f.write(f"SOURCE_FILE: {pdf_path}\n")
                f.write(f"CASE_FOLDER: {case_folder}\n")
                f.write(f"RELATIVE_PATH: {relative_path}\n")
                f.write("="*70 + "\n\n")
                f.write(text)
            
            print(f"  ✓ Saved\n")
            successful += 1
            except Exception as e:
            #if anything goes wrong, log it and continue
            print(f"  ✗ ERROR: {e}\n")
            failed += 1
            failed_files.append((pdf_path, str(e)))
    
    #print final summary
    print("="*70)
    print(f"Processing complete")
    print(f"  Successful: {successful}/{total_pdfs}")
    print(f"  Failed: {failed}/{total_pdfs}")
    print(f"  Output folder: {output_folder}")
    
    #save error log if there were failures
    if failed_files:
        error_log = output_folder / "ERRORS.txt"
        with open(error_log, "w", encoding="utf-8") as f:
            f.write(f"Failed to process {len(failed_files)} files:\n\n")
            for pdf_path, error in failed_files:
                f.write(f"{pdf_path}\n")
                f.write(f"  Error: {error}\n\n")
        print(f"  Error log saved: {error_log}")
    
    print("="*70)

#run the script
if __name__ == "__main__":
    #CHANGE THESE PATHS to match your setup
    root = "path/to/your/case/folders"  # Your 700 case folders
    output = "extracted_texts"           # Where to save output
    
    #run the processing
    process_all_pdfs(root, output)