"""
This script extracts JSON from OCR using Azure's Open AI service.
It is designed to process text extracted via OCR from eviction case documents.
Working directory should be set as right-to-counsel-nudge folder.
"""

import os
from dotenv import load_dotenv

#load environment variables from .env file
load_dotenv()

import base64
from openai import AzureOpenAI
import json
from pathlib import Path

endpoint = os.getenv("ENDPOINT_URL")
deployment = os.getenv("DEPLOYMENT_NAME")
subscription_key = os.getenv("AZURE_OPENAI_API_KEY")

#initialize OpenAI client
if not endpoint or not subscription_key:
    raise SystemExit("Set AZURE_OPENAI_ENDPOINT and AZURE_OPENAI_KEY environment variables.")

client = AzureOpenAI(
    azure_endpoint=endpoint,
    api_key=subscription_key,
    api_version="2024-08-01-preview"  # or latest available
)

#load extraction prompt for LLM
SYSTEM_PROMPT = """You are an assistant that reads text extracted via optical character recognition from documents filed in Washington State unlawful detainer (eviction) cases filed in Pierce County. You specifically extract fields of interest from these documents.

Schema:
{
"case_number": "string",
"appearance_date": "string",
"hearing_held_date": "string",
"hearing_att": "string",
"rep_screened": "string",
"rep_appointed": "string",
"rep_waived": "string",
"rep_denied": "string",
"writ": "string",
"writ_stayed_vacated": "string",
"monetary_judgment": "string",
"dismissal": "string",
"dismissal_vacated": "string",
"old": "string",
"old_vacated": "string",
"agreement_to_move": "string"
}

STRICT RULES
- If a string field is unknown, set it to "" (empty string).
- Do NOT guess. Prefer "" to an invented value.
- Use only ASCII. Trim leading/trailing spaces; collapse internal whitespace to single spaces.
- No newlines in any field. No trailing commas in JSON.
- Return ONLY valid raw JSON and nothing else. No prose, no code fences.

CASE NUMBER
- For each document, extract WA Superior Court case number matching: ^\\d{2}-\\d-\\d{5}-\\d$.
- If digits appear with other separators (e.g., "23 2 04870 4"), normalize to "23-2-04870-4".
- If you cannot form exactly 9 digits in that pattern with high confidence, set "case_number": "".

APPEARANCE DATE
- If the document contains the text of an answer, response, or appearance document submitted by the defendant (tenant) in response to a summons, complaint or the case filed against them, set "appearance_date" to the date that the document was filed.
- Set "appearance date" based on when the document was filed in the court, rather than when it was signed.
Search priority (stop at the first match that fits):
A clerk/e-filing stamp or header with words like "FILED", "E-FILED", "ACCEPTED", "ENTERED", "RECEIVED", "SUBMITTED", near a date/time.
A docket/header watermark showing a file/entry date.
Ignore signature dates, notary acknowledgments, certificate of service/mailing dates, "DATED this …" lines, and any dates inside the narrative body.
If only a 2-digit year is present, assume 2000–2099 (e.g., 9/5/24 → 2024-09-05).
Normalize "appearance_date" to ISO "YYYY-MM-DD".
Accept common variants (e.g., "9/5/24", "09/05/2024", "September 5, 2024") and normalize to "YYYY-MM-DD".

HEARING HELD DATE
- If the document contains a summary of a hearing, description of events at a hearing, or hearing minutes from a case for a hearing that occurred, set "hearing_held_date" to the date that the hearing occurred.
Normalize "hearing_held_date" to ISO "YYYY-MM-DD".
If only a 2-digit year is present, assume 2000-2099 (e.g., 9/5/24 → 2024-09-05).
Accept common variants (e.g., "9/5/24", "09/05/2024", "September 5, 2024") and normalize to "YYYY-MM-DD".

- If the document contains a summary of a hearing, description of events at a hearing, or hearing minutes from a case for a hearing that was rescheduled, canceled, stricken, or another similar term, set "hearing_date" to "".

DEFENDANT HEARING ATTENDANCE
- If the document contains a summary of a hearing, description of events at a hearing, or hearing minutes from a case for a hearing that occurred, set "hearing_att" to "Yes" if the defendant (tenant) was in attendance at the hearing, else "No". Set "hearing_att" to "Yes" if parties attended the hearing.

DEFENDANT (TENANT) LEGAL REPRESENTATION
- If the document says that the defendant (tenant) was screened for eligibility for legal assistance from an attorney through the right to counsel program, set "rep_screened" to "Yes". If the defendant (tenant) was not screened for a court-appointed attorney, set "rep_screened" to "No."
- If the document says that the defendant (tenant) had an attorney appointed through the right to counsel program, set "rep_appointed" to "Yes". If the document does not indicate that the defendant (tenant) had an attorney appointed, set "rep_appointed" to "No."
- If the document says that the defendant (tenant) declined an attorney appointed through the right to counsel program or waived their right to an attorney, set "rep_waived" to "Yes". If the document does not indicate that the defendant (tenant) waived their right to an attorney, set "rep_waived" to "No."
- If the document says that the defendant (tenant) was not eligible or was denied an attorney through the right to counsel program, set "rep_denied" to "Yes". If the document does not indicate that the defendant (tenant) waived their right to an attorney, set "rep_denied" to "No."

WRIT OF RESTITUTION (EVICTION JUDGMENT)
 - If the document indicates that the court or judge ordered a writ of restitution (eviction judgment) to be issued in the case, set "writ" to "Yes". If the document does not indicate that the court or judge ordered a writ (for example, in a motion by one side that is not signed or approved by a judge), set "writ" to "".
 - Do not include motions that were not approved by the court or a judge when setting "writ". If the document contains text from a motion but no indication that it was approved, set "writ" to "".
 - If the document indicates that the court or judge approved an order overturning, canceling, or vacating a writ of restitution in the case, set "writ_stayed_vacated" to "Yes".
 
MONETARY JUDGMENT
- If the document indicates that the court or judge issued a ruling stating that the defendant (tenant) must pay the plaintiff (landlord), set "monetary_judgment" as the numerical sum of all fees, costs, back rent, and other monetary damages awarded in the case.

DISMISSAL
 - If the document indicates that the court or judge ordered a dismissal to be issued in the case, or that the plaintiff (landlord) or plaintiff's attorney requested a dismissal of the case, set "dismissal" to "Yes".
 - Do not include motions that were not approved by the court or a judge when setting "dismissal". If the document contains text from a motion but no indication that it was approved, set "dismissal" to "".
 - If the document indicates that the court or judge approved an order overturning, canceling, or vacating a dismissal in the case, set "dismissal_vacated" to "Yes".

ORDER FOR LIMITED DISSEMINATION
- If the document indicates that the court or judge approved an order preventing dissemination (OLD) of the case record in tenant screening files (an "Order for Limited Dissemination"), set "old" to "Yes".
- Do not include motions that were not approved by the court or a judge when setting "old". If the document contains text from a motion but no indication that it was approved, set "old" to "".
- If the document indicates that the court or judge approved an order overturning, canceling, or vacating an order for limited dissemination in the case, set "old_vacated" to "Yes".

AGREEMENT TO MOVE
- If the document indicates that the plaintiff (landlord) and defendant (tenant) came to an agreement or stipulation that the tenant would move, terminate the tenancy, or vacate the premises, set "agreement_to_move" as "Yes".
- Exclude documents summarizing a sherriff's return on a writ of restitution. For these documents, set "agreement_to_move" to "".

Return only the JSON object described above."""

#function to read one text file generated by OCR
def read_text_file(file_path):
    with open(file_path, "r", encoding="utf-8") as f:
        text = f.read()
    return text

#function to send text to Azure OpenAI and get structured JSON
def extract_json_from_text(text_content):
    response = client.chat.completions.create(
        model=deployment,  # "gpt-4o"
        messages=[
            {"role": "system", "content": SYSTEM_PROMPT},
            {"role": "user", "content": text_content}
        ],
        temperature=0, 
        response_format={"type": "json_object"}  # Force valid JSON
    )
    
    #get the JSON string from the response
    json_string = response.choices[0].message.content
    return json_string

#identify folders
input_folder = Path("data/extracted_texts")
output_folder = Path("data/extracted_json")

#create output folder if it doesn't exist already
output_folder.mkdir(exist_ok=True)

# Find all .txt files
all_text_files = list(input_folder.rglob("*.txt"))
all_text_files = [f for f in all_text_files if f.name != "ERRORS.txt"]

print(f"Found {len(all_text_files)} text files to process\n")

# Process all files
successful = 0
failed = 0

for i, text_file in enumerate(all_text_files, start=1):
    print(f"[{i}/{len(all_text_files)}] {text_file.name}")
    
    try:
        #read file
        text = read_text_file(text_file)
        
        #extract JSON
        json_string = extract_json_from_text(text)
        
        #ensure validity
        data = json.loads(json_string)
        
        #identify output path
        output_path = output_folder / text_file.relative_to(input_folder)
        output_path = output_path.with_suffix(".json")
        
        #create subfolders (if needed)
        output_path.parent.mkdir(parents=True, exist_ok=True)
        
        #save JSON file
        with open(output_path, "w", encoding="utf-8") as f:
            json.dump(data, f, indent=2)
        
        print(f"  Saved\n")
        successful += 1
        
    except Exception as e:
        print(f"  ERROR: {e}\n")
        failed += 1

#summary
print(f"  Successes: {successful}/{len(all_text_files)}")
print(f"  Failed: {failed}/{len(all_text_files)}")
print(f"  JSON saved in: {output_folder}")