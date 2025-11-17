###script to read Azure Document Intelligence output and extract complaint data
###reads .txt files from data/ocr output and .json metadata from data/ocr metadata
###working directory should be the main folder of the right-to-counsel-nudge repo

#set Python path
Sys.setenv(RETICULATE_PYTHON = file.path(getwd(), ".venv", "Scripts", "python.exe"))

#load required packages
library(tidyverse)
library(reticulate)
library(jsonlite)

#load virtual environment and python
venv_path <- file.path(getwd(), ".venv")
if (!dir.exists(venv_path)) {
  stop("Virtual environment not found at: ", venv_path, 
       "\nPlease ensure .venv exists in the current working directory.")
}
use_virtualenv(venv_path, required = TRUE)
py_config()
py <- import_builtins()
pickle <- import("pickle")

#define python file
source_python("scripts/data_from_llm.py")

#simple JSON validator
validate <- function(json_str) {
  tryCatch({
    fromJSON(json_str)
    return(TRUE)
  }, error = function(e) {
    return(FALSE)
  })
}

#define python file
source_python("scripts/data_from_llm.py")

#function to read OCR output files into dataframe
read_ocr_files <- function(text_folder, metadata_folder) {
  text_folder <- normalizePath(text_folder)
  metadata_folder <- normalizePath(metadata_folder)
  
  #get all .txt files from output folder
  txt_files <- list.files(
    text_folder,
    pattern = "\\.txt$",
    full.names = TRUE,
    recursive = TRUE
  )
  
  if (length(txt_files) == 0) {
    warning("No .txt files found in ", text_folder)
    return(tibble())
  }
  
  #read each file and its corresponding metadata
  ocr_data <- map_dfr(txt_files, function(txt_path) {
    #read text content
    text <- tryCatch(
      read_file(txt_path),
      error = function(e) {
        warning(paste("Failed to read", txt_path, ":", e$message))
        ""
      }
    )
    
    #construct corresponding metadata path
    relative_path <- sub(paste0("^", gsub("\\\\", "/", normalizePath(text_folder)), "/?"), "", gsub("\\\\", "/", txt_path))
    meta_path <- file.path(metadata_folder, sub("\\.txt$", ".json", relative_path))
    
    #read metadata
    metadata <- tryCatch(
      fromJSON(meta_path),
      error = function(e) {
        warning(paste("Failed to read metadata", meta_path))
        list(
          source_file = txt_path,
          case_number = NA_character_,
          relative_path = relative_path,
          ocr_quality_score = NA_real_,
          ocr_notes = ""
        )
      }
    )
    
    #extract document name from filename
    document <- basename(txt_path) %>%
      str_remove("\\.txt$")
    
    #extract directory (case folder)
    direc <- basename(dirname(txt_path))
    
    tibble(
      row_id = NA_integer_,
      direc = direc,
      document = document,
      text = text,
      source_file = metadata$source_file %||% txt_path,
      case_number = metadata$case_number %||% NA_character_,
      relative_path = metadata$relative_path %||% relative_path,
      ocr_quality_score = metadata$ocr_quality_score %||% NA_real_,
      ocr_notes = metadata$ocr_notes %||% ""
    )
  })
  
  #assign row IDs
  ocr_data <- ocr_data %>%
    mutate(row_id = row_number()) %>%
    select(row_id, direc, document, text, everything())
  
  return(ocr_data)
}

#read the OCR files
ocr_final <- read_ocr_files(
  text_folder = "data/ocr output",
  metadata_folder = "data/ocr metadata"
)

cat(sprintf("Loaded %d OCR documents\n", nrow(ocr_final)))
cat(sprintf("Average OCR quality score: %.2f\n", mean(ocr_final$ocr_quality_score, na.rm = TRUE)))

#filter for complaint documents
docs_to_run_complaint <- ocr_final[grepl("complaint", ocr_final$document, ignore.case = TRUE), ]

cat(sprintf("Found %d complaint documents to process\n", nrow(docs_to_run_complaint)))

#prompt

prompt_complaint <- 'You are an assistant that reads Washington State unlawful detainer (eviction) complaint documents and extracts fields.

Return ONLY valid raw JSON and nothing else. No prose, no code fences.

Schema:
  {
    "case_number": "string",
    "address": "string",
    "address_street": "string",
    "address_unit": "string",
    "address_city": "string",
    "address_state": "string",
    "address_zip": "string",
    "case_type": "residential" | "commercial",
    "plaintiff_names": ["string"],
    "plaintiff_attorneys": ["string"],
    "defendant_names": ["string"],
    "rent_owed": "string",
    "confidence": {
      "case_number": 0.0,
      "address": 0.0,
      "address_street": 0.0,
      "address_unit": 0.0,
      "address_city": 0.0,
      "address_state": 0.0,
      "address_zip": 0.0,
      "case_type": 0.0,
      "plaintiff_names": 0.0,
      "plaintiff_attorneys": 0.0,
      "defendant_names": 0.0,
      "rent_owed": 0.0
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
- If a STRING field is unknown, set it to "" (empty string). If an ARRAY field is unknown, set it to [].
- Do NOT guess. Prefer "" to an invented value.
- Use only ASCII. Trim leading/trailing spaces. Collapse internal whitespace to single spaces.
- No newlines in any field. No trailing commas in JSON.

CASE NUMBER
- Extract the WA Superior Court case number matching regex: ^\\d{2}-\\d-\\d{5}-\\d$.
- If the digits appear with other separators (e.g., "23 2 04870 4"), normalize to "23-2-04870-4".
- If you cannot form exactly 9 digits in that pattern with high confidence, set "case_number": "".

ADDRESS (premises for the tenancy/defendant)
- Choose the property/premises address for the tenancy/defendant (NOT the court, NOT the plaintiff firm).
- Prefer text labeled "Premises", "Property Address", "Address for service", or near "Defendant".
- Fill components:
- "address_street": house number + street name + suffix (e.g., "12517 47th Ave SW"). Do NOT include unit here.
- "address_unit": unit/suite/apt/space/lot (e.g., "Apt 3", "Unit 5", "Space #21", "Lot 12"). If none, "".
- "address_city": city name (e.g., "Lakewood"). Use title case.
- "address_state": always "WA" for Washington.
- "address_zip": 5-digit ZIP only (e.g., "98499"). If missing, leave "".
- Compose "address" exactly as:
  - If address_unit != "": "address_street address_unit, address_city, WA address_zip"
- Else: "address_street, address_city, WA address_zip"
- If only partial address is available (e.g., missing ZIP), populate known fields instead of returning all "".
- Do NOT return PO Boxes unless they are the ONLY address and explicitly the premises; otherwise prefer premises or return "".

NAMES
- Provide full names only (no labels like "Plaintiff:"/"Defendant:"). Split multiple persons into separate array elements.
- Remove tokens like "aka", "dba" and keep the primary legal name.

CASE TYPE
- "commercial" only if clearly indicated as commercial/non-residential; otherwise "residential".

RENT OWED
- Set this as the amount of rent owed to the plaintiff by the defendant, as alleged in the complaint. Do not include attorney fees or other fees unrelated to housing. If more than one relevant amount is included, return the sum of relevant amounts. ONLY INCLUDE ONE DOLLAR AMOUNT PER DOCUMENT.

Return only JSON objects following this schema.
'

#function to clean leading and trailing fences from JSON strings
clean_fences <- function(s) gsub("^```[a-zA-Z]*\\s*|\\s*```$", "", s)

#function to merge duplicate observations of the same variable
merge_dupe_vars <- function(x) {
  if (is.null(names(x))) return(x)
  nm <- names(x)
  if (!any(duplicated(nm))) return(x)
  
  out <- list()
  for (var in unique(nm)) {
    vals <- x[nm == var]
    #combine scalars & vectors into a single vector, dropping NULLs
    combined <- as.character(unlist(vals, use.names = FALSE))
    #if nothing is there, keep NULL; otherwise keep unique values
    out[[var]] <- if (length(combined)) unique(combined) else NULL
  }
  out
}

#safely pull a field by name with default
pull_field <- function(x, var, default = NULL) {
  if (!is.null(x[[var]])) x[[var]] else default
}

#convert LLM output to tibble row
to_row_complaint <- function(x) {
  #scalars
  case_number <- as.character(pull_field(x, "case_number", NA_character_))
  address <- as.character(pull_field(x, "address", NA_character_))
  address_street <- as.character(pull_field(x, "address_street", NA_character_))
  address_unit <- as.character(pull_field(x, "address_unit", NA_character_))
  address_city <- as.character(pull_field(x, "address_city", NA_character_))
  address_state <- as.character(pull_field(x, "address_state", NA_character_))
  address_zip <- as.character(pull_field(x, "address_zip", NA_character_))
  case_type <- as.character(pull_field(x, "case_type", NA_character_))
  rent_owed <- as.character(pull_field(x, "rent_owed", NA_character_))
  
  #arrays (ensure character vector, even if scalar was returned)
  pn <- as.character(unlist(pull_field(x, "plaintiff_names", character()), use.names = FALSE))
  pa <- as.character(unlist(pull_field(x, "plaintiff_attorneys", character()), use.names = FALSE))
  dn <- as.character(unlist(pull_field(x, "defendant_names", character()), use.names = FALSE))
  
  #confidence scores
  conf <- pull_field(x, "confidence", list())
  conf_case_num <- as.numeric(pull_field(conf, "case_number", 0.0))
  conf_address <- as.numeric(pull_field(conf, "address", 0.0))
  conf_address_street <- as.numeric(pull_field(conf, "address_street", 0.0))
  conf_address_unit <- as.numeric(pull_field(conf, "address_unit", 0.0))
  conf_address_city <- as.numeric(pull_field(conf, "address_city", 0.0))
  conf_address_state <- as.numeric(pull_field(conf, "address_state", 0.0))
  conf_address_zip <- as.numeric(pull_field(conf, "address_zip", 0.0))
  conf_case_type <- as.numeric(pull_field(conf, "case_type", 0.0))
  conf_rent_owed <- as.numeric(pull_field(conf, "rent_owed", 0.0))
  conf_pn <- as.numeric(pull_field(conf, "plaintiff_names", 0.0))
  conf_pa <- as.numeric(pull_field(conf, "plaintiff_attorneys", 0.0))
  conf_dn <- as.numeric(pull_field(conf, "defendant_names", 0.0))
  
  tibble(
    case_number = case_number,
    address = address,
    address_street = address_street,
    address_unit = address_unit,
    address_city = address_city,
    address_state = address_state,
    address_zip = address_zip,
    case_type = case_type,
    rent_owed = rent_owed,
    plaintiff_names = list(pn),
    plaintiff_attorneys = list(pa),
    defendant_names = list(dn),
    conf_case_number = conf_case_num,
    conf_address = conf_address,
    conf_address_street = conf_address_street,
    conf_address_unit = conf_address_unit,
    conf_address_city = conf_address_city,
    conf_address_state = conf_address_state,
    conf_address_zip = conf_address_zip,
    conf_case_type = conf_case_type,
    conf_rent_owed = conf_rent_owed,
    conf_plaintiff_names = conf_pn,
    conf_plaintiff_attorneys = conf_pa,
    conf_defendant_names = conf_dn
  )
}

#function to extract data from complaint docs using prompt_complaint
parse_one <- function(txt, doc_id = NULL) {
  result <- tryCatch({
    outer <- data_from_llm(prompt_complaint, txt)
    obj   <- tryCatch(fromJSON(outer, simplifyVector = FALSE), error = function(e) NULL)
    
    if (is.null(obj) || is.null(obj$choices) || length(obj$choices) < 1) {
      warning(if (!is.null(doc_id)) paste("Document", doc_id, ":") else "", 
              "Invalid response structure from LLM")
      return(NULL)
    }
    
    raw <- obj$choices[[1]]$message$content
    raw <- trimws(clean_fences(raw))
    
    if (!validate(raw)) {
      warning(if (!is.null(doc_id)) paste("Document", doc_id, ":") else "", 
              "LLM returned invalid JSON")
      return(NULL)
    }
    
    inner <- tryCatch(fromJSON(raw, simplifyVector = FALSE), error = function(e) NULL)
    if (is.null(inner)) {
      warning(if (!is.null(doc_id)) paste("Document", doc_id, ":") else "", 
              "Failed to parse inner JSON")
      return(NULL)
    }
    
    inner <- merge_dupe_vars(inner)
    to_row_complaint(inner)
  }, error = function(e) {
    warning(if (!is.null(doc_id)) paste("Document", doc_id, ":") else "", 
            "Error in parse_one: ", e$message)
    return(NULL)
  })
  
  return(result)
}

#run on test subset
n_test <- 15
set.seed(24)
docs_test_complaint <- slice_sample(docs_to_run_complaint, n = min(n_test, nrow(docs_to_run_complaint)))

llm_test_complaint <- map2_dfr(
  docs_test_complaint$text,
  seq_len(nrow(docs_test_complaint)),
  ~{
    cat(sprintf("Processing test document %d/%d\r", .y, nrow(docs_test_complaint)))
    res <- parse_one(.x, doc_id = docs_test_complaint$document[.y])
    if (is.null(res)) tibble()
    else bind_cols(
      tibble(row_id = docs_test_complaint$row_id[.y],
             direc    = docs_test_complaint$direc[.y],
             document = docs_test_complaint$document[.y]),
      res
    )
  }
)
cat("\n")

cat(sprintf("Successfully processed %d/%d test documents\n", 
            nrow(llm_test_complaint), nrow(docs_test_complaint)))

### run on all complaint documents, and save .rds ###

cat("\nProcessing all complaint documents...\n")
llm_data_complaint <- map2_dfr(
  docs_to_run_complaint$text,
  seq_len(nrow(docs_to_run_complaint)),
  ~{
    if (.y %% 10 == 0) {
      cat(sprintf("Processing document %d/%d (%.1f%%)\n", 
                  .y, nrow(docs_to_run_complaint), 
                  100 * .y / nrow(docs_to_run_complaint)))
    }
    res <- parse_one(.x, doc_id = docs_to_run_complaint$document[.y])
    if (is.null(res)) tibble()
    else bind_cols(
      tibble(row_id = docs_to_run_complaint$row_id[.y],
             direc    = docs_to_run_complaint$direc[.y],
             document = docs_to_run_complaint$document[.y]),
      res
    )
  }
)

cat(sprintf("\nSuccessfully processed %d/%d documents\n", 
            nrow(llm_data_complaint), nrow(docs_to_run_complaint)))

saveRDS(llm_data_complaint, "data/llm_data_complaint.rds")