### script to read Azure Document Intelligence output and prepare for LLM extraction
### reads .txt files from data/ocr output and .json metadata from data/ocr metadata
### working directory should be the main folder of the eviction-data repo

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
python_script <- file.path("scripts", "data_from_llm.py")
if (!file.exists(python_script)) {
  stop("Python script not found at: ", python_script)
}
source_python(python_script)

#simple JSON validator
validate <- function(json_str) {
  tryCatch({
    fromJSON(json_str)
    return(TRUE)
  }, error = function(e) {
    return(FALSE)
  })
}

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
    # Extract just the relative path from text_folder
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
      row_id = NA_integer_,  # Will be assigned later
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

#filter for agreement documents
doc_names <- coalesce(ocr_final$document, "")

doc_names_agreement <- c(
  "cr2a",
  "agreement"
)

#exclude document names containing these strings (case-insensitive)
doc_names_agreement_excluded <- c(
  "motion",
  "lease",
  "rental",
  "declaration"
)

#inclusion mask (case-insensitive)
inc_mask <- reduce(
  doc_names_agreement,
  .init = rep(FALSE, nrow(ocr_final)),
  .f = ~ .x | str_detect(doc_names, fixed(.y, ignore_case = TRUE))
)

#exclusion mask (case-insensitive)
exc_mask <- reduce(
  doc_names_agreement_excluded,
  .init = rep(FALSE, nrow(ocr_final)),
  .f = ~ .x | str_detect(doc_names, fixed(.y, ignore_case = TRUE))
)

docs_to_run_agreement <- ocr_final[inc_mask & !exc_mask, ]

cat(sprintf("Found %d agreement documents to process\n", nrow(docs_to_run_agreement)))

#prompt
prompt_agreement <- '
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
    # Combine scalars & vectors into a single vector, dropping NULLs
    combined <- as.character(unlist(vals, use.names = FALSE))
    # If nothing is there, keep NULL; otherwise keep unique values
    out[[var]] <- if (length(combined)) unique(combined) else NULL
  }
  out
}

#safely pull a field by name with default
pull_field <- function(x, var, default = NULL) {
  if (!is.null(x[[var]])) x[[var]] else default
}

#convert LLM output to tibble row
to_row_agreement <- function(x) {
  #scalars
  document_file_date <- as.character(pull_field(x, "document_file_date", NA_character_))
  case_number <- as.character(pull_field(x, "case_number", NA_character_))
  agreement <- as.character(pull_field(x, "agreement", NA_character_))
  tenant_move <- as.character(pull_field(x, "tenant_move", NA_character_))
  
  #arrays (ensure character vector, even if scalar was returned)
  pn <- as.character(unlist(pull_field(x, "plaintiff_names", character()), use.names = FALSE))
  pa <- as.character(unlist(pull_field(x, "plaintiff_attorneys", character()), use.names = FALSE))
  dn <- as.character(unlist(pull_field(x, "defendant_names", character()), use.names = FALSE))
  da <- as.character(unlist(pull_field(x, "defendant_attorneys", character()), use.names = FALSE))
  
  #confidence scores
  conf <- pull_field(x, "confidence", list())
  conf_doc_date <- as.numeric(pull_field(conf, "document_file_date", 0.0))
  conf_case_num <- as.numeric(pull_field(conf, "case_number", 0.0))
  conf_agreement <- as.numeric(pull_field(conf, "agreement", 0.0))
  conf_tenant_move <- as.numeric(pull_field(conf, "tenant_move", 0.0))
  conf_pn <- as.numeric(pull_field(conf, "plaintiff_names", 0.0))
  conf_pa <- as.numeric(pull_field(conf, "plaintiff_attorneys", 0.0))
  conf_dn <- as.numeric(pull_field(conf, "defendant_names", 0.0))
  conf_da <- as.numeric(pull_field(conf, "defendant_attorneys", 0.0))
  
  tibble(
    document_file_date = document_file_date,
    case_number = case_number,
    agreement = agreement,
    tenant_move = tenant_move,
    plaintiff_names = list(pn),
    plaintiff_attorneys = list(pa),
    defendant_names = list(dn),
    defendant_attorneys = list(da),
    conf_document_file_date = conf_doc_date,
    conf_case_number = conf_case_num,
    conf_agreement = conf_agreement,
    conf_tenant_move = conf_tenant_move,
    conf_plaintiff_names = conf_pn,
    conf_plaintiff_attorneys = conf_pa,
    conf_defendant_names = conf_dn,
    conf_defendant_attorneys = conf_da
  )
}

#function to extract data from agreement docs using prompt_agreement
parse_one <- function(txt, doc_id = NULL) {
  result <- tryCatch({
    outer <- data_from_llm(prompt_agreement, txt)
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
    to_row_agreement(inner)
  }, error = function(e) {
    warning(if (!is.null(doc_id)) paste("Document", doc_id, ":") else "", 
            "Error in parse_one: ", e$message)
    return(NULL)
  })
  
  return(result)
}

###run on test subset ###
n_test <- 15
set.seed(2666)
docs_test_agreement <- slice_sample(docs_to_run_agreement, n = min(n_test, nrow(docs_to_run_agreement)))

cat("\nProcessing test subset...\n")
llm_test_agreement <- map2_dfr(
  docs_test_agreement$text,
  seq_len(nrow(docs_test_agreement)),
  ~{
    cat(sprintf("Processing test document %d/%d\r", .y, nrow(docs_test_agreement)))
    res <- parse_one(.x, doc_id = docs_test_agreement$document[.y])
    if (is.null(res)) tibble()
    else bind_cols(
      tibble(row_id = docs_test_agreement$row_id[.y],
             direc    = docs_test_agreement$direc[.y],
             document = docs_test_agreement$document[.y]),
      res
    )
  }
)
cat("\n")

cat(sprintf("Successfully processed %d/%d test documents\n", 
            nrow(llm_test_agreement), nrow(docs_test_agreement)))

### run on all agreement documents, and save .rds ###
cat("\nProcessing all agreement documents...\n")
llm_data_agreement <- map2_dfr(
  docs_to_run_agreement$text,
  seq_len(nrow(docs_to_run_agreement)),
  ~{
    if (.y %% 10 == 0) {
      cat(sprintf("Processing document %d/%d (%.1f%%)\n", 
                  .y, nrow(docs_to_run_agreement), 
                  100 * .y / nrow(docs_to_run_agreement)))
    }
    res <- parse_one(.x, doc_id = docs_to_run_agreement$document[.y])
    if (is.null(res)) tibble()
    else bind_cols(
      tibble(row_id = docs_to_run_agreement$row_id[.y],
             direc    = docs_to_run_agreement$direc[.y],
             document = docs_to_run_agreement$document[.y]),
      res
    )
  }
)

cat(sprintf("\nSuccessfully processed %d/%d documents\n", 
            nrow(llm_data_agreement), nrow(docs_to_run_agreement)))

saveRDS(llm_data_agreement, "data/llm_data_agreement.rds")