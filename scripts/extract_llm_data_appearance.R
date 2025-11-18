### script to read Azure Document Intelligence output and extract appearance data
### reads .txt files from data/ocr output and .json metadata from data/ocr metadata
### working directory should be the main folder of the right-to-counsel-nudge repo

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

#function to extract case number from folder path
#assumes folder structure like: data/ocr output/23-2-04870-4/document.txt
extract_case_number_from_path <- function(txt_path, text_folder) {
  text_folder <- normalizePath(text_folder)
  txt_path <- normalizePath(txt_path)
  
  #get relative path from text_folder
  relative_path <- sub(paste0("^", gsub("\\\\", "/", text_folder), "/?"), "", 
                       gsub("\\\\", "/", txt_path))
  
  #extract first path component (should be case number folder)
  parts <- strsplit(relative_path, "/")[[1]]
  
  if (length(parts) > 0) {
    potential_case_num <- parts[1]
    #validate against regex: ^\d{2}-\d-\d{5}-\d$
    if (grepl("^\\d{2}-\\d-\\d{5}-\\d$", potential_case_num)) {
      return(potential_case_num)
    }
  }
  
  return(NA_character_)
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
    
    #extract case number from folder path
    folder_case_number <- extract_case_number_from_path(txt_path, text_folder)
    
    tibble(
      row_id = NA_integer_,
      direc = direc,
      document = document,
      text = text,
      source_file = metadata$source_file %||% txt_path,
      case_number = metadata$case_number %||% NA_character_,
      folder_case_number = folder_case_number,
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

#filter for appearance documents
doc_names <- coalesce(ocr_final$document, "")

doc_names_appearance <- c(
  "appearance_pro_se",
  "response_from_defendant",
  "response",
  "answer",
  "answer1",
  "defendant_s_answer",
  "defendants_answer_to_complaint",
  "defendant_s_answer_to_complaint",
  "defendant_s_response",
  "defendants_response",
  "answer_defendants_answer_to_complaint",
  "response_noa",
  "notice_of_appearance"
)

#exclude document names containing these strings (case-insensitive)
doc_names_appearance_excluded <- c(
  "answer_order_fee_unlawful_detainer_received",
  "answer_to_writ",
  "garnishment",
  "judgment_on_answer_of_garnishee_defendant",
  "reply",
  "declaration",
  "motion",
  "order",
  "certificate"
)

#inclusion mask (case-insensitive)
inc_mask <- reduce(
  doc_names_appearance,
  .init = rep(FALSE, nrow(ocr_final)),
  .f = ~ .x | str_detect(doc_names, fixed(.y, ignore_case = TRUE))
)

#exclusion mask (case-insensitive)
exc_mask <- reduce(
  doc_names_appearance_excluded,
  .init = rep(FALSE, nrow(ocr_final)),
  .f = ~ .x | str_detect(doc_names, fixed(.y, ignore_case = TRUE))
)

docs_to_run_appearance <- ocr_final[inc_mask & !exc_mask, ]

cat(sprintf("Found %d appearance documents to process\n", nrow(docs_to_run_appearance)))

#prompt
prompt_appearance <- 'You are an assistant that reads documents filed in Washington State unlawful detainer (eviction) cases and extracts fields.

Return ONLY valid raw JSON and nothing else. No prose, no code fences.

Schema:
{
  "document_file_date": "string",
  "case_number": "string",
  "appearance_for": "string",
  "appearance_pro_se": "string",
  "plaintiff_names": ["string"],
  "plaintiff_attorneys": ["string"],
  "defendant_names": ["string"],
  "defendant_attorneys": ["string"],
  "tacomaprobono_help": "string",
  "confidence": {
    "document_file_date": 0.0,
    "case_number": 0.0,
    "appearance_for": 0.0,
    "appearance_pro_se": 0.0,
    "plaintiff_names": 0.0,
    "plaintiff_attorneys": 0.0,
    "defendant_names": 0.0,
    "defendant_attorneys": 0.0,
    "tacomaprobono_help": 0.0
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
- Do NOT guess. Prefer ""/[] to an invented value.
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
- Extract WA Superior Court case number matching: ^\\d{2}-\\d-\\d{5}-\\d$.
- If digits appear with other separators (e.g., "23 2 04870 4"), normalize to "23-2-04870-4".
- If you cannot form exactly 9 digits in that pattern with high confidence, set "case_number": "".

PARTY IDENTIFICATION (appearance_for)
- Set "appearance_for" to "plaintiff" if the document describes an appearance for or on behalf of the plaintiff; "defendant" if the document describes an appearance for or on behalf of the defendant; "" if unclear.
- Prioritize, in order:
  1) Explicit phrases: "appears for Defendant", "attorney for Plaintiff", "Notice of Appearance of [Name] for Defendant/Plaintiff".
- If both sides are mentioned, prefer the side whose appearance is validated or described by the document.
- Ignore e-filing footer metadata unless it clearly states the filer role (e.g., "Filed by Attorney for Defendant").

APPEARANCE PRO SE (appearance_pro_se)
- Set "appearance_pro_se" to "Yes" if the document is an appearance by a party that they initiated as an unrepresented party.
- Set "appearance_pro_se" to "No" if the document describes an appearance by an attorney on behalf of a plaintiff or defendant.
- When possible, use phrases such as "appears for Defendant", "attorney for Plaintiff", "Notice of Appearance of [Name] for Defendant/Plaintiff".

NAMES
- Normalize personal names from "LAST, FIRST MI" → "First MI Last". Preserve hyphenated/multi-word surnames.
- Provide full personal names only; split multiple persons into separate array elements.
- Remove tokens like "aka", "dba", role labels, and credentials (e.g., "Esq.", "WA Bar No.").
- Include placeholders like "ALL OCCUPANTS" as their own array element (ASCII, uppercase). If a name is unreadable, return "".
- If a document is filed by the plaintiff, the target of the communication is likely to be on the defendant side (excluding judges or the court). If filed by the defendant, the target of the communication is likely to be on the plaintiff side (excluding judges or the court).

TACOMAPROBONO HELP
- Set "tacomaprobono_help" to "Yes" if the words "Tacoma" "pro" "bono" appear in order at any point in the document. Include instances where the words are joined. If those words are not present, set "tacomaprobono_help" to "No".

Return only the JSON object described above.'

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
to_row_appearance <- function(x) {
  #scalars
  document_file_date <- as.character(pull_field(x, "document_file_date", NA_character_))
  case_number <- as.character(pull_field(x, "case_number", NA_character_))
  appearance_for <- as.character(pull_field(x, "appearance_for", NA_character_))
  appearance_pro_se <- as.character(pull_field(x, "appearance_pro_se", NA_character_))
  tacomaprobono_help <- as.character(pull_field(x, "tacomaprobono_help", NA_character_))
  
  #arrays (ensure character vector, even if scalar was returned)
  pn <- as.character(unlist(pull_field(x, "plaintiff_names", character()), use.names = FALSE))
  pa <- as.character(unlist(pull_field(x, "plaintiff_attorneys", character()), use.names = FALSE))
  dn <- as.character(unlist(pull_field(x, "defendant_names", character()), use.names = FALSE))
  da <- as.character(unlist(pull_field(x, "defendant_attorneys", character()), use.names = FALSE))
  
  #confidence scores
  conf <- pull_field(x, "confidence", list())
  conf_doc_date <- as.numeric(pull_field(conf, "document_file_date", 0.0))
  conf_case_num <- as.numeric(pull_field(conf, "case_number", 0.0))
  conf_appearance_for <- as.numeric(pull_field(conf, "appearance_for", 0.0))
  conf_appearance_pro_se <- as.numeric(pull_field(conf, "appearance_pro_se", 0.0))
  conf_pn <- as.numeric(pull_field(conf, "plaintiff_names", 0.0))
  conf_pa <- as.numeric(pull_field(conf, "plaintiff_attorneys", 0.0))
  conf_dn <- as.numeric(pull_field(conf, "defendant_names", 0.0))
  conf_da <- as.numeric(pull_field(conf, "defendant_attorneys", 0.0))
  conf_tpb <- as.numeric(pull_field(conf, "tacomaprobono_help", 0.0))
  
  tibble(
    document_file_date = document_file_date,
    case_number = case_number,
    appearance_for = appearance_for,
    appearance_pro_se = appearance_pro_se,
    tacomaprobono_help = tacomaprobono_help,
    plaintiff_names = list(pn),
    plaintiff_attorneys = list(pa),
    defendant_names = list(dn),
    defendant_attorneys = list(da),
    conf_document_file_date = conf_doc_date,
    conf_case_number = conf_case_num,
    conf_appearance_for = conf_appearance_for,
    conf_appearance_pro_se = conf_appearance_pro_se,
    conf_plaintiff_names = conf_pn,
    conf_plaintiff_attorneys = conf_pa,
    conf_defendant_names = conf_dn,
    conf_defendant_attorneys = conf_da,
    conf_tacomaprobono_help = conf_tpb
  )
}

#function to extract data from appearance docs using prompt_appearance
parse_one <- function(txt, doc_id = NULL) {
  result <- tryCatch({
    outer <- data_from_llm(prompt_appearance, txt)
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
    to_row_appearance(inner)
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
docs_test_appearance <- slice_sample(docs_to_run_appearance, n = min(n_test, nrow(docs_to_run_appearance)))

llm_test_appearance <- map2_dfr(
  docs_test_appearance$text,
  seq_len(nrow(docs_test_appearance)),
  ~{
    cat(sprintf("Processing test document %d/%d\r", .y, nrow(docs_test_appearance)))
    res <- parse_one(.x, doc_id = docs_test_appearance$document[.y])
    if (is.null(res)) tibble()
    else bind_cols(
      tibble(row_id = docs_test_appearance$row_id[.y],
             direc    = docs_test_appearance$direc[.y],
             document = docs_test_appearance$document[.y],
             folder_case_number = docs_test_appearance$folder_case_number[.y]),
      res
    )
  }
)
cat("\n")

#check for case number conflicts in test set
test_conflicts <- llm_test_appearance %>%
  filter(!is.na(folder_case_number)) %>%
  mutate(llm_case_num = case_number) %>%
  filter(llm_case_num != "" & llm_case_num != folder_case_number) %>%
  select(row_id, direc, document, folder_case_number, llm_case_num, 
         conf_case_number)

if (nrow(test_conflicts) > 0) {
  cat(sprintf("WARNING: Found %d case number conflicts in test set:\n", nrow(test_conflicts)))
  print(test_conflicts)
} else {
  cat("No case number conflicts found in test set.\n")
}

cat(sprintf("Successfully processed %d/%d test documents\n", 
            nrow(llm_test_appearance), nrow(docs_test_appearance)))

### run on all appearance documents ###
llm_data_appearance <- map2_dfr(
  docs_to_run_appearance$text,
  seq_len(nrow(docs_to_run_appearance)),
  ~{
    if (.y %% 10 == 0) {
      cat(sprintf("Processing document %d/%d (%.1f%%)\n", 
                  .y, nrow(docs_to_run_appearance), 
                  100 * .y / nrow(docs_to_run_appearance)))
    }
    res <- parse_one(.x, doc_id = docs_to_run_appearance$document[.y])
    if (is.null(res)) tibble()
    else bind_cols(
      tibble(row_id = docs_to_run_appearance$row_id[.y],
             direc    = docs_to_run_appearance$direc[.y],
             document = docs_to_run_appearance$document[.y],
             folder_case_number = docs_to_run_appearance$folder_case_number[.y]),
      res
    )
  }
)

cat(sprintf("\nSuccessfully processed %d/%d documents\n", 
            nrow(llm_data_appearance), nrow(docs_to_run_appearance)))

#check for case number conflicts in full dataset
all_conflicts <- llm_data_appearance %>%
  filter(!is.na(folder_case_number)) %>%
  mutate(llm_case_num = case_number) %>%
  filter(llm_case_num != "" & llm_case_num != folder_case_number) %>%
  select(row_id, direc, document, folder_case_number, llm_case_num, 
         conf_case_number)

if (nrow(all_conflicts) > 0) {
  cat(sprintf("Found %d case number conflicts:\n", nrow(all_conflicts)))
  print(all_conflicts)
} else {
  cat("No case number conflicts found.\n")
}

#replace case numbers with folder case numbers when folder case number is valid
llm_data_appearance <- llm_data_appearance %>%
  mutate(
    case_number_replaced = !is.na(folder_case_number) & 
      (is.na(case_number) | case_number == "" | case_number != folder_case_number),
    case_number = if_else(!is.na(folder_case_number), folder_case_number, case_number)
  )

#report on replacements
n_replaced <- sum(llm_data_appearance$case_number_replaced, na.rm = TRUE)
cat(sprintf("\nReplaced case numbers in %d documents\n", n_replaced))

#remove temporary columns before saving
llm_data_appearance <- llm_data_appearance %>%
  select(-folder_case_number, -case_number_replaced)

saveRDS(llm_data_appearance, "data/llm_data_appearance.rds")
