### script to read Azure Document Intelligence output and extract hearing minute data
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

#filter for minute entry documents
docs_to_run_minute_entry <- ocr_final[grepl("minute", ocr_final$document, ignore.case = TRUE), ]

cat(sprintf("Found %d minute entry documents to process\n", nrow(docs_to_run_minute_entry)))

#prompt
prompt_minute_entry <- 'You are an assistant that reads Washington State unlawful detainer (eviction) hearing minutes and extracts fields.

Return ONLY valid raw JSON and nothing else. No prose, no code fences.

Schema:
{
  "hearing_date": "string",
  "case_number": "string",
  "plaintiff_names": ["string"],
  "plaintiff_attorneys": ["string"],
  "plaintiffs_at_hearing": ["string"],
  "plaintiff_attorneys_at_hearing": ["string"],
  "defendant_names": ["string"],
  "defendant_attorneys": ["string"],
  "defendants_at_hearing": ["string"],
  "defendant_attorneys_at_hearing": ["string"],
  "confidence": {
    "hearing_date": 0.0,
    "case_number": 0.0,
    "plaintiff_names": 0.0,
    "plaintiff_attorneys": 0.0,
    "plaintiffs_at_hearing": 0.0,
    "plaintiff_attorneys_at_hearing": 0.0,
    "defendant_names": 0.0,
    "defendant_attorneys": 0.0,
    "defendants_at_hearing": 0.0,
    "defendant_attorneys_at_hearing": 0.0
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
- Use only ASCII. Trim leading/trailing spaces; collapse internal whitespace to single spaces.
- No newlines in any field. No trailing commas in JSON.

HEARING DATE
- Normalize "hearing_date" to ISO format "YYYY-MM-DD".
- Accept common variants in source text (e.g., "9/5/24", "09/05/2024", "September 5, 2024") and normalize to "YYYY-MM-DD".
- If both a hearing date and a clerk "Entered"/"Filed" date appear, set "hearing_date" = the date the hearing actually occurred or was scheduled for.
- When multiple dates appear, choose in this priority:
  1) Date explicitly labeled "Hearing Date", "Date of Hearing", "On [date], the Court...", "In open court on [date]".
  2) Date near hearing header lines (e.g., document title block, caption block) clearly tied to the case hearing.
  3) Calendar references like "[9:00 AM] UD Calendar on [date]".
- Ignore file system timestamps, footer print dates, or e-filing metadata unless they are explicitly the hearing date.
- If the year is missing but month/day are present and a nearby 4-digit year exists in the document header/caption, use that year. Otherwise return "".

CASE NUMBER
- Extract the WA Superior Court case number matching regex: ^\\d{2}-\\d-\\d{5}-\\d$.
- If the digits appear with other separators (e.g., "23 2 04870 4"), normalize to "23-2-04870-4".
- If you cannot form exactly 9 digits in that pattern with high confidence, set "case_number": "".

NAMES
- Normalize all personal names from LAST, FIRST MI → First MI Last. Collapse internal whitespace to single spaces.
- Preserve hyphenated and multi-word last names (e.g., "De La Cruz", "Smith-Johnson").
- Provide full names only (no labels like "Plaintiff:"/"Defendant:"). Split multiple persons into separate array elements.
- Remove tokens like "aka", "dba" and keep the primary legal name.
- If a placeholder like "ALL OCCUPANTS" appears, include it as its own array element exactly as text (ASCII, uppercase).
- If a name is truly unreadable or missing, return "" and do not invent names.

ATTENDANCE / APPEARANCES
- Arrays ending with "_at_hearing" must reflect who was present at THIS hearing per the minutes (e.g., "present", "appeared", "by phone", "by Zoom").
- If attendance is ambiguous, prefer "" or [].
- Attendance arrays (defendants_at_hearing, plaintiffs_at_hearing) must be subsets of the corresponding *_names arrays, limited to those explicitly marked as present/appearing (e.g., "present", "appeared", "by phone/Zoom"). Do not omit absent parties from *_names.

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
to_row_minute_entry <- function(x) {
  #scalars
  hearing_date <- as.character(pull_field(x, "hearing_date", NA_character_))
  case_number <- as.character(pull_field(x, "case_number", NA_character_))
  
  #arrays (ensure character vector, even if scalar was returned)
  pn <- as.character(unlist(pull_field(x, "plaintiff_names", character()), use.names = FALSE))
  pa <- as.character(unlist(pull_field(x, "plaintiff_attorneys", character()), use.names = FALSE))
  pnh <- as.character(unlist(pull_field(x, "plaintiffs_at_hearing", character()), use.names = FALSE))
  pah <- as.character(unlist(pull_field(x, "plaintiff_attorneys_at_hearing", character()), use.names = FALSE))
  dn <- as.character(unlist(pull_field(x, "defendant_names", character()), use.names = FALSE))
  da <- as.character(unlist(pull_field(x, "defendant_attorneys", character()), use.names = FALSE))
  dnh <- as.character(unlist(pull_field(x, "defendants_at_hearing", character()), use.names = FALSE))
  dah <- as.character(unlist(pull_field(x, "defendant_attorneys_at_hearing", character()), use.names = FALSE))
  
  #confidence scores
  conf <- pull_field(x, "confidence", list())
  conf_hearing_date <- as.numeric(pull_field(conf, "hearing_date", 0.0))
  conf_case_num <- as.numeric(pull_field(conf, "case_number", 0.0))
  conf_pn <- as.numeric(pull_field(conf, "plaintiff_names", 0.0))
  conf_pa <- as.numeric(pull_field(conf, "plaintiff_attorneys", 0.0))
  conf_pnh <- as.numeric(pull_field(conf, "plaintiffs_at_hearing", 0.0))
  conf_pah <- as.numeric(pull_field(conf, "plaintiff_attorneys_at_hearing", 0.0))
  conf_dn <- as.numeric(pull_field(conf, "defendant_names", 0.0))
  conf_da <- as.numeric(pull_field(conf, "defendant_attorneys", 0.0))
  conf_dnh <- as.numeric(pull_field(conf, "defendants_at_hearing", 0.0))
  conf_dah <- as.numeric(pull_field(conf, "defendant_attorneys_at_hearing", 0.0))
  
  tibble(
    hearing_date = hearing_date,
    case_number = case_number,
    plaintiff_names = list(pn),
    plaintiff_attorneys = list(pa),
    plaintiffs_at_hearing = list(pnh),
    plaintiff_attorneys_at_hearing = list(pah),
    defendant_names = list(dn),
    defendant_attorneys = list(da),
    defendants_at_hearing = list(dnh),
    defendant_attorneys_at_hearing = list(dah),
    conf_hearing_date = conf_hearing_date,
    conf_case_number = conf_case_num,
    conf_plaintiff_names = conf_pn,
    conf_plaintiff_attorneys = conf_pa,
    conf_plaintiffs_at_hearing = conf_pnh,
    conf_plaintiff_attorneys_at_hearing = conf_pah,
    conf_defendant_names = conf_dn,
    conf_defendant_attorneys = conf_da,
    conf_defendants_at_hearing = conf_dnh,
    conf_defendant_attorneys_at_hearing = conf_dah
  )
}

#function to extract data from minute entry docs using prompt_minute_entry
parse_one <- function(txt, doc_id = NULL) {
  result <- tryCatch({
    outer <- data_from_llm(prompt_minute_entry, txt)
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
    to_row_minute_entry(inner)
  }, error = function(e) {
    warning(if (!is.null(doc_id)) paste("Document", doc_id, ":") else "", 
            "Error in parse_one: ", e$message)
    return(NULL)
  })
  
  return(result)
}

#run on test subset
n_test <- 15
set.seed(198)
docs_test_minute_entry <- slice_sample(docs_to_run_minute_entry, n = min(n_test, nrow(docs_to_run_minute_entry)))

llm_test_minute_entry <- map2_dfr(
  docs_test_minute_entry$text,
  seq_len(nrow(docs_test_minute_entry)),
  ~{
    cat(sprintf("Processing test document %d/%d\r", .y, nrow(docs_test_minute_entry)))
    res <- parse_one(.x, doc_id = docs_test_minute_entry$document[.y])
    if (is.null(res)) tibble()
    else bind_cols(
      tibble(row_id = docs_test_minute_entry$row_id[.y],
             direc    = docs_test_minute_entry$direc[.y],
             document = docs_test_minute_entry$document[.y],
             folder_case_number = docs_test_minute_entry$folder_case_number[.y]),
      res
    )
  }
)
cat("\n")

#check for case number conflicts in test set
test_conflicts <- llm_test_minute_entry %>%
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
            nrow(llm_test_minute_entry), nrow(docs_test_minute_entry)))

### run on all minute entry documents ###

llm_data_minute_entry <- map2_dfr(
  docs_to_run_minute_entry$text,
  seq_len(nrow(docs_to_run_minute_entry)),
  ~{
    if (.y %% 10 == 0) {
      cat(sprintf("Processing document %d/%d (%.1f%%)\n", 
                  .y, nrow(docs_to_run_minute_entry), 
                  100 * .y / nrow(docs_to_run_minute_entry)))
    }
    res <- parse_one(.x, doc_id = docs_to_run_minute_entry$document[.y])
    if (is.null(res)) tibble()
    else bind_cols(
      tibble(row_id = docs_to_run_minute_entry$row_id[.y],
             direc    = docs_to_run_minute_entry$direc[.y],
             document = docs_to_run_minute_entry$document[.y],
             folder_case_number = docs_to_run_minute_entry$folder_case_number[.y]),
      res
    )
  }
)

cat(sprintf("\nSuccessfully processed %d/%d documents\n", 
            nrow(llm_data_minute_entry), nrow(docs_to_run_minute_entry)))

#check for case number conflicts in full dataset
all_conflicts <- llm_data_minute_entry %>%
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
llm_data_minute_entry <- llm_data_minute_entry %>%
  mutate(
    case_number_replaced = !is.na(folder_case_number) & 
      (is.na(case_number) | case_number == "" | case_number != folder_case_number),
    case_number = if_else(!is.na(folder_case_number), folder_case_number, case_number)
  )

#report on replacements
n_replaced <- sum(llm_data_minute_entry$case_number_replaced, na.rm = TRUE)
cat(sprintf("\nReplaced case numbers in %d documents\n", n_replaced))

#remove temporary columns before saving
llm_data_minute_entry <- llm_data_minute_entry %>%
  select(-folder_case_number, -case_number_replaced)

saveRDS(llm_data_minute_entry, "data/llm_data_minute_entry.rds")