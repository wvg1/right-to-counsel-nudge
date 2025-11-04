#load packages
library(tidyverse)
library(jsonlite)
library(stringdist)

#setwd

#read in data
extracted_llm_data <- read.csv("data/extracted_llm_data.csv")

###### drop rows with no valid data ######
#define fields to check
key_fields <- c("case_number", "appearance_date", "hearing_held_date", "hearing_att", 
                "rep_screened", "rep_appointed", "rep_waived", "rep_denied", 
                "writ", "writ_stayed_vacated", "monetary_judgment", "dismissal", 
                "dismissal_vacated", "old", "old_vacated", "agreement_to_move")

#drop rows where ALL key fields are blank or NA
extracted_llm_data <- extracted_llm_data %>%
  filter(!if_all(all_of(key_fields), ~is.na(.) | . == ""))

###### clean case numbers ######
#check docs with case numbers that don't match zip folders
zip_folder <- "data/RCT Case documents"

#list all zip files
zip_files <- list.files(zip_folder, pattern = "\\.zip$", full.names = FALSE)

#extract case numbers from zip file names
valid_cases <- str_remove(zip_files, "\\.zip$")
valid_cases <- sort(valid_cases)

length(valid_cases)

#identify documents with other case numbers, and summarize
invalid_cases <- extracted_llm_data %>%
  filter(!case_number %in% valid_cases)

invalid_cases_summary <- invalid_cases %>%
  group_by(case_number) %>%
  summarize(n_docs = n()) %>%
  arrange(desc(n_docs))

invalid_cases_summary %>% print(n = Inf)

###### fuzzy match invalid case numbers to valid ones ######
#create note column
extracted_llm_data <- extracted_llm_data %>% mutate(note = "")

#all valid cases
valid_cases <- str_remove(list.files("data/RCT Case documents", pattern = "\\.zip$"), "\\.zip$") %>% sort()

#apply fuzzy matching row-wise using Optimal String Alignment (OSA)
extracted_llm_data <- extracted_llm_data %>%
  rowwise() %>%
  mutate(
    final_case = if(case_number %in% invalid_cases_summary$case_number) {
      valid_cases[which.min(stringdist(case_number, valid_cases, method = "osa"))]
    } else {
      case_number
    },
    note = if(case_number != final_case) "fuzzy_corrected" else note
  ) %>%
  ungroup()
