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
extracted_llm_data<- extracted_llm_data %>%
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

invalid_cases_summary









#create frequency count of unique case numbers
case_frequency <- extracted_llm_data %>%
  filter(case_number != "") %>%  #exclude empty case numbers
  count(case_number, name = "n_docs") %>%
  arrange(n_docs)

case_frequency %>%
  count(n_docs, name = "n_cases") %>%
  print

#likely errors (two or fewer docs)
likely_errors <- case_frequency %>%
  filter(n_docs <= 2) %>%
  pull(case_number)

#likely valid cases
valid_cases <- case_frequency %>%
  filter(n_docs > 2) %>%
  pull(case_number)







#parse LLM confidence scores from JSON list
confidence_parsed <- extracted_llm_data %>%
  mutate(
    confidence_fixed = str_replace_all(confidence, "'", '"'),
    conf_data = map(confidence_fixed, ~fromJSON(.x, simplifyVector = TRUE))
  ) %>%
  select(X_source_file, case_folder, conf_data) %>%
  unnest_wider(conf_data)

#collect all confidence level column names
conf_cols <- setdiff(names(confidence_parsed), c("X_source_file", "case_folder"))

#ensure all confidence level columns are numeric
confidence_parsed <- confidence_parsed %>%
  mutate(across(all_of(conf_cols), ~as.numeric(unlist(.))))

#summary statistics for each confidence level column
conf_summary <- confidence_parsed %>%
  select(all_of(conf_cols)) %>%
  pivot_longer(everything(), names_to = "field", values_to = "confidence") %>%
  group_by(field) %>%
  summarise(
    mean = round(mean(confidence[confidence > 0], na.rm = TRUE), 3),
    median = round(median(confidence[confidence > 0], na.rm = TRUE), 3),
    min = round(min(confidence, na.rm = TRUE), 3),
    max = if(any(confidence > 0)) round(max(confidence[confidence > 0], na.rm = TRUE), 3) else 0,
    pct_zero = round(mean(confidence == 0, na.rm = TRUE) * 100, 1),
    pct_low = round(mean(confidence[confidence > 0] < 0.9, na.rm = TRUE) * 100, 1),
    pct_high = round(mean(confidence[confidence > 0] >= 0.9, na.rm = TRUE) * 100, 1)
  ) %>%
  arrange(desc(mean))

print(conf_summary, n = Inf)

####### clean case numbers #######
#create frequency count of unique case numbers
case_frequency <- extracted_llm_data %>%
  filter(case_number != "") %>%  #exclude empty case numbers
  count(case_number, name = "n_docs") %>%
  arrange(n_docs)

case_frequency %>%
  count(n_docs, name = "n_cases") %>%
  print
