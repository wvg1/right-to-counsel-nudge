#load packages
library(tidyverse)
library(jsonlite)
library(stringdist)
library(writexl)

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
#count case numbers in documents vs. folder names
extracted_llm_data %>%
  summarize(
    rows = n(),
    unique_case_numbers = n_distinct(case_number),
    unique_case_number_folders = n_distinct(case_number_folder)
  )

#replace all case_number with case_number_folder
extracted_llm_data <- extracted_llm_data %>%
  mutate(case_number = case_number_folder)

#identify case_numbers that appear in multiple case_number_folders
dup_cases <- extracted_llm_data %>%
  distinct(case_number, case_number_folder) %>%
  count(case_number, name = "folder_count") %>%
  filter(folder_count > 1) %>%
  pull(case_number)

#drop any remaining duplicates in wrong folders
extracted_llm_data <- extracted_llm_data %>%
  filter(!(case_number %in% dup_cases & case_number != case_number_folder))

#now these should match
extracted_llm_data %>%
  summarize(
    rows = n(),
    unique_case_numbers = n_distinct(case_number),
    unique_case_number_folders = n_distinct(case_number_folder)
  )

###### create case-level variables #######
#create dataframe
case_level_data <- extracted_llm_data %>%
  group_by(case_number) %>%
  summarize(
    appearance_date = first(appearance_date[appearance_date != "" & !is.na(appearance_date)]),
    hearing_held_date = first(hearing_held_date[hearing_held_date != "" & !is.na(hearing_held_date)]),
    hearing_att = first(hearing_att[hearing_att != "" & !is.na(hearing_att)]),
    rep_screened = first(rep_screened[rep_screened != "" & !is.na(rep_screened)]),
    rep_appointed = first(rep_appointed[rep_appointed != "" & !is.na(rep_appointed)]),
    rep_waived = first(rep_waived[rep_waived != "" & !is.na(rep_waived)]),
    rep_denied = first(rep_denied[rep_denied != "" & !is.na(rep_denied)]),
    writ = first(writ[writ != "" & !is.na(writ)]),
    writ_stayed_vacated = first(writ_stayed_vacated[writ_stayed_vacated != "" & !is.na(writ_stayed_vacated)]),
    monetary_judgment = first(monetary_judgment[monetary_judgment != "" & !is.na(monetary_judgment)]),
    dismissal = first(dismissal[dismissal != "" & !is.na(dismissal)]),
    dismissal_vacated = first(dismissal_vacated[dismissal_vacated != "" & !is.na(dismissal_vacated)]),
    old = first(old[old != "" & !is.na(old)]),
    old_vacated = first(old_vacated[old_vacated != "" & !is.na(old_vacated)]),
    agreement_to_move = first(agreement_to_move[agreement_to_move != "" & !is.na(agreement_to_move)]),
    n_documents = n(),
    .groups = "drop"
  )

###### export excel for case outcomes ######
#select and prepare data for export
export_data <- case_level_data %>%
  select(
    case_number,
    rep_screened,
    rep_appointed,
    rep_waived,
    rep_denied,
    writ,
    writ_stayed_vacated,
    monetary_judgment,
    dismissal,
    dismissal_vacated,
    old,
    old_vacated,
    agreement_to_move
  )

# Write to Excel
write_xlsx(export_data, "data/llm_data.xlsx")
