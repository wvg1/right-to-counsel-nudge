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

###### fuzzy match invalid case numbers to valid ones ######

#find all potential matches within .15 threshold for each invalid case
all_potential_matches <- invalid_cases_summary %>%
  rowwise() %>%
  mutate(
    #calculate Jaro-Winkler distance to all valid cases
    distances = list(data.frame(
      valid_case = valid_cases,
      distance = stringdist(case_number, valid_cases, method = "jw")
    )),
    #filter to matches within threshold, may need to adjust threshold
    matches_within_threshold = list(
      distances[[1]] %>% 
        filter(distance < 0.15) %>%
        arrange(distance)
    )
  ) %>%
  ungroup()

#view results, expand to see all matches per invalid case
all_potential_matches_expanded <- all_potential_matches %>%
  select(case_number, n_docs, matches_within_threshold) %>%
  unnest(matches_within_threshold)

print(all_potential_matches_expanded)

match_summary <- all_potential_matches_expanded %>%
  group_by(case_number) %>%
  summarize(
    n_docs = first(n_docs),
    n_potential_matches = n(),
    closest_match = valid_case[which.min(distance)],
    closest_distance = min(distance)
  ) %>%
  arrange(desc(n_docs))

print(match_summary)

#cases with no matches in threshold
no_matches <- all_potential_matches %>%
  mutate(n_matches = nrow(matches_within_threshold[[1]])) %>%
  filter(n_matches == 0) %>%
  select(case_number, n_docs)

if (nrow(no_matches) > 0) {
  print(no_matches)
}



