#load packages
library(tidyverse)
library(jsonlite)
library(stringdist)

#setwd

#read in data
extracted_llm_data <- read.csv("data/extracted_llm_data.csv")
#OR
extracted_llm_data <- readRDS("data/extracted_llm_data.rds")

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

###### create case-level variables #######

