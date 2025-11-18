###this script merges and collapses data extracted from OCR text of eviction documents using an LLM.
###requires .rds files created by llm data extraction scripts
###working directory should be the main folder of the eviction-data repo

#load packages
library(tidyverse)
library(readxl)

#read in doctype_data
doctype_data <- readRDS("data/doctype_data.rds")

#read in llm data
llm_data_agreement <- readRDS("data/llm_data_agreement.rds")
llm_data_minute_entry <- readRDS("data/llm_data_minute_entry.rds")

#bind dataframes, add source identifier
combined_rag_data <- bind_rows(
  agreement    = llm_data_agreement,
  minute_entry = llm_data_minute_entry,
  .id = "source"
)

#load in rct data

  
#create hearing-level hearing_held_rct variable in new dataframe
rct_data_rag <- rct_data %>%
  left_join(
    combined_rag_data %>%
      distinct(case_number, hearing_date) %>%
      mutate(hearing_held = TRUE),
    by = c("case_number", "hearing_date")
  ) %>%
  mutate(hearing_held = coalesce(hearing_held, FALSE))
  
#create hearing-level hearing_att_rct variable
  
  
#create case-level appearance_before_treatment variable
  
#create case-level rep_screened
  
#create case-level rep_appointed
  
#create case-level rep_waived
  
#create case-level rep_denied
  
#create case-level writ variable
  
#create case-level writ_vacated variable

#create case-level dismissal variable

#create case-level dismissal_vacated variable

#create case-level old variable

#create case-level old_vacated variable

#create case-level court_displacement variable