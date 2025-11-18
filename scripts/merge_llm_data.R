#this script merges and collapses data extracted from OCR text of eviction documents using chatgpt. Requires .rds files created by llm data extraction scripts
###working directory should be the main folder of the eviction-data repo

#load packages
library(tidyverse)

#read in data
llm_data_complaint_clean <- readRDS("data/llm_data_complaint.rds")
llm_data_appearance_clean <- readRDS("data/llm_data_appearance.rds")
llm_data_minute_entry_clean <- readRDS("data/llm_data_minute_entry.rds")
llm_data_agreement_clean <- readRDS("data/llm_data_agreement.rds")

#bind dataframes, add source identifier
llm_data_combined <- bind_rows(
  agreement    = llm_data_agreement,
  appearance   = llm_data_appearance,
  complaint    = llm_data_complaint,
  minute_entry = llm_data_minute_entry,
  .id = "source"
)
