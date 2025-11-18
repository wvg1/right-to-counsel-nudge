###this script merges and collapses data extracted from OCR text of eviction documents using an LLM.
###requires .rds files created by llm data extraction scripts
###working directory should be the main folder of the eviction-data repo

#load packages
library(tidyverse)
library(readxl)

#read in rct data
rct_data <- read_xlsx("data/pre_treatment_data.xlsx")

#read in doctype_data, rename case_number variable, change variables to logical
doctype_data <- readRDS("data/doctype_data.rds")
doctype_data <- doctype_data %>% 
  rename(case_number = case_nos) %>%
  mutate(across(
    c(appearance, hearing_held, tenant_rep, tenant_rep_denied, dismissal, 
      dismissal_vacated, writ, writ_vacated, agreement, old, old_vacated),
    ~ as.logical(.)
  ))
  
#read in llm data
llm_data_agreement <- readRDS("data/llm_data_agreement.rds")
llm_data_minute_entry <- readRDS("data/llm_data_minute_entry.rds")

#bind llm dataframes, add source identifier
combined_rag_data <- bind_rows(
  agreement    = llm_data_agreement,
  minute_entry = llm_data_minute_entry,
  .id = "source"
)

#convert hearing date to right format
combined_rag_data <- combined_rag_data %>%
  mutate(hearing_date = as.Date(hearing_date))

#NEED TO DEAL WITH CASES THAT HAVE MULTIPLE HEARINGS FOR THE SAME DAY

#create hearing-level hearing_held variable in new combined dataframe
rct_data_rag <- rct_data %>%
  left_join(
    combined_rag_data %>%
      distinct(case_number, hearing_date) %>%
      mutate(hearing_held = TRUE),
    by = c("case_number", "hearing_date")
  ) %>%
  mutate(hearing_held = coalesce(hearing_held, FALSE))

#create hearing-level defendants_present variable
rct_data_rag <- rct_data_rag %>%
  left_join(
    combined_rag_data %>%
      select(case_number, hearing_date, defendants_at_hearing) %>%
      mutate(hearing_att = lengths(defendants_at_hearing) > 0) %>%
      distinct(case_number, hearing_date, hearing_att),
    by = c("case_number", "hearing_date")
  ) %>%
  mutate(hearing_att = coalesce(hearing_att, FALSE))
  
#merge in doctype_data, aggregate to case-level (TRUE if any TRUE for that case_number)
rct_data_rag <- rct_data_rag %>%
  left_join(
    doctype_data %>%
      group_by(case_number) %>%
      summarise(
        tenant_rep = any(tenant_rep, na.rm = TRUE),
        tenant_rep_denied = any(tenant_rep_denied, na.rm = TRUE),
        dismissal = any(dismissal, na.rm = TRUE),
        dismissal_vacated = any(dismissal_vacated, na.rm = TRUE),
        writ = any(writ, na.rm = TRUE),
        writ_vacated = any(writ_vacated, na.rm = TRUE),
        agreement = any(agreement, na.rm = TRUE),
        old = any(old, na.rm = TRUE),
        old_vacated = any(old_vacated, na.rm = TRUE),
        .groups = "drop"
      ),
    by = "case_number"
  ) %>%
  mutate(
    across(
      c(tenant_rep, tenant_rep_denied, dismissal, dismissal_vacated, 
        writ, writ_vacated, agreement, old, old_vacated),
      ~ coalesce(., FALSE)
    )
  )

#merge in tenant_move from combined_rag_data, aggregate to case-level
rct_data_rag <- rct_data_rag %>%
  left_join(
    combined_rag_data %>%
      select(case_number, tenant_move) %>%
      mutate(tenant_move = tenant_move == "Yes") %>%
      group_by(case_number) %>%
      summarise(tenant_move = any(tenant_move, na.rm = TRUE), .groups = "drop"),
    by = "case_number"
  ) %>%
  mutate(tenant_move = coalesce(tenant_move, FALSE))

#create court displacement variable
rct_data_rag <- rct_data_rag %>%
  mutate(
    court_displacement = (tenant_move == TRUE) | (writ == TRUE & writ_vacated == FALSE)
  )

#final analysis data: one row per hearing_ID (n = 712)
rct_data_rag_analysis <- rct_data_rag %>%
  select(
    hearing_ID, case_number,hearing_held, hearing_att, tenant_rep, tenant_rep_denied, 
    dismissal, dismissal_vacated, writ, writ_vacated, agreement, old, old_vacated, court_displacement
  )