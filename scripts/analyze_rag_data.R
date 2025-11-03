#load packages
library(tidyverse)

#setwd

#read in data
rag_data <- read.csv("data/consolidated_cases.csv")

#arrange by case number
rag_data <- rag_data %>%
  arrange(case_number)

#select outcomes from documents other than hearing minutes
rag_data <- rag_data %>%
  mutate(across(
    c(writ, writ_stayed_vacated, monetary_judgment, dismissal, 
      dismissal_vacated, old, old_vacated, agreement_to_move),
    ~ ifelse(hearing_held_date != "", "", .x)
  ))

#collapse to case level
outcome_vars <- c("hearing_att", "rep_screened", "rep_appointed", "rep_waived", 
                  "rep_denied", "writ", "writ_stayed_vacated", "monetary_judgment", 
                  "dismissal", "dismissal_vacated", "old", "old_vacated", "agreement_to_move")

case_level_data <- rag_data %>%
  group_by(case_number) %>%
  summarise(
    # Keep earliest appearance date
    appearance_date = min(appearance_date, na.rm = TRUE),
    
    # hearing_held = TRUE if hearing_held_date is not empty for any row
    hearing_held = if_else(any(hearing_held_date != "" & !is.na(hearing_held_date)), 
                           TRUE, FALSE),
    
    # Apply function to all outcome variables
    across(all_of(outcome_vars), 
           ~if_else(any(. == "Yes", na.rm = TRUE), TRUE, FALSE),
           .names = "{.col}"),
    
    .groups = 'drop'
  )
