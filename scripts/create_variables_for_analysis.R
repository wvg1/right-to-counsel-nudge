### this script analyzes public data from an experiment testing the effectiveness of a mailer for increasing access to WA's right to counsel program

#load packages
library(readxl)
library(tidyverse)

#read in data, double check working directory before running
rct_data_sensitive <- read_excel("rct_data_final_sensitive.xlsx")

#convert appearance_date to date format
rct_data_sensitive <- rct_data_sensitive %>%
  mutate(appearance_date = as.numeric(appearance_date),
         appearance_date = as.Date(appearance_date, origin = "1899-12-30")
  )

#convert hearing_ID and household_ID to factors
rct_data_sensitive <- rct_data_sensitive %>%
  mutate(household_ID = as.factor(household_ID),
         hearing_ID = as.factor(hearing_ID)
           )

#create numeric variables for a) number of days between hearing_date and end_date and b) number of days between appearance_date and hearing_date
rct_data_sensitive <- rct_data_sensitive %>%
  mutate(case_length = as.integer(difftime(end_date, hearing_date, units = "days")),
         appearance_gap = as.integer(difftime(hearing_date, appearance_date, units = "days")),
         appearance_gap = ifelse(is.na(appearance_gap), 0L, appearance_gap),
         appearance_before_hearing = ifelse(appearance_gap >= 1, 1L, 0L)
  )

#create flag variable for Tacoma addresses
rct_data_sensitive <- rct_data_sensitive %>%
  mutate(flag_tacoma = as.integer(city == "Tacoma"))

#normalize all existing binary measures to numeric variables
rct_data_sensitive <- rct_data_sensitive %>%
  mutate(
    across(
      c(
        flag_tacoma,
        appearance,
        appearance_provider,
        hearing_held,
        hearing_att,
        rep_screened,
        rep_appointed,
        rep_waived,
        rep_denied,
        writ,
        writ_stayed_vacated,
        dismissal,
        dismissal_vacated,
        old,
        old_vacated,
        forced_move,
        treat
      ),
      ~ as.numeric(.x))
  )

#create binary outcome variables (binary measure of monetary judgment, and final outcomes)
rct_data_sensitive <- rct_data_sensitive %>%
  mutate(
    monetary_judgment_binary = if_else(monetary_judgment != 0, 1, 0),
    writ_final = if_else(writ == 1 & writ_stayed_vacated == 0, 1, 0),
    dismissal_final = if_else(dismissal == 1 & dismissal_vacated == 0, 1, 0),
    old_final = if_else(old == 1 & old_vacated == 0, 1, 0)
  )

#create ever_treated variable for household_ID
rct_data_sensitive <- rct_data_sensitive %>%
  group_by(household_ID) %>%
  #rows are in case/chronological order within household
  arrange(is.na(mail_date), mail_date, .by_group = TRUE) %>%
  mutate(
    household_treated_by_this_case = cummax(coalesce(as.integer(treat), 0L))
  ) %>%
  ungroup()

#create household_treated_before_hearing variable for hearing_ID
rct_data_sensitive <- rct_data_sensitive %>%
  group_by(household_ID) %>%
  arrange(hearing_date, hearing_ID, .by_group = TRUE) %>%
  mutate(
    treat_bin = as.integer(coalesce(treat == 1, FALSE)),
    household_treated_before_hearing = as.integer(cummax(treat_bin))
  ) %>%
  ungroup() %>%
  select(-treat_bin)
