###this script transforms variables in the algorithmic_data df and presents several simple analyses of the transformed dataset 
###requires the algorithmic_data df created by doctype_search.R and algorithmic_doc_search.R 
###working directory should be the main folder of the eviction-data repo

#load packages
library(tidyverse)

#read in data
doctype_data <- readRDS("data/doctype_data.rds")

#rename case number variable
doctype_data <- doctype_data %>%
  rename(
    case_number = case_nos
  )

#create binary logical indicators for most variables
doctype_data <- doctype_data %>%
  mutate(across(
    c(appearance, hearing_held, tenant_rep, tenant_rep_denied, dismissal, dismissal_vacated, 
      writ, writ_vacated, old, old_vacated),
    ~ . != 0
  ))

#create final outcome variables
doctype_data <- doctype_data %>%
  mutate(
    writ_final       = writ == TRUE & writ_vacated == FALSE,
    dismissal_final  = dismissal == TRUE & dismissal_vacated == FALSE,
    old_final        = old == TRUE & old_vacated == FALSE,
    tenant_rep_final = tenant_rep == TRUE & tenant_rep_denied == FALSE
  )

