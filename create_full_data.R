### this script analyzes non-public data from an experiment testing the effectiveness of a mailer for increasing access to WA's right to counsel program

#load packages
library(readxl)
library(tidyverse)

#read in data
rct_data_sensitive <- read_excel(file.choose())

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
         appearance_gap = ifelse(is.na(appearance_gap), 0L, appearance_gap)
  )

#create two control variables (flag for Tacoma addresses, and ordinal appearance variable)
rct_data_sensitive <- rct_data_sensitive %>%
  mutate(flag_tacoma = as.integer(city == "Tacoma"),
         response_cat = case_when(
      !is.na(appearance) & appearance == 1 &
        !is.na(appearance_date) & appearance_date < as.Date(hearing_date) &
        !is.na(appearance_provider) & appearance_provider == 1 ~ "provider",
      !is.na(appearance) & appearance == 1 &
        !is.na(appearance_date) & appearance_date < as.Date(hearing_date) ~ "other",
      TRUE ~ "none"
    ) |> factor(levels = c("none","other","provider"))
  )


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
        rep_offered,
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

#review duplicate case numbers by name
name_check <- rct_data_sensitive %>%
  select(case_no, name1:name4) %>%
  pivot_longer(cols = starts_with("name"), names_to = "name_field", values_to = "tenant_name") %>%
  filter(!is.na(tenant_name) & tenant_name != "") %>%
  distinct(case_no, tenant_name) %>%
  count(tenant_name, name = "n_case_numbers") %>%
  filter(n_case_numbers > 1) %>%
  arrange(desc(n_case_numbers))

name_check

#review duplicate case numbers and addresses
rct_data_sensitive %>%
  filter(duplicated(case_no) | duplicated(case_no, fromLast = TRUE) |
           duplicated(address) | duplicated(address, fromLast = TRUE)) %>%
  arrange(address, case_no) %>%
  select(case_no, address, hearing_ID) %>%
  print(n = Inf)

#duplicate cases are in the following pairs of hearing_IDs
#(214) 523
#(451) 710
#(94) 174
#(6) 298
#(390) 616
#(1) 229
#(336) 596
#(10) 97
#(96) 135)
#(598) 676
#(588) 668
#(98) 648)
#(381) 456

#saving code to use those pairs remove all repeat tenant households
pairs <- tribble(
  ~low, ~high,
  214, 523,
  451, 710,
  94, 174,
  6, 298,
  390, 616,
  1, 229,
  336, 596,
  10,  97,
  96, 135,
  598, 676,
  588, 668,
  98, 648,
  381, 456
)

rct_data_sensitive <- rct_data_sensitive %>%
  filter(!hearing_ID %in% pairs$high)

#ethnic enclaves

#function to create surnames from full names
surname_from_full <- function(x) {
  suf <- c("JR","SR","II","III","IV","V","JR.","SR.")
  one_word_particles <- c("DE","DEL","DA","DI","DU","VAN","VON","AL","ST","ST.","AH","BIN","BINTI","LA","LE")
  two_word_particles <- list(
    c("DE","LA"), c("DE","LOS"), c("DE","LAS"),
    c("VAN","DER"), c("VAN","DEN"), c("VON","DER")
  )
  sapply(x, function(one) {
    if (is.na(one) || !nzchar(one)) return(NA_character_)
    # normalize spaces, drop commas, split
    txt <- gsub(",", " ", trimws(one))
    parts <- strsplit(txt, "\\s+")[[1]]
    # drop bare hyphens and common suffixes
    parts <- parts[!(parts %in% c("-", "–", "—"))]
    while (length(parts) && toupper(gsub("\\.", "", parts[length(parts)])) %in% suf) {
      parts <- parts[-length(parts)]
    }
    if (!length(parts)) return(NA_character_)
    n <- length(parts)
    last <- parts[n]; prev <- if (n>=2) parts[n-1] else NA
    
    #for 3+ word last names (e.g., DE LA CRUZ; VAN DER MEER)
    if (n >= 3) {
      prev2 <- parts[n-2]
      if (any(mapply(function(a,b) toupper(prev2)==a & toupper(prev)==b,
                     sapply(two_word_particles, `[`, 1),
                     sapply(two_word_particles, `[`, 2)))) {
        return(paste(prev2, prev, last))
      }
    }
    #2 word last names (e.g., DE LEON; AH SUE; VON TRAPP)
    if (!is.na(prev) && toupper(prev) %in% one_word_particles) {
      return(paste(prev, last))
    }
    #default: last token (keeps hyphens like Shaw-Ferry)
    last
  }, USE.NAMES = FALSE)
}

#apply function to name columns
rct_data_sensitive <- rct_data_sensitive %>%
  mutate(across(c(name1, name2, name3, name4),
                surname_from_name,
                .names = "{.col}_surname"))

