### this script analyzes non-public data from an experiment testing the effectiveness of a mailer for increasing access to WA's right to counsel program

#load packages
library(readxl)
library(tidyverse)

#read in data, convert appearance_date to date format
rct_data_sensitive <- read_excel(file.choose()) 

rct_data_sensitive <- rct_data_sensitive %>%
  mutate(appearance_date = as.numeric(appearance_date),
         appearance_date = as.Date(appearance_date, origin = "1899-12-30")
         ) %>% filter(!is.na(end_date))

#create two control variables (flag for Tacoma addresses, and number of days between initial appearance and hearing)
rct_data_sensitive <- rct_data_sensitive %>%
  mutate(flag_tacoma = as.integer(city == "Tacoma"),
         appearance_gap = as.integer(difftime(hearing_date, appearance_date, units = "days")),
           appearance_gap = ifelse(is.na(appearance_gap), 0L, appearance_gap)
         )

#create outcome variables (binary measure of monetary judgment, and final outcomes)
rct_data_sensitive <- rct_data_sensitive %>%
  mutate(
    monetary_judgment_binary = if_else(monetary_judgment != 0, 1, 0),
    writ_final = if_else(writ == 1 & writ_stayed_vacated == 0, 1, 0),
    dismissal_final = if_else(dismissal == 1 & dismissal_vacated == 0, 1, 0),
    old_final = if_else(old == 1 & old_vacated == 0, 1, 0)
  )

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

#create anonymized dataframe for analysis
data_for_analysis <- rct_data_sensitive %>%
  filter(!is.na(end_date))


