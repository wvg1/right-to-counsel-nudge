#load packages
library(tidyverse)
library(jsonlite)

#setwd

#read in data
extracted_llm_data <- read.csv("data/extracted_llm_data.csv")

#parse LLM confidence scores from JSON list
confidence_parsed <- extracted_llm_data %>%
  mutate(
    confidence_fixed = str_replace_all(confidence, "'", '"'),
    conf_data = map(confidence_fixed, ~fromJSON(.x, simplifyVector = TRUE))
  ) %>%
  select(X_source_file, case_folder, conf_data) %>%
  unnest_wider(conf_data)

# Get all confidence column names
conf_cols <- setdiff(names(confidence_parsed), c("_source_file", "case_folder"))

cat("Confidence fields found:", length(conf_cols), "\n")
cat(paste(conf_cols, collapse = ", "), "\n\n")

# Summary statistics for each field
cat("=== CONFIDENCE SUMMARY BY FIELD ===\n")
conf_summary <- confidence_parsed %>%
  select(all_of(conf_cols)) %>%
  pivot_longer(everything(), names_to = "field", values_to = "confidence") %>%
  group_by(field) %>%
  summarise(
    mean = round(mean(confidence, na.rm = TRUE), 3),
    median = round(median(confidence, na.rm = TRUE), 3),
    min = round(min(confidence, na.rm = TRUE), 3),
    max = round(max(confidence, na.rm = TRUE), 3),
    pct_zero = round(mean(confidence == 0, na.rm = TRUE) * 100, 1),
    pct_low = round(mean(confidence < 0.5, na.rm = TRUE) * 100, 1),
    pct_high = round(mean(confidence >= 0.8, na.rm = TRUE) * 100, 1)
  ) %>%
  arrange(desc(mean))

print(conf_summary, n = Inf)