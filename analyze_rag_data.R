#load packages
library(tidyverse)

#setwd

#read in data
rag_data <- read.csv("data/consolidated_cases.csv")

#arrange by case number
rag_data <- rag_data %>%
  arrange(case_number)
