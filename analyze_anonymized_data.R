### this script analyzes the final anonymized dataset from an experiment testing the effectiveness of a mailer for increasing access to WA's right to counsel program

#load packages
library(tidyverse)
library(sandwich)

#model 1
m_logit <- glm(
  hearing_held ~ treat + flag_tacoma + appearance_gap + appearance,
  data = data_for_analysis,
  family = binomial()
)
  