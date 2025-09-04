### this script analyzes the final anonymized dataset from an experiment testing the effectiveness of a mailer for increasing access to WA's right to counsel program
#before running, ensure rct_data_sensitive has been created from create_full_data.R

#load packages
library(tidyverse)
library(sandwich)
library(lmtest)
library(stargazer) #if desired

#create dataframe of completed cases
data_for_analysis <- rct_data_sensitive %>%
  select(
    case_no,
    hearing_ID,
    flag_tacoma,
    mail_date,
    hearing_date,
    appearance,
    appearance_date,
    appearance_gap,
    treat,
    hearing_held,
    hearing_att,
    rep_offered,
    writ_final,
    dismissal_final,
    old_final,
    forced_move,
    monetary_judgment,
    monetary_judgment_binary
  ) %>%
  filter(!is.na(end_date))

#if needed to remove repeat observations
data_for_analysis <- data_for_analysis %>%
  group_by(case_no) %>%
  slice_max(order_by = hearing_date, n = 1, with_ties = FALSE) %>%
  ungroup()

#model 1
model_1 <- glm(
  hearing_held ~ treat + flag_tacoma + appearance_gap + appearance,
  data = data_for_analysis,
  family = binomial()
)

V_robust <- vcovHC(model_1, type = "HC1")
ct <- coeftest(model_1, vcov. = V_robust)

#odds ratios with cluster-robust CIs
est <- coef(model_1)
se  <- sqrt(diag(V_robust))
ci  <- cbind(est - 1.96*se, est + 1.96*se)

#print summary table
model_1_results <- data.frame(
  term     = rownames(ct),
  estimate = ct[, "Estimate"],
  robust_se= ct[, "Std. Error"],
  z_value  = ct[, "z value"],
  p_value  = ct[, "Pr(>|z|)"]
) %>%
  mutate(
    OR     = exp(estimate),
    OR_lo  = exp(estimate - 1.96*robust_se),
    OR_hi  = exp(estimate + 1.96*robust_se)
  )

model_1_results
