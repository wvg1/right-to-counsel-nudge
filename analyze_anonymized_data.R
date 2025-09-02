### this script analyzes the final anonymized dataset from an experiment testing the effectiveness of a mailer for increasing access to WA's right to counsel program

#load packages
library(tidyverse)
library(sandwich)
library(lmtest)
library(stargazer) #if desired

#model 1
model_1 <- glm(
  hearing_held ~ treat + flag_tacoma + appearance_gap + appearance,
  data = data_for_analysis,
  family = binomial()
)

V_robust <- vcovHC(model_1, type = "HC1")
coeftest(m_logit, vcov. = V_robust)

#odds ratios with cluster-robust CIs
est <- coef(m_logit)
se  <- sqrt(diag(V_cl))
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