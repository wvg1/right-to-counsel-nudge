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
    household_ID,
    flag_tacoma,
    mail_date,
    hearing_date,
    appearance,
    appearance_date,
    response_cat,
    treat,
    hearing_held,
    hearing_att,
    rep_offered,
    writ_final,
    dismissal_final,
    old_final,
    forced_move,
    monetary_judgment,
    monetary_judgment_binary,
    end_date,
    case_length
  ) %>%
  filter(!is.na(end_date))

#label binary outcomes
binary_outcomes <- c("hearing_held",
                     "hearing_att",
                     "rep_offered",
                     "writ_final",
                     "dismissal_final",
                     "old_final",
                     "forced_move",
                     "monetary_judgment_binary"
)

#function to fit logistic regression and return effect sizes with CIs
fit_one <- function(outcome, data) {
  fml <- as.formula(paste0(outcome, " ~ treat + flag_tacoma + response_cat"))
  m   <- glm(fml, data = data, family = binomial())
  
  # Align the cluster vector to the rows actually used in the model
  mf <- model.frame(m)                              # rows kept by glm after NA handling
  idx <- as.integer(rownames(mf))                   # row indices in `data`
  cl  <- data$household_ID[idx]                     # cluster vector aligned to m’s rows
  
  V   <- sandwich::vcovCL(m, cluster = cl, type = "HC1")  # cluster-robust by household
  ct  <- lmtest::coeftest(m, vcov. = V)
  
  est <- ct["treat", "Estimate"]
  se  <- ct["treat", "Std. Error"]
  tibble(
    outcome   = outcome,
    n         = nobs(m),
    events    = sum(model.frame(m)[[outcome]] == 1, na.rm = TRUE),
    estimate  = est,
    robust_se = se,
    z         = est / se,
    p_value   = 2 * pnorm(-abs(est / se)),
    OR        = exp(est),
    OR_lo     = exp(estimate - 1.96 * se),
    OR_hi     = exp(estimate + 1.96 * se)
  )
}

#analyze hearing attendance using ITT approach
data_itt <- data_for_analysis %>%
  mutate(hearing_att = if_else(is.na(hearing_att) & hearing_held == 0, 0, hearing_att))

results_itt <- map_dfr(binary_outcomes, fit_one, data = data_itt) %>%
  mutate(spec = "ITT (NA attendance -> 0 when no hearing)")

#print all effects on binary outcomes
all_results <- bind_rows(
  results_itt) %>%
  mutate(
    outcome = recode(outcome,
                     hearing_held = "Hearing held",
                     hearing_att  = "Attendance",
                     rep_offered  = "Representation offered",
                     writ_final   = "Writ (final)",
                     dismissal_final = "Dismissal (final)",
                     old_final    = "Order of limited dissemination (final)",
                     forced_move  = "Forced move",
                     monetary_judgment_binary = "Monetary judgment (binary)"
    )
  ) %>%
  arrange(spec, outcome)

all_results

#now for continuous outcomes
fit_one_cont <- function(outcome, data, transform = c("level","log1p")) {
  transform <- match.arg(transform)
  lhs <- if (transform == "log1p") paste0("log1p(", outcome, ")") else outcome
  fml <- as.formula(paste0(lhs, " ~ treat + flag_tacoma + response_cat"))
  
  m <- lm(fml, data = data)
  
  # cluster-robust SEs by household
  mf  <- model.frame(m)
  idx <- as.integer(rownames(mf))
  cl  <- data$household_ID[idx]
  
  V  <- vcovCL(m, cluster = cl, type = "HC1")
  ct <- coeftest(m, vcov. = V)
  
  est <- ct["treat","Estimate"]
  se  <- ct["treat","Std. Error"]
  t   <- est / se
  ci_lo <- est - 1.96*se
  ci_hi <- est + 1.96*se
  
  tibble(
    outcome,
    scale      = transform,
    n          = nobs(m),
    estimate   = est,       
    robust_se  = se,
    t_value    = t,
    p_value    = 2*pnorm(-abs(t)),
    ci_lo      = ci_lo,
    ci_hi      = ci_hi,
    # Back-of-envelope back-transform for log1p: multiplicative effect on (1 + outcome)
    mult_effect_on_1plus = if (transform == "log1p") exp(est) else NA_real_
  )
}

continuous_outcomes <- c("monetary_judgment")

# Primary: levels
results_levels <- map_dfr(continuous_outcomes, ~fit_one_cont(.x, data_for_analysis, "level"))

# Optional sensitivity: log(1+y) to handle skew / outliers
results_log1p  <- map_dfr(continuous_outcomes, ~fit_one_cont(.x, data_for_analysis, "log1p"))

bind_rows(results_levels, results_log1p) %>%
  mutate(
    pretty_outcome = recode(outcome,
                            case_length = "Case length (days)",
                            monetary_judgment = "Monetary judgment ($)"
    )
  ) %>%
  arrange(pretty_outcome, scale)
