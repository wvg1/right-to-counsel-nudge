### this script analyzes the final dataset from an experiment testing the effectiveness of a mailer for increasing access to WA's right to counsel program
#setwd as right-to-counsel-nudge prior to running

#load packages
library(tidyverse)
library(readxl)
library(sandwich)
library(lmtest)

#read in data
data_for_analysis <- read_xlsx("data/final_data_for_analysis.xlsx")

#mutate binary variables to logical
data_for_analysis <- data_for_analysis %>%
  mutate(across(c(appearance, appearance_provider, treat, hearing_held, hearing_att, rep_screened, 
                  rep_appointed, rep_waived, rep_denied, writ, writ_vacated, 
                  dismissal, dismissal_vacated, old, 
                  old_vacated, court_displacement),
                ~as.logical(.)))

#mutate appearance_date variable, warnings just indicate presence of NAs (expected)
data_for_analysis <- data_for_analysis %>%
  mutate(appearance_date = as.POSIXct(as.Date(as.numeric(appearance_date), 
                                              origin = "1899-12-30")))

#create flag for tacoma addresses (flag_tacoma)
data_for_analysis <- data_for_analysis %>%
  mutate(flag_tacoma = city == "Tacoma")

#create hearing-level variable for appearance before hearing (appearance_before_hearing)
data_for_analysis <- data_for_analysis %>%
  mutate(appearance_before_hearing = if_else(
    is.na(appearance_date),
    FALSE,
    appearance_date < hearing_date
  ))

#create hearing-level variable indicating prior household treatment (household_treated_by_this_hearing) 
data_for_analysis <- data_for_analysis %>%
  arrange(household_ID, hearing_date) %>%
  group_by(household_ID) %>%
  mutate(
    household_treated_by_this_hearing = cumsum(treat == 1 & mail_date < hearing_date) > 0
  ) %>%
  ungroup()

#what are the duplicate household_IDs?
dup_households <- data_for_analysis %>%
  count(household_ID, name = "n_rows") %>%
  filter(n_rows > 1) %>%
  arrange(desc(n_rows))

dup_households

#do any tenant households have multiple cases?
multi_case_households <- data_for_analysis %>%
  filter(household_ID %in% dup_households$household_ID) %>%
  group_by(household_ID) %>%
  summarise(
    n_cases = n_distinct(case_no, na.rm = TRUE),
    cases   = paste(sort(unique(na.omit(case_no))), collapse = "; "),
    .groups = "drop"
  ) %>%
  filter(n_cases > 1) %>%
  arrange(desc(n_cases))

multi_case_households

###begin analysis ###

#quick balancing test for all hearings
with(data_for_analysis, chisq.test(table(household_treated_by_this_hearing, flag_tacoma)))
with(data_for_analysis, chisq.test(table(household_treated_by_this_hearing, appearance_before_hearing)))

#model 1: effect of any household-level treatment on hearing_held
m1 <- glm(hearing_held ~ household_treated_by_this_hearing + appearance_before_hearing + flag_tacoma, 
          data = data_for_analysis, 
          family = binomial())

V1 <- vcovCL(m1, cluster = data_for_analysis$household_ID, type = "HC1")
ct1 <- coeftest(m1, vcov. = V1)

#odds ratios
m1_est <- ct1["household_treated_by_this_hearingTRUE", "Estimate"]
m1_se <- ct1["household_treated_by_this_hearingTRUE", "Std. Error"]
m1_or <- exp(m1_est)
m1_or_lo <- exp(m1_est - 1.96 * m1_se)
m1_or_hi <- exp(m1_est + 1.96 * m1_se)

#model 2: effect of treatment on hearing attendance (all cases)

#model 2: effect of treatment on hearing attendance (all cases, NAs as 0)
#first create dataframe where hearing_att = 0 if no hearing
data_for_m2 <- data_for_analysis %>%
  mutate(hearing_att = if_else(is.na(hearing_att), FALSE, hearing_att))

m2 <- glm(hearing_att ~ household_treated_by_this_hearing + appearance_before_hearing + flag_tacoma, 
          data = data_for_m2, 
          family = binomial())

V2 <- vcovCL(m2, cluster = data_for_m2$household_ID, type = "HC1")
ct2 <- coeftest(m2, vcov. = V2)

m2_est <- ct2["household_treated_by_this_hearingTRUE", "Estimate"]
m2_se <- ct2["household_treated_by_this_hearingTRUE", "Std. Error"]
m2_or <- exp(m2_est)
m2_or_lo <- exp(m2_est - 1.96 * m2_se)
m2_or_hi <- exp(m2_est + 1.96 * m2_se)

#model 3: effect of treatment on hearing attendance (conditional on hearing held)
m3 <- glm(hearing_att ~ household_treated_by_this_hearing + appearance_before_hearing + flag_tacoma, 
          data = data_for_analysis %>% filter(hearing_held == TRUE), 
          family = binomial())

V3 <- vcovCL(m3, cluster = data_for_analysis %>% filter(hearing_held == TRUE) %>% pull(household_ID), type = "HC1")
ct3 <- coeftest(m3, vcov. = V3)

#odds ratios
m3_est <- ct3["household_treated_by_this_hearingTRUE", "Estimate"]
m3_se <- ct3["household_treated_by_this_hearingTRUE", "Std. Error"]
m3_or <- exp(m3_est)
m3_or_lo <- exp(m3_est - 1.96 * m3_se)
m3_or_hi <- exp(m3_est + 1.96 * m3_se)

#combine results for plotting (m1 and m2 only)
results <- bind_rows(
  tibble(outcome = "Hearing held", or = m1_or, or_lo = m1_or_lo, or_hi = m1_or_hi),
  tibble(outcome = "Attendance", or = m2_or, or_lo = m2_or_lo, or_hi = m2_or_hi)
) %>%
  mutate(outcome = factor(outcome, levels = c("Hearing held", "Attendance")))

#plot treatment effects
ggplot(results, aes(x = or, y = outcome)) +
  geom_point(size = 2, color = "steelblue") +
  geom_errorbarh(aes(xmin = or_lo, xmax = or_hi),
                 height = 0.15, linewidth = 0.8, color = "steelblue") +
  geom_vline(xintercept = 1, linetype = "dashed", color = "gray40") +
  scale_x_log10(breaks = c(0.5, 1, 2, 3), limits = c(0.4, 3.5)) +
  labs(x = "Odds Ratio (log scale)", y = NULL, 
       title = "Treatment Effects on Hearing Outcomes") +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", hjust = 0, margin = margin(b = 10)))

#create table with all three models, including sample sizes and p-values
results_table <- bind_rows(
  tibble(outcome = "Hearing held", 
         n = nobs(m1),
         events = sum(model.frame(m1)$hearing_held == 1, na.rm = TRUE),
         or = m1_or, or_lo = m1_or_lo, or_hi = m1_or_hi,
         p = 2 * pnorm(-abs(m1_est / m1_se))),
  tibble(outcome = "Attendance (all cases)",
         n = nobs(m2),
         events = sum(model.frame(m2)$hearing_att == 1, na.rm = TRUE),
         or = m2_or, or_lo = m2_or_lo, or_hi = m2_or_hi,
         p = 2 * pnorm(-abs(m2_est / m2_se))),
  tibble(outcome = "Attendance (conditional)",
         n = nobs(m3),
         events = sum(model.frame(m3)$hearing_att == 1, na.rm = TRUE),
         or = m3_or, or_lo = m3_or_lo, or_hi = m3_or_hi,
         p = 2 * pnorm(-abs(m3_est / m3_se)))
) %>%
  mutate(
    Outcome = outcome,
    N = n,
    Events = events,
    `Odds Ratio` = round(or, 3),
    `95% CI` = paste0("[", round(or_lo, 3), ", ", round(or_hi, 3), "]"),
    `p-value` = round(p, 3)
  ) %>%
  select(Outcome, N, Events, `Odds Ratio`, `95% CI`, `p-value`)

#print table
print(results_table)








#function to fit logistic regression using household-level treatment variable and return effect sizes with CIs
fit_one <- function(outcome, data) {
  fml <- as.formula(paste0(outcome, " ~ household_treated_by_this_hearing + flag_tacoma + appearance_before_hearing"))
  m   <- glm(fml, data = data, family = binomial())
  
  #align the cluster vector to the rows actually used in the model
  mf <- model.frame(m)                              
  idx <- as.integer(rownames(mf))                   
  cl  <- data$case_no[idx]                     
  
  V   <- vcovCL(m, cluster = cl, type = "HC1")  # cluster-robust by case
  ct  <- coeftest(m, vcov. = V)
  
  est <- ct["household_treated_by_this_hearing", "Estimate"]
  se  <- ct["household_treated_by_this_hearing", "Std. Error"]
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

#label hearing outcomes
hearing_outcomes <- c("hearing_held",
                     "hearing_att"
)

#analyze ITT effects on hearing outcomes
data_itt <- data_for_analysis %>%
  mutate(hearing_att = if_else(is.na(hearing_att) & hearing_held == 0, 0, hearing_att))

results_itt <- map_dfr(hearing_outcomes, fit_one, data = data_itt) %>%
  mutate(spec = "ITT (NA attendance -> 0 when no hearing)")

results_itt

#fit models on the same data used for ITT
mods <- setNames(lapply(hearing_outcomes, function(y) {
  glm(
    reformulate(
      c("household_treated_before_hearing", "flag_tacoma", "appearance_before_hearing"),
      response = y
    ),
    data = data_itt, family = binomial()
  )
}), hearing_outcomes)

#average marginal effects of the correct treatment term, with clustered SEs
ame_tab <- imap_dfr(mods, function(m, y) {
  ame <- avg_slopes(m,
                    variables = "household_treated_before_hearing",
                    vcov = ~ case_no)
  tibble(
    outcome = y,                          
    AME_pp  = 100 * ame$estimate,        
    AME_lo  = 100 * ame$conf.low,
    AME_hi  = 100 * ame$conf.high
  )
})

#join directly by raw outcome codes
report_full <- results_itt %>%
  left_join(ame_tab, by = "outcome")

report_full %>%
  select(-spec) %>%
  print(n = Inf)

#plot ITT effects on hearing outcomes
#map codes to labels
label_map <- c(
  hearing_held = "Hearing held",
  hearing_att  = "Attendance"
)

results_itt %>%
  filter(outcome %in% names(label_map)) %>%
  mutate(outcome_label = recode(outcome, !!!label_map),
         outcome_label = factor(outcome_label,
                                levels = c("Hearing held", "Attendance"))) %>%
  ggplot(aes(x = OR, y = outcome_label)) +
  geom_point(size = 2, color = "steelblue") +
  geom_errorbarh(aes(xmin = OR_lo, xmax = OR_hi),
                 height = 0.15, linewidth = 0.8, color = "steelblue") +
  geom_vline(xintercept = 1, linetype = "dashed", color = "gray40") +
  scale_x_log10(
    breaks = c(0.5, 1, 2, 3),
    limits = c(0.4, 3.5),
    expand = expansion(mult = c(0.02, 0.06))   # add padding inside panel
  ) +
  scale_y_discrete(labels = function(x) sub("\\s", "\n", x)) +
  labs(
    x = "Odds Ratio (log scale)",
    y = NULL,
    title = "Treatment Effects on Hearing Outcomes"
  ) +
  theme_minimal(base_size = 10, base_family = "serif") +
  theme(
    panel.grid.minor = element_blank(),
    axis.text.y  = element_text(size = 10, margin = margin(r = 8)),
    axis.text.x  = element_text(margin = margin(t = 6)),               
    axis.title.x = element_text(margin = margin(t = 10)),              
    plot.title   = element_text(size = 12, face = "bold", hjust = 0,
                                margin = margin(b = 12)),               
    plot.margin  = margin(t = 4, r = 6, b = 4, l = 4)              
  )

#analyze CATE effects on hearing outcomes
hearing_outcomes_cate <- c("hearing_att", "rep_screened")
data_cate <- data_for_analysis %>% filter(hearing_held == 1)

results_cate <- map_dfr(hearing_outcomes_cate, fit_one, data = data_cate) %>%
  mutate(spec = "CATE (subset: hearing held)")

mods_cate <- setNames(lapply(hearing_outcomes_cate, function(y) {
  glm(reformulate(c("household_treated_before_hearing",
                    "flag_tacoma", "appearance_before_hearing"),
                  response = y),
      data = data_cate, family = binomial())
}), hearing_outcomes_cate)

ame_cate <- imap_dfr(mods_cate, function(m, y) {
  ame <- avg_slopes(
    m,
    variables = "household_treated_before_hearing",
    vcov = ~ case_no
  )
  tibble(
    outcome = y,
    AME_pp  = 100 * ame$estimate,
    AME_lo  = 100 * ame$conf.low,
    AME_hi  = 100 * ame$conf.high
  )
})

report_cate <- results_cate %>%
  left_join(ame_cate, by = "outcome")

report_cate %>%
  select(-spec) %>%
  print(n = Inf)

#analyze ITT effects on binary case outcomes
binary_case_outcomes <- c(
                     "rep_screened",
                     "rep_appointed",
                     "writ_final",
                     "dismissal_final",
                     "old_final",
                     "forced_move",
                     "monetary_judgment_binary"
)

#function to fit logistic regressions and return effect sizes with CIs
fit_one <- function(outcome, data) {
  fml <- as.formula(paste0(outcome, " ~ household_treated_by_this_case + flag_tacoma + appearance_before_hearing"))
  m   <- glm(fml, data = data, family = binomial())
  
  # align the cluster vector to the rows actually used in the model
  mf <- model.frame(m)                              # rows kept by glm after NA handling
  idx <- as.integer(rownames(mf))                   # row indices in `data`
  cl  <- data$household_ID[idx]                     # cluster vector aligned to m’s rows
  
  V   <- sandwich::vcovCL(m, cluster = cl, type = "HC1")  # cluster-robust by household
  ct  <- lmtest::coeftest(m, vcov. = V)
  
  est <- ct["household_treated_by_this_case", "Estimate"]
  se  <- ct["household_treated_by_this_case", "Std. Error"]
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

#analyze binary case outcomes using ITT approach
data_itt_case_outcomes <- data_for_analysis %>%
  mutate(hearing_att = if_else(is.na(hearing_att) & hearing_held == 0, 0, hearing_att))

results_itt_case_outcomes <- map_dfr(binary_case_outcomes, fit_one, data = data_itt_case_outcomes) %>%
  mutate(spec = "ITT (NA attendance -> 0 when no hearing)")

#print all effects on binary outcomes
case_outcome_results <- bind_rows(
  results_itt_case_outcomes) %>%
  mutate(
    outcome = recode(outcome,
                     writ_final   = "Writ issued",
                     dismissal_final = "Dismissal",
                     old_final    = "Order of limited dissemination issued",
                     forced_move  = "Forced move",
                     monetary_judgment_binary = "Monetary judgment issued",
                     rep_screened = "Screened for representation",
                     rep_appointed = "Representation appointed"
    )
  ) %>%
  arrange(spec, outcome)

case_outcome_results

#refit and save logistic regressions
mods <- setNames(lapply(binary_case_outcomes, function(y) {
  glm(as.formula(paste0(y, " ~ household_treated_by_this_case + flag_tacoma + appearance_before_hearing")),
      data = data_itt_case_outcomes, family = binomial())
}), binary_case_outcomes)

#use clustered SEs directly inside marginaleffects
ame_tab <- imap_dfr(mods, function(m, y) {
  ame <- avg_slopes(m, variables = "household_treated_by_this_case", vcov = ~ case_no)
  tibble(outcome = y,
         AME_pp = 100 * ame$estimate,
         AME_lo = 100 * ame$conf.low,
         AME_hi = 100 * ame$conf.high)
})

#map from raw outcome var -> pretty label used in all_results$outcome
outcome_map <- tibble(
  outcome_raw = c(
    "dismissal_final",
    "writ_final",
    "old_final",
    "monetary_judgment_binary",
    "forced_move",
    "rep_screened",
    "rep_appointed"
  ),
  outcome_label = c(
    "Dismissal",
    "Writ issued",
    "Order of limited dissemination issued",
    "Monetary judgment issued",
    "Forced move",
    "Screened for representation",
    "Representation appointed"
  )
)

#recode AME table to the same labels, then join
ame_tab_labeled <- ame_tab %>%
  rename(outcome_raw = outcome) %>%
  left_join(outcome_map, by = "outcome_raw") %>%
  transmute(outcome = outcome_label, AME_pp, AME_lo, AME_hi)

report_full_case_outcomes <- case_outcome_results %>%
  left_join(ame_tab_labeled, by = "outcome")

report_full_case_outcomes %>%
  select(-spec) %>%
  print(n = Inf)

fit_one_continuous <- function(outcome, data) {
  fml <- as.formula(paste0(outcome, " ~ household_treated_by_this_case + flag_tacoma + appearance_before_hearing"))
  m   <- lm(fml, data = data)
  
  #align the cluster vector to the rows actually used in the model
  mf <- model.frame(m)                              
  idx <- as.integer(rownames(mf))                   
  cl  <- data$household_ID[idx]                     
  
  V   <- sandwich::vcovCL(m, cluster = cl, type = "HC1")
  ct  <- lmtest::coeftest(m, vcov. = V)
  
  est <- ct["household_treated_by_this_case", "Estimate"]
  se  <- ct["household_treated_by_this_case", "Std. Error"]
  tibble(
    outcome   = outcome,
    n         = nobs(m),
    estimate  = est,
    robust_se = se,
    t_stat    = est / se,
    p_value   = 2 * pt(-abs(est / se), df = nobs(m) - 1),
    ci_lo     = est - 1.96 * se,
    ci_hi     = est + 1.96 * se
  )
}

#analyze treatment effect on monetary_judgment
results_monetary <- fit_one_continuous("monetary_judgment", data = data_itt_case_outcomes)

results_monetary %>%
  print(n = Inf)

#plot ITT effects on case outcomes
#map codes to labels
label_map_case_outcomes <- c(
  writ_final   = "Writ issued",
  dismissal_final = "Dismissal",
  old_final    = "Order of limited dissemination issued",
  forced_move  = "Forced move",
  monetary_judgment_binary = "Monetary judgment issued",
  rep_appointed = "Representation appointed",
  rep_screened = "Screened for representation"
)

#define base order
base_lvls <- c(
  "Writ issued", "Dismissal",
  "Order of limited dissemination issued",
  "Forced move", "Monetary judgment issued",
  "Representation appointed", "Screened for representation"
)

#interleave invisible spacer levels (one between each real level)
spaced_lvls <- c(rbind(base_lvls, paste0("___spacer_", seq_along(base_lvls))))
spaced_lvls <- spaced_lvls[-length(spaced_lvls)]  # drop trailing spacer

results_itt_case_outcomes %>%
  filter(outcome %in% names(label_map_case_outcomes)) %>%
  mutate(
    outcome_label = recode(outcome, !!!label_map_case_outcomes),
    outcome_label = factor(outcome_label, levels = base_lvls)
  ) %>%
  ggplot(aes(x = OR, y = outcome_label)) +
  geom_point(size = 2, color = "steelblue") +
  geom_errorbarh(aes(xmin = OR_lo, xmax = OR_hi),
                 height = 0.1, linewidth = 0.8, color = "steelblue") +
  geom_vline(xintercept = 1, linetype = "dashed", color = "gray40") +
  scale_x_log10(breaks = c(0.5, 1, 2, 3), limits = c(0.4, 3.5)) +
  scale_y_discrete(
    limits = spaced_lvls,                             # inserts blank rows between categories
    breaks = base_lvls,                               # only label real categories
    labels = function(x) str_wrap(x, width = 22)
  ) +
  labs(
    x = "Odds Ratio (log scale)",
    y = NULL,
    title = "Treatment Effects on Case Outcomes"
  ) +
  theme_minimal(base_size = 10, base_family = "serif") +
  theme(
    panel.grid.minor = element_blank(),
    axis.text.y = element_text(size = 10, margin = margin(r = 8)),
    axis.text.x = element_text(margin = margin(t = 6)),
    axis.title.x = element_text(margin = margin(t = 10)),
    plot.title = element_text(size = 12, face = "bold", hjust = 0, margin = margin(b = 12)),
    plot.margin = margin(10, 10, 10, 10)
  )

