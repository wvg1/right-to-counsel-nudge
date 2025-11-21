### this script analyzes the final dataset from an experiment testing the effectiveness of a mailer for increasing access to WA's right to counsel program
#setwd as right-to-counsel-nudge prior to running

#load packages
library(tidyverse)
library(readxl)
library(sandwich)
library(lmtest)

#read in data (crystal, you will have to adapt this)
data_for_analysis <- read_xlsx("data/final_data_for_analysis.xlsx")

#mutate binary variables to logical
data_for_analysis <- data_for_analysis %>%
  mutate(across(c(appearance, appearance_provider, treat, hearing_held, hearing_att, rep_screened, 
                  rep_appointed, rep_waived, rep_denied, writ, writ_vacated, 
                  dismissal, dismissal_vacated, old, 
                  old_vacated, court_displacement),
                ~as.logical(.)))

#create final outcome variables (exclude cases where outcomes were reversed)
data_for_analysis <- data_for_analysis %>%
  mutate(
    writ_final = writ == TRUE & writ_vacated == FALSE,
    dismissal_final = dismissal == TRUE & dismissal_vacated == FALSE,
    old_final = old == TRUE & old_vacated == FALSE
  )

#create binary monetary judgment variable
data_for_analysis <- data_for_analysis %>%
  mutate(monetary_judgment_issued = monetary_judgment > 0)

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
#quick balancing tests
#balance test: chi-square tests for categorical variables
with(data_for_analysis, chisq.test(table(household_treated_by_this_hearing, appearance_before_hearing)))
with(data_for_analysis, chisq.test(table(household_treated_by_this_hearing, flag_tacoma)))

#create balance table
balance_table <- data_for_analysis %>%
  group_by(household_treated_by_this_hearing) %>%
  summarise(
    n = n(),
    appearance_pct = sum(appearance_before_hearing == TRUE, na.rm = TRUE) / n() * 100,
    tacoma_pct = sum(flag_tacoma == TRUE, na.rm = TRUE) / n() * 100,
    .groups = "drop"
  )

print(balance_table)

###models 1-3: treatment effects on hearing outcomes
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
  mutate(outcome = factor(outcome, levels = c("Attendance", "Hearing held")))

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

###models 4-10: treatment effects on case outcomes
#model 4: screened for representation
m4 <- glm(rep_screened ~ household_treated_by_this_hearing + appearance_before_hearing + flag_tacoma,
          data = data_for_analysis,
          family = binomial())
V4 <- vcovCL(m4, cluster = data_for_analysis$household_ID, type = "HC1")
ct4 <- coeftest(m4, vcov. = V4)
m4_est <- ct4["household_treated_by_this_hearingTRUE", "Estimate"]
m4_se <- ct4["household_treated_by_this_hearingTRUE", "Std. Error"]
m4_or <- exp(m4_est)
m4_or_lo <- exp(m4_est - 1.96 * m4_se)
m4_or_hi <- exp(m4_est + 1.96 * m4_se)

#model 5: representation appointed
m5 <- glm(rep_appointed ~ household_treated_by_this_hearing + appearance_before_hearing + flag_tacoma,
          data = data_for_analysis,
          family = binomial())
V5 <- vcovCL(m5, cluster = data_for_analysis$household_ID, type = "HC1")
ct5 <- coeftest(m5, vcov. = V5)
m5_est <- ct5["household_treated_by_this_hearingTRUE", "Estimate"]
m5_se <- ct5["household_treated_by_this_hearingTRUE", "Std. Error"]
m5_or <- exp(m5_est)
m5_or_lo <- exp(m5_est - 1.96 * m5_se)
m5_or_hi <- exp(m5_est + 1.96 * m5_se)

#model 6: writ (not vacated)
m6 <- glm(writ_final ~ household_treated_by_this_hearing + appearance_before_hearing + flag_tacoma,
          data = data_for_analysis,
          family = binomial())
V6 <- vcovCL(m6, cluster = data_for_analysis$household_ID, type = "HC1")
ct6 <- coeftest(m6, vcov. = V6)
m6_est <- ct6["household_treated_by_this_hearingTRUE", "Estimate"]
m6_se <- ct6["household_treated_by_this_hearingTRUE", "Std. Error"]
m6_or <- exp(m6_est)
m6_or_lo <- exp(m6_est - 1.96 * m6_se)
m6_or_hi <- exp(m6_est + 1.96 * m6_se)

#model 7: monetary judgment issued (binary)
m7 <- glm(monetary_judgment_issued ~ household_treated_by_this_hearing + appearance_before_hearing + flag_tacoma,
          data = data_for_analysis,
          family = binomial())
V7 <- vcovCL(m7, cluster = data_for_analysis$household_ID, type = "HC1")
ct7 <- coeftest(m7, vcov. = V7)
m7_est <- ct7["household_treated_by_this_hearingTRUE", "Estimate"]
m7_se <- ct7["household_treated_by_this_hearingTRUE", "Std. Error"]
m7_or <- exp(m7_est)
m7_or_lo <- exp(m7_est - 1.96 * m7_se)
m7_or_hi <- exp(m7_est + 1.96 * m7_se)

#model 8: dismissal (not vacated)
m8 <- glm(dismissal_final ~ household_treated_by_this_hearing + appearance_before_hearing + flag_tacoma,
          data = data_for_analysis,
          family = binomial())
V8 <- vcovCL(m8, cluster = data_for_analysis$household_ID, type = "HC1")
ct8 <- coeftest(m8, vcov. = V8)
m8_est <- ct8["household_treated_by_this_hearingTRUE", "Estimate"]
m8_se <- ct8["household_treated_by_this_hearingTRUE", "Std. Error"]
m8_or <- exp(m8_est)
m8_or_lo <- exp(m8_est - 1.96 * m8_se)
m8_or_hi <- exp(m8_est + 1.96 * m8_se)

#model 9: record protection orders (not vacated)
m9 <- glm(old_final ~ household_treated_by_this_hearing + appearance_before_hearing + flag_tacoma,
          data = data_for_analysis,
          family = binomial())
V9 <- vcovCL(m9, cluster = data_for_analysis$household_ID, type = "HC1")
ct9 <- coeftest(m9, vcov. = V9)
m9_est <- ct9["household_treated_by_this_hearingTRUE", "Estimate"]
m9_se <- ct9["household_treated_by_this_hearingTRUE", "Std. Error"]
m9_or <- exp(m9_est)
m9_or_lo <- exp(m9_est - 1.96 * m9_se)
m9_or_hi <- exp(m9_est + 1.96 * m9_se)

#model 10: court_displacement
m10 <- glm(court_displacement ~ household_treated_by_this_hearing + appearance_before_hearing + flag_tacoma,
           data = data_for_analysis,
           family = binomial())
V10 <- vcovCL(m10, cluster = data_for_analysis$household_ID, type = "HC1")
ct10 <- coeftest(m10, vcov. = V10)
m10_est <- ct10["household_treated_by_this_hearingTRUE", "Estimate"]
m10_se <- ct10["household_treated_by_this_hearingTRUE", "Std. Error"]
m10_or <- exp(m10_est)
m10_or_lo <- exp(m10_est - 1.96 * m10_se)
m10_or_hi <- exp(m10_est + 1.96 * m10_se)

#combine results for plotting
results_outcomes <- bind_rows(
  tibble(outcome = "Screened for representation", or = m4_or, or_lo = m4_or_lo, or_hi = m4_or_hi),
  tibble(outcome = "Representation appointed", or = m5_or, or_lo = m5_or_lo, or_hi = m5_or_hi),
  tibble(outcome = "Eviction judgment", or = m6_or, or_lo = m6_or_lo, or_hi = m6_or_hi),
  tibble(outcome = "Monetary judgment issued", or = m7_or, or_lo = m7_or_lo, or_hi = m7_or_hi),
  tibble(outcome = "Dismissal", or = m8_or, or_lo = m8_or_lo, or_hi = m8_or_hi),
  tibble(outcome = "Record protected", or = m9_or, or_lo = m9_or_lo, or_hi = m9_or_hi),
  tibble(outcome = "Court displacement", or = m10_or, or_lo = m10_or_lo, or_hi = m10_or_hi)
) %>%
  mutate(outcome = factor(outcome, levels = c("Court displacement", "Record protected",
                                              "Dismissal", "Monetary judgment issued",
                                              "Eviction judgment", "Representation appointed",
                                              "Screened for representation")))

#plot treatment effects on case outcomes
ggplot(results_outcomes, aes(x = or, y = outcome)) +
  geom_point(size = 2, color = "steelblue") +
  geom_errorbarh(aes(xmin = or_lo, xmax = or_hi),
                 height = 0.15, linewidth = 0.8, color = "steelblue") +
  geom_vline(xintercept = 1, linetype = "dashed", color = "gray40") +
  scale_x_log10(breaks = c(0.5, 1, 2), limits = c(0.3, 2.5)) +
  labs(x = "Odds Ratio (log scale)", y = NULL, 
       title = "Treatment Effects on Case Outcomes") +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", hjust = 0, margin = margin(b = 10)))

#create table with case outcome results
case_outcomes_table <- bind_rows(
  tibble(outcome = "Screened for representation", 
         n = nobs(m4), events = sum(model.frame(m4)$rep_screened == 1, na.rm = TRUE),
         or = m4_or, or_lo = m4_or_lo, or_hi = m4_or_hi, p = 2 * pnorm(-abs(m4_est / m4_se))),
  tibble(outcome = "Representation appointed",
         n = nobs(m5), events = sum(model.frame(m5)$rep_appointed == 1, na.rm = TRUE),
         or = m5_or, or_lo = m5_or_lo, or_hi = m5_or_hi, p = 2 * pnorm(-abs(m5_est / m5_se))),
  tibble(outcome = "Eviction judgment",
         n = nobs(m6), events = sum(model.frame(m6)$writ == 1, na.rm = TRUE),
         or = m6_or, or_lo = m6_or_lo, or_hi = m6_or_hi, p = 2 * pnorm(-abs(m6_est / m6_se))),
  tibble(outcome = "Monetary judgment Issued",
         n = nobs(m7), events = sum(model.frame(m7)$monetary_judgment_issued == 1, na.rm = TRUE),
         or = m7_or, or_lo = m7_or_lo, or_hi = m7_or_hi, p = 2 * pnorm(-abs(m7_est / m7_se))),
  tibble(outcome = "Dismissal",
         n = nobs(m8), events = sum(model.frame(m8)$dismissal == 1, na.rm = TRUE),
         or = m8_or, or_lo = m8_or_lo, or_hi = m8_or_hi, p = 2 * pnorm(-abs(m8_est / m8_se))),
  tibble(outcome = "Order for Limited Dissemination",
         n = nobs(m9), events = sum(model.frame(m9)$old == 1, na.rm = TRUE),
         or = m9_or, or_lo = m9_or_lo, or_hi = m9_or_hi, p = 2 * pnorm(-abs(m9_est / m9_se))),
  tibble(outcome = "Court Displacement",
         n = nobs(m10), events = sum(model.frame(m10)$court_displacement == 1, na.rm = TRUE),
         or = m10_or, or_lo = m10_or_lo, or_hi = m10_or_hi, p = 2 * pnorm(-abs(m10_est / m10_se))),
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
print(case_outcomes_table, width = 100)

#model 11: effect on amount of monetary judgment

#11a: logistic regression on whether judgment was issued
m_11a <- glm(judgment_issued ~ household_treated_by_this_hearing + appearance_before_hearing + flag_tacoma,
               data = data_for_analysis,
               family = binomial())

V_11a <- vcovCL(m_11a, cluster = data_for_analysis$household_ID, type = "HC1")
ct_11a <- coeftest(m_11a, vcov. = V_part1)

m_11a_est <- ct_11a["household_treated_by_this_hearingTRUE", "Estimate"]
m_11a_se <- ct_11a["household_treated_by_this_hearingTRUE", "Std. Error"]
m_11a_or <- exp(m_11a_est)
m_11a_or_lo <- exp(m_11a_est - 1.96 * m_11a_se)
m_11a_or_hi <- exp(m_11a_est + 1.96 * m_11a_se)

#11b: OLS regression on log(monetary judgment) among cases with positive judgments
data_11b <- data_for_analysis %>%
  filter(monetary_judgment > 0) %>%
  mutate(log_judgment = log(monetary_judgment))

m_11b <- lm(log_judgment ~ household_treated_by_this_hearing + appearance_before_hearing + flag_tacoma,
            data = data_11b)

V_11b <- vcovCL(m_11b, cluster = data_11b$household_ID, type = "HC1")
ct_11b <- coeftest(m_11b, vcov. = V_11b)

m_11b_est <- ct_11b["household_treated_by_this_hearingTRUE", "Estimate"]
m_11b_se <- ct_11b["household_treated_by_this_hearingTRUE", "Std. Error"]
m_11b_pct_change <- (exp(m_11b_est) - 1) * 100

#print model 11 results and CI bounds
print(ct_11a)
round(m_11a_or, 3)
round(m_11a_or_hi, 3)

print(ct_11b)

#calculate average (control) judgment and % change
data_for_analysis %>%
  filter(monetary_judgment > 0 & household_treated_by_this_hearing == FALSE) %>%
  summarise(mean_judgment = mean(monetary_judgment, na.rm = TRUE))
print(m_11b_pct_change)

#histograms
data_for_analysis %>%
  filter(monetary_judgment > 0) %>%
  mutate(treatment = if_else(household_treated_by_this_hearing, "Treatment", "Control")) %>%
  ggplot(aes(x = monetary_judgment / 1000, fill = treatment)) +
  geom_histogram(bins = 20, alpha = 0.7) +
  facet_wrap(~treatment) +
  labs(x = "Monetary Judgment Amount ($1000s)", y = "Count",
       title = "Distribution of Monetary Judgments by Treatment Status") +
  scale_fill_manual(values = c("Treatment" = "steelblue", "Control" = "lightcoral")) +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", hjust = 0, margin = margin(b = 10)),
        legend.position = "none")

#density plot
data_for_analysis %>%
  filter(monetary_judgment > 0) %>%
  mutate(treatment = if_else(household_treated_by_this_hearing, "Treatment", "Control")) %>%
  ggplot(aes(x = monetary_judgment, fill = treatment)) +
  geom_density(alpha = 0.6) +
  labs(x = "Monetary Judgment ($)", y = "Density",
       title = "Distribution of Monetary Judgments by Treatment Status",
       fill = "Group") +
  scale_fill_manual(values = c("Treatment" = "steelblue", "Control" = "lightcoral")) +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", hjust = 0, margin = margin(b = 10)))
