### this script analyzes the final anonymized dataset from an experiment testing the effectiveness of a mailer for increasing access to WA's right to counsel program
#before running, ensure rct_data_sensitive has been created from create_full_data.R

#load packages
library(tidyverse)
library(sandwich)
library(lmtest)
library(marginaleffects)
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

#refit and save logistic regressions
mods <- setNames(lapply(binary_outcomes, function(y) {
  glm(as.formula(paste0(y, " ~ treat + flag_tacoma + response_cat")),
      data = data_itt, family = binomial())
}), binary_outcomes)

# Use clustered SEs directly inside marginaleffects
ame_tab <- imap_dfr(mods, function(m, y) {
  ame <- avg_slopes(m, variables = "treat", vcov = ~ household_ID)
  tibble(outcome = y,
         AME_pp = 100 * ame$estimate,
         AME_lo = 100 * ame$conf.low,
         AME_hi = 100 * ame$conf.high)
})

# map from raw outcome var -> pretty label used in all_results$outcome
outcome_map <- tibble::tibble(
  outcome_raw = c(
    "hearing_att",
    "dismissal_final",
    "forced_move",
    "hearing_held",
    "monetary_judgment_binary",
    "old_final",
    "rep_offered",
    "writ_final"
  ),
  outcome_label = c(
    "Attendance",
    "Dismissal (final)",
    "Forced move",
    "Hearing held",
    "Monetary judgment (binary)",
    "Order of limited dissemination (final)",
    "Representation offered",
    "Writ (final)"
  )
)

# Recode AME table to the same labels, then join
ame_tab_labeled <- ame_tab %>%
  rename(outcome_raw = outcome) %>%
  left_join(outcome_map, by = "outcome_raw") %>%
  transmute(outcome = outcome_label, AME_pp, AME_lo, AME_hi)

report_full <- all_results %>%
  left_join(ame_tab_labeled, by = "outcome")

report_full %>%
  select(-spec) %>%
  print(n = Inf)

#plot for hearing outcomes
all_results %>%
  filter(outcome %in% c("Hearing held", "Attendance", "Representation offered")) %>%
  ggplot(aes(x = OR, y = outcome)) +
  geom_point(size = 2, color = "steelblue") +
  geom_errorbarh(aes(xmin = OR_lo, xmax = OR_hi),
                 height = 0.2, linewidth = 0.8, color = "steelblue") +
  geom_vline(xintercept = 1, linetype = "dashed", color = "gray40") +
  scale_x_log10(breaks = c(0.5, 1, 2, 3), limits = c(0.4, 3.5)) +
  scale_y_discrete(labels = function(x) ifelse(grepl("\\s", x), sub("\\s", "\n", x), x)) +
  labs(
    x = "Odds Ratio (log scale)",
    y = NULL,
    title = "Treatment Effects on Hearing Outcomes"
  ) +
  theme_minimal(base_size = 9) +
  theme(
    panel.grid.minor = element_blank(),
    axis.text.y = element_text(size = 9),
    plot.title = element_text(size = 12, face = "bold", hjust = 0), # left-aligned, larger
    plot.margin = margin(10, 10, 10, 10) # add room around entire plot
  )

