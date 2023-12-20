################################################################################
# Carlos Rodriguez, PhD. Dept. of Family Medicine, CU Anschutz Medical Campus
# December 11, 2023
# Better Together
################################################################################


# Load libraries ---------------------------------------------------------------
pacman::p_load(REDCapR,
               here,
               Hmisc,
               tidyverse,
               gtsummary,
               bstfun,
               furrr,
               flextable,
               magrittr,
               nlme)

prop_data <- 
  data %>%
  group_by(identifier) %>%
  slice_head(n = 1) %>%
  select(identifier, randomization.factor, pre_post_complete) %>%
  ungroup()



prop_data %>%
  filter(randomization.factor == "Waitlist") %>%
  nrow()

prop_data %>%
  group_by(randomization.factor) %>%
  summarise(n_responders = sum(pre_post_complete))

prop_data %>%
  select(-identifier) %>%
  tbl_summary(by = randomization.factor) %>%
  add_p()

tbl <-
  prop_data %>%
  filter(pre_post_complete == 1) %>%
  select(randomization.factor) %>%
  tbl_summary(missing = "no") %>%
  add_n() %>%
  modify_footnote(everything() ~ NA)

# calculate CI and put it in format that can merge with tbl$table_body
tbl_ci <-
  tbl$meta_data %>%
  filter(summary_type %in% c("categorical", "dichotomous")) %>%
  select(summary_type, var_label, df_stats) %>%
  unnest(df_stats) %>%
  mutate(
    conf.low = (p - qnorm(0.975) * sqrt(p * (1 - p) / N)) %>% style_percent(symbol = TRUE),
    conf.high =( p + qnorm(0.975) * sqrt(p * (1 - p) / N)) %>% style_percent(symbol = TRUE),
    ci = str_glue("{conf.low}, {conf.high}"),
    label = coalesce(variable_levels, var_label),
    row_type = ifelse(summary_type == "dichotomous", "label", "level")
  ) %>%
  select(variable, row_type, label, ci) 

# merge in CI and set column header
tbl %>%
  modify_table_body(
    left_join,
    tbl_ci,
    by = c("variable", "row_type", "label")
  ) %>%
  modify_table_styling(
    ci, 
    hide = FALSE,
    label = "**95% CI**"
  )

p <- .53
N = 120
p - qnorm(0.975) * sqrt(p * (1 - p) / N)
