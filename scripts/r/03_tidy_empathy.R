# Tidy empathy quotient data --------------------------------------------------
#
# Get empathy quotient data from exp directory by loading all .csv's
# Filter for empathy block and score according to the following criteria: 
# - 'agree' scoring items (is_agree_score == 1)
#    - “strongly agree” responses scored 2 points 
#    - “slightly agree” responses scored 1 point 
# - 'disagree' scoring items (is_disagree_score == 1)
#    - “strongly disagree” responses scored 2 points
#    - “slightly disagree” responses scored 1 point
# - All other responses scored as 0
#
# -----------------------------------------------------------------------------




# Source libs  and helpers ----------------------------------------------------

source(here::here("scripts", "r", "01_helpers.R"))

# -----------------------------------------------------------------------------





# Tidy empathy ----------------------------------------------------------------

dir_ls(
  path = here("exp", "empathy_intonation_perc", "data"), 
  regexp = "\\.csv$"
  ) %>%
  map_dfr(read_csv, .id = "source") %>% 
  select(
    participant, 
    en_variety = `What variety of English do you speak?*`, 
    sp_variety = `What variety of Spanish are you most familiar with?*`, 
    have_ln = `Are you proficient in any languages other than English/Spanish (yes/no)?*`, 
    check_pass, check_fails, 
    slider_eq_trial.response:is_disagree_score
  ) %>% 
  group_by(participant) %>% 
  mutate(check_pass = sum(check_pass, na.rm = T), 
         check_fails = sum(check_fails, na.rm = T), .groups = "drop") %>% 
  filter(!is.na(slider_eq_trial.response)) %>% 
  mutate(
    group = "learner", 
    eq_response = slider_eq_trial.response, 
    score = case_when(
      is_agree_score    == 1 & eq_response == 1 ~ 2, 
      is_agree_score    == 1 & eq_response == 2 ~ 1, 
      is_disagree_score == 1 & eq_response == 5 ~ 2, 
      is_disagree_score == 1 & eq_response == 4 ~ 1, 
      TRUE ~ 0
      )
    ) %>% 
  group_by(participant, group, en_variety, sp_variety, 
           have_ln, check_pass, check_fails) %>% 
  summarize(eq_score = sum(score), .groups = "drop") %>% 
  write_csv(here("data", "tidy", "learners_eq_tidy.csv"))

# -----------------------------------------------------------------------------
