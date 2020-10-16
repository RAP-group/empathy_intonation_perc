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

source(here::here("scripts", "01_helpers.R"))

# -----------------------------------------------------------------------------





# Tidy empathy ----------------------------------------------------------------

dir_ls(here("exp", "data"), regexp = "\\.csv$", ) %>%
  map_dfr(read_csv, .id = "source") %>% 
  mutate(eq_response = slider_eq_trial.response, 
    score = case_when(
    is_agree_score == 1 & eq_response == "strongly agree" ~ 2, 
    is_agree_score == 1 & eq_response == "slightly agree" ~ 1, 
    is_disagree_score == 1 & eq_response == "strongly disagree" ~ 2, 
    is_disagree_score == 1 & eq_response == "slightly disagree" ~ 1, 
    TRUE ~ 0
  )) %>% 
  group_by(source) %>% 
  summarize(eq_score = sum(score), .groups = "drop")

# -----------------------------------------------------------------------------
