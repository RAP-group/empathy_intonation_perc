# Tidy lexTALE data -----------------------------------------------------------
#
# Get empathy quotient data from exp directory by loading all .csv's
# Filter for empathy block and score according to the following criteria: 
# 
# -----------------------------------------------------------------------------




# Source libs  and helpers ----------------------------------------------------

source(here::here("scripts", "r","01_helpers.R"))

# -----------------------------------------------------------------------------




# calculate score 
# Score = N yes to words – 2 * N yes to nonwords 
#
# or 
#
# The LexTALE score consists of the percentage of correct responses, 
# corrected for the unequal proportion of words and nonwords in the test by 
# averaging the percentages correct for these two item types. 
# We call this measure % correctav (averaged % correct). 
# It is calculated as follows:
# ((n_corr_real / n_real_words * 100) + (n_corr_nonse / n_nonse_words * 100)) / 2






# Tidy lexTALE ----------------------------------------------------------------

dir_ls(here("exp", "data"), regexp = "\\.csv$", ) %>%
  map_dfr(read_csv, .id = "source") %>% 
  filter(!is.na(key_resp_lextale_trial.keys)) %>% 
  mutate(is_real = if_else(correct_response == 1, "real", "nonse"), 
         is_correct = key_resp_lextale_trial.corr, 
         is_incorrect = if_else(is_correct == 0, 1, 0), 
         type = case_when(
           is_real == "real"  & is_correct == 1 ~ "real_correct", 
           is_real == "real"  & is_correct == 0 ~ "real_incorrect", 
           is_real == "nonse" & is_correct == 1 ~ "nonse_correct", 
           is_real == "nonse" & is_correct == 0 ~ "nonse_incorrect" 
         )) %>% 
  group_by(source, type) %>% 
  summarize(totals = n()) %>% 
  pivot_wider(names_from = "type", values_from = "totals") %>% 
  mutate(n_real = nonse_correct + nonse_incorrect, 
         n_nonse = real_correct + real_incorrect, 
         n = n_real + n_nonse, 
         score_avg = score_lextale(n_real = n_real, n_nonse = n_nonse,
           n_real_correct = real_correct, n_nonse_correct = nonse_correct), 
         score_tra = score_lextale(n_real_correct = real_correct,
           n_nonse_incorrect = nonse_incorrect)) 

# -----------------------------------------------------------------------------
