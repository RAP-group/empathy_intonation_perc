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

# Get path to learner data
path <- paste0(here(), "/exp/empathy_intonation_perc/data/")

# Vector of .csv's to remove ("returned")
returned <- c(
  "60514fe20a8ea24a26a96e85_empathy_intonation_perc_2021-03-22_00h58.41.495.csv", # incomplete
  "5f70ecb8b8137d701eb29f26_empathy_intonation_perc_2021-03-22_02h08.33.943.csv", # incomplete
  "5f70ecb8b8137d701eb29f26_empathy_intonation_perc_2021-03-22_02h03.33.854.csv", # incomplete
  "5e0e1b8800a6bf000a694f79_empathy_intonation_perc_2021-03-22_08h02.40.720.csv", # incomplete
  "5dd28beafce6062a4f5221b3_empathy_intonation_perc_2021-03-22_05h23.46.880.csv", # incomplete
  "PARTICIPANT_empathy_intonation_perc2_2021-03-22_03h48.00.848.csv", 
  "PARTICIPANT_empathy_intonation_perc2_2021-03-22_03h47.57.700.csv", 
  "PARTICIPANT_empathy_intonation_perc2_2021-03-22_00h48.40.258.csv", 
  "PARTICIPANT_empathy_intonation_perc2_2021-03-21_23h39.09.387.csv", 
  "60482cefb198f41385293a2d_empathy_intonation_perc_2021-03-21_21h39.24.588.csv", # incomplete
  "5fc8232ac3b2ae180fa1a39b_empathy_intonation_perc_2021-03-21_10h28.18.777.csv", # incomplete/slow
  "PARTICIPANT_empathy_intonation_perc2_2021-03-21_10h34.31.784.csv", 
  "5dcd91a0b7ebf606d16c971e_empathy_intonation_perc_2021-03-20_22h15.44.775.csv", # attention check
  "5fc98a7db2180e4b23c6f5be_empathy_intonation_perc_2021-03-20_20h14.49.809.csv", # all 1's
  "5dd2441687c1fd266f9457f6_empathy_intonation_perc_2021-03-20_21h50.59.284.csv", 
  "58cbd0a55d42920001414dd3_empathy_intonation_perc_2021-03-20_21h06.21.726.csv", 
  "PARTICIPANT_empathy_intonation_perc2_2021-03-20_21h18.06.182.csv", 
  "PARTICIPANT_empathy_intonation_perc2_2021-03-20_21h54.32.690.csv"
  )

# Combine path and file names to filter them out
path_returned <- paste0(path, returned)

dir_ls(
  path = here("exp", "empathy_intonation_perc", "data"), 
  regexp = "\\.csv$"
  ) %>% 
  as_tibble() %>% 
  filter(!(value %in% path_returned)) %>% 
  pull() %>% 
  map_dfr(read_csv, .id = "source", 
    col_types = cols(.default = "?", `key_resp_ac1.keys` = "c")) %>% 
  select(
    participant, 
    eng_variety = `What part of the US are you from?*`, 
    spn_variety = `I am most familiar with Spanish from...*`, 
    have_ln = `Are you proficient in any languages other than English/Spanish?*`, 
    aoa = `At what age did you begin learning Spanish?*`, 
    check_pass, check_fails, stim, word, 
    key_resp_lextale_trial.keys:trials_lextale_loop.ran
  ) %>% 
  group_by(participant) %>% 
  mutate(check_pass = sum(check_pass, na.rm = T), 
         check_fails = sum(check_fails, na.rm = T), .groups = "drop") %>% 
  filter(!is.na(key_resp_lextale_trial.keys)) %>% 
  mutate(
    group = "learner", 
    response = `key_resp_lextale_trial.keys`, 
    is_correct = key_resp_lextale_trial.corr, 
    is_incorrect = if_else(is_correct == 0, 1, 0), 
    is_real = case_when(
      response == 1 & is_correct == 1 ~ "real", 
      response == 1 & is_correct == 0 ~ "nonse", 
      response == 0 & is_correct == 1 ~ "nonse", 
      response == 0 & is_correct == 0 ~ "real"),  
    real_correct    = if_else(is_real == "real"  & is_correct == 1, 1, 0), 
    real_incorrect  = if_else(is_real == "real"  & is_correct == 0, 1, 0), 
    nonse_correct   = if_else(is_real == "nonse" & is_correct == 1, 1, 0), 
    nonse_incorrect = if_else(is_real == "nonse" & is_correct == 0, 1, 0)
    ) %>% 
  group_by(participant, group, eng_variety, spn_variety, have_ln, 
           check_pass, check_fails) %>% 
  summarize(totals = n(), 
    real_correct = sum(real_correct), 
    real_incorrect = sum(real_incorrect), 
    nonse_correct = sum(nonse_correct), 
    nonse_incorrect = sum(nonse_incorrect), .groups = "drop") %>% 
  mutate(
    n_real = real_correct + real_incorrect, 
    n_nonse = nonse_correct + nonse_incorrect, 
    n = n_real + n_nonse, 
    lextale_avg = score_lextale(
      n_real = n_real, 
      n_nonse = n_nonse,
      n_real_correct = real_correct, 
      n_nonse_correct = nonse_correct), 
    lextale_tra = score_lextale(
      n_real_correct = real_correct,
      n_nonse_incorrect = nonse_incorrect
      )
    ) %>% 
  write_csv(here("data", "tidy", "learners_lextale_tidy.csv"))

# -----------------------------------------------------------------------------
