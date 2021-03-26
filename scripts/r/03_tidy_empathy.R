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

# Get path to learner data
path <- paste0(here(), "/exp/empathy_intonation_perc/data/")

# Vector of .csv's to remove ("returned")
returned <- c(
  "5e065c9fe78021031a4b1d59_empathy_intonation_perc_2021-03-25_13h00.56.460.csv", # incomplete
  "PARTICIPANT_empathy_intonation_perc2_2021-03-25_16h24.34.062.csv", 
  "PARTICIPANT_empathy_intonation_perc2_2021-03-25_16h24.31.523.csv", 
  "PARTICIPANT_empathy_intonation_perc2_2021-03-25_15h42.51.473.csv", 
  "PARTICIPANT_empathy_intonation_perc2_2021-03-25_15h42.48.600.csv", 
  "PARTICIPANT_empathy_intonation_perc2_2021-03-25_15h08.05.826.csv", 
  "PARTICIPANT_empathy_intonation_perc2_2021-03-25_10h55.38.820.csv", 
  "5f2f06e7a2eef42d5780b0ba_empathy_intonation_perc_2021-03-24_17h56.21.659.csv", # incomplete
  "5febbf353cc53cce4cec36e3_empathy_intonation_perc_2021-03-24_17h54.44.243.csv", # incomplete redid
  "5e9a84af150a8012575e4784_empathy_intonation_perc_2021-03-24_21h16.07.495.csv", # incomplete
  "605910bdcee62314abcb6e1a_empathy_intonation_perc_2021-03-24_22h46.32.595.csv", # incomplete
  "5484fd09fdf99b38dfb9645c_empathy_intonation_perc_2021-03-24_17h25.39.063.csv", # incomplete
  "5fbdde99e3096b24351dc1cb_empathy_intonation_perc_2021-03-24_17h01.46.515.csv", # incomplete
  "5f5328104f8d2d6363b0e4ee_empathy_intonation_perc_2021-03-24_16h26.25.761.csv", # incomplete
  "5f5353ff017c5965165df065_empathy_intonation_perc_2021-03-24_16h49.08.914.csv", # incomplete
  "5f2f03deec679e2cf4a992e6_empathy_intonation_perc_2021-03-24_14h48.07.752.csv", # incomplete but redid
  "5c1dc77ee2d9150001b5b70e_empathy_intonation_perc_2021-03-24_17h02.09.917.csv", # incomplete but redid
  "PARTICIPANT_empathy_intonation_perc2_2021-03-24_16h21.08.557.csv", 
  "PARTICIPANT_empathy_intonation_perc2_2021-03-24_17h01.48.903.csv", 
  "PARTICIPANT_empathy_intonation_perc2_2021-03-24_17h29.26.394.csv", 
  "PARTICIPANT_empathy_intonation_perc2_2021-03-24_18h04.30.364.csv", 
  "PARTICIPANT_empathy_intonation_perc2_2021-03-24_18h52.25.551.csv", 
  "PARTICIPANT_empathy_intonation_perc2_2021-03-24_21h34.04.701.csv", 
  "5fb2caa34a6f4d94967b2748_empathy_intonation_perc_2021-03-23_23h08.18.781.csv", # all -RTs
  "5d5af833f35ed70001e17a5c_empathy_intonation_perc_2021-03-23_10h12.18.436.csv", # all 1's
  "PARTICIPANT_empathy_intonation_perc2_2021-03-23_20h49.03.313.csv", 
  "58add4364d580c0001e0bf38_empathy_intonation_perc_2021-03-22_17h10.15.779.csv", # incomplete
  "5eb46ccc0c06142c0faae377_empathy_intonation_perc_2021-03-22_12h33.41.507.csv", # incomplete
  "5e9749b44592572013c96868_empathy_intonation_perc_2021-03-22_13h29.55.390.csv", # incomplete
  "PARTICIPANT_empathy_intonation_perc2_2021-03-22_17h00.58.434.csv", 
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
    col_types = cols(.default = "?", key_resp_ac1.keys = "c")) %>% 
  select(
    participant, 
    eng_variety = `What part of the US are you from?*`, 
    spn_variety = `I am most familiar with Spanish from...*`, 
    have_ln = `Are you proficient in any languages other than English/Spanish?*`, 
    aoa = `At what age did you begin learning Spanish?*`, 
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
  group_by(participant, group, eng_variety, spn_variety, 
           have_ln, check_pass, check_fails) %>% 
  summarize(eq_score = sum(score), .groups = "drop") %>% 
  write_csv(here("data", "tidy", "learners_eq_tidy.csv"))

# -----------------------------------------------------------------------------
