# Tidy 2afc data --------------------------------------------------------------
#
# Get 2afc data from exp directory by loading all .csv's
# Select relevant rows and Filter for 2afc block
# Correct columns names and combine monolingual and L2 data
#
# -----------------------------------------------------------------------------




# Source libs  and helpers ----------------------------------------------------

source(here::here("scripts", "r", "01_helpers.R"))

# -----------------------------------------------------------------------------




# Tidy 2afc -------------------------------------------------------------------

# Participants to remove
id_remove <- tribble(
  ~"participant",             ~"group",   ~"reason", 
   "5fb2caa34a6f4d94967b2748", "learners", "low_accuracy", 
   "5d5af833f35ed70001e17a5c", "learners", "low_accuracy", 
   "5dd55364dcec8750a2efc32b", "learners", "no_code", 
   "5fc8232ac3b2ae180fa1a39b", "learners", "incomplete/slow", 
   "5dcd91a0b7ebf606d16c971e", "learners", "attention_check", 
   "5fc98a7db2180e4b23c6f5be", "learners", "low_accuracy", 
   "5efcbbdd231bde0af2edb847", "natives",  "low_accuracy",
   "5e8f67f9cc3d6c24f4b646c9", "natives",  "no_code", 
   "5f3b1f2078bf3c22f24a262c", "natives",  "low_accuracy", 
   "5e8ea704815afd16671d19e7", "natives",  "no_code", 
   "5fbb46ab3109908f63f6f95b", "natives",  "low_accuracy", 
  ) %>% 
  printy::super_split(group) 
  saveRDS(id_remove, here("data", "tidy", "participants_removed_list.Rds"))


# Get path to learner data
path <- paste0(here(), "/exp/empathy_intonation_perc/data/")

# Vector of .csv's to remove ("returned")
returned <- c(
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

# Spanish learners
learners <- dir_ls(path = here("exp", "empathy_intonation_perc", "data"), 
  regexp = "\\.csv$") %>%
  as_tibble() %>% 
  filter(!(value %in% path_returned)) %>% 
  pull() %>% 
  map_dfr(read_csv, .id = "source", 
    col_types = cols(.default = "?", `key_resp_ac1.keys` = "c")) %>% 
  filter(!(participant %in% id_remove$learners$participant)) %>%
  select(
    participant, 
    eng_variety = `What part of the US are you from?*`, 
    spn_variety = `I am most familiar with Spanish from...*`, 
    have_ln = `Are you proficient in any languages other than English/Spanish?*`, 
    aoa = `At what age did you begin learning Spanish?*`, 
    check_pass, check_fails, item = andalusian, 
    trial_n = trials_2afc_loop.thisN, 
    correct_response:key_resp_2afc_trial.rt
  ) %>% 
  group_by(participant) %>% 
  mutate(check_pass = sum(check_pass, na.rm = T), 
         check_fails = sum(check_fails, na.rm = T)) %>% 
  ungroup() %>% 
  filter(!is.na(key_resp_2afc_trial.keys)) %>% 
  mutate(item = str_remove(item, "./stim/wavs/")) %>% 
  separate(
    col = item, 
    into = c("speaker_variety", "condition", "sentence_type", "sentence"), 
    sep = "_", 
    remove = F) %>% 
  mutate(
    group = "learner", 
    sentence = str_remove(sentence, ".wav"), 
    sentence = str_replace_all(sentence, "-", " ")) %>% 
  select(
    participant, group, eng_variety:check_fails, item, speaker_variety = the_col,
    condition:correct_response, response = key_resp_2afc_trial.keys,
    is_correct = key_resp_2afc_trial.corr, rt_raw = key_resp_2afc_trial.rt, 
    trial_n) %>%
  left_join(., 
    read_csv(here("data", "tidy", "speech_rate_tidy.csv")) %>% 
      select(speaker_variety:sentence, sentence_dur), 
      by = c("speaker_variety", "condition", "sentence_type", "sentence")) %>% 
  mutate(rt_adj = rt_raw - sentence_dur) %>% 
  write_csv(here("data", "tidy", "learners_2afc_tidy.csv"))




#
# Native Spanish
#

natives <- dir_ls(path = here("exp", "empathy_intonation_perc_sp", "data"), 
  regexp = "\\.csv$") %>%
  map_dfr(read_csv, .id = "source") %>% 
  left_join(., read_csv(
    here("data", "raw", "prolific_export_602ab2f3ccc37d12d58ba79b.csv")) %>% 
      select(participant = participant_id, spn_variety = Nationality), 
    by = "participant") %>% 
  filter(!grepl('iniciales \\+', source), 
         !(participant %in% id_remove$natives$participant)) %>% 
  select(
    participant, 
    spn_variety, 
    have_ln = `¿Hablas con fluidez otra lengua (sí/no)?*`, 
    trial_n = trials_2afc_loop.thisN, 
    correct_response:key_resp_2afc_trial.rt, 
    item = andalusian
  ) %>% 
  mutate(check_pass = NA, check_fails = NA, aoa = NA) %>%
  filter(!is.na(key_resp_2afc_trial.keys)) %>% 
  mutate(item = str_remove(item, "./stim/wavs/")) %>% 
  separate(
    col = item, 
    into = c("speaker_variety", "condition", "sentence_type", "sentence"), 
    sep = "_", 
    remove = F) %>% 
  mutate(
    group = "native", 
    eng_variety = NA, 
    sentence = str_remove(sentence, ".wav"), 
    sentence = str_replace_all(sentence, "-", " ")
  ) %>% 
  select(participant, group, eng_variety, spn_variety:have_ln, aoa, 
    check_pass:check_fails, item, speaker_variety = the_col, condition:sentence,
    correct_response, response = key_resp_2afc_trial.keys, 
    is_correct = key_resp_2afc_trial.corr, rt_raw = key_resp_2afc_trial.rt, 
    trial_n) %>% 
  left_join(., 
    read_csv(here("data", "tidy", "speech_rate_tidy.csv")) %>% 
      select(speaker_variety:sentence, sentence_dur), 
      by = c("speaker_variety", "condition", "sentence_type", "sentence")) %>% 
  mutate(rt_adj = rt_raw - sentence_dur) %>% 
  write_csv(here("data", "tidy", "natives_2afc_tidy.csv"))

bind_rows(learners, natives) %>% 
  write_csv(here("data", "tidy", "complete_2afc_tidy.csv"))

# -----------------------------------------------------------------------------
