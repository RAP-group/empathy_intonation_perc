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
   "xxxxxx",                   "learners", "low_accuracy", 
   "5efcbbdd231bde0af2edb847", "natives",  "low_accuracy",
   "5e8f67f9cc3d6c24f4b646c9", "natives",  "no_code", 
   "5f3b1f2078bf3c22f24a262c", "natives",  "low_accuracy", 
   "5e8ea704815afd16671d19e7", "natives",  "no_code", 
   "5fbb46ab3109908f63f6f95b", "natives",  "low_accuracy", 
  ) %>% 
  printy::super_split(group) 
  saveRDS(id_remove, here("data", "tidy", "participants_removed_list.Rds"))


# Spanish learners
learners <- dir_ls(path = here("exp", "empathy_intonation_perc", "data"), 
  regexp = "\\.csv$") %>%
  map_dfr(read_csv, .id = "source") %>% 
  filter(!(participant %in% id_remove$learners$participant)) %>%
  select(
    participant, 
    en_variety = `What variety of English do you speak?*`, 
    sp_variety = `What variety of Spanish are you most familiar with?*`, 
    have_ln = `Are you proficient in any languages other than English/Spanish (yes/no)?*`, 
    check_pass, check_fails, item = andalusian, 
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
    remove = T) %>% 
  mutate(
    group = "learner", 
    sentence = str_remove(sentence, ".wav"), 
    sentence = str_replace_all(sentence, "-", " ")) %>% 
  select(
    participant, group, en_variety:check_fails, speaker_variety = the_col,
    condition:correct_response, response = key_resp_2afc_trial.keys,
    is_correct = key_resp_2afc_trial.corr, rt_raw = key_resp_2afc_trial.rt) %>%
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
      select(participant = participant_id, sp_variety = Nationality), 
    by = "participant") %>% 
  filter(!grepl('iniciales \\+', source), 
         !(participant %in% id_remove$natives$participant)) %>% 
  select(
    participant, 
    sp_variety, 
    have_ln = `¿Hablas con fluidez otra lengua (sí/no)?*`, 
    correct_response:key_resp_2afc_trial.rt, 
    item = andalusian
  ) %>% 
  mutate(check_pass = NA, check_fails = NA, ) %>%
  filter(!is.na(key_resp_2afc_trial.keys)) %>% 
  mutate(item = str_remove(item, "./stim/wavs/")) %>% 
  separate(
    col = item, 
    into = c("speaker_variety", "condition", "sentence_type", "sentence"), 
    sep = "_", 
    remove = T) %>% 
  mutate(
    group = "native", 
    en_variety = NA, 
    sentence = str_remove(sentence, ".wav"), 
    sentence = str_replace_all(sentence, "-", " ")
  ) %>% 
  select(participant, group, en_variety, sp_variety:have_ln, 
    check_pass:check_fails, speaker_variety = the_col, condition:sentence,
    correct_response, response = key_resp_2afc_trial.keys, 
    is_correct = key_resp_2afc_trial.corr, rt_raw = key_resp_2afc_trial.rt) %>% 
  left_join(., 
    read_csv(here("data", "tidy", "speech_rate_tidy.csv")) %>% 
      select(speaker_variety:sentence, sentence_dur), 
      by = c("speaker_variety", "condition", "sentence_type", "sentence")) %>% 
  mutate(rt_adj = rt_raw - sentence_dur) %>% 
  write_csv(here("data", "tidy", "natives_2afc_tidy.csv"))

bind_rows(learners, natives) %>% 
  write_csv(here("data", "tidy", "complete_2afc_tidy.csv"))

# -----------------------------------------------------------------------------
