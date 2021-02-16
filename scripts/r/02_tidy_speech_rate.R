# -

# Source libs  and helpers ----------------------------------------------------

source(here::here("scripts", "r","01_helpers.R"))

# -----------------------------------------------------------------------------


# Load Speech rate data -------------------------------------------------------

read_csv(here("data", "raw", "speech_rate_raw.csv")) %>% 
  separate(
    col = soundname, 
    into = c("speaker_variety", "condition", "sentence_type", "sentence"), 
    sep = "_") %>%
  rename(
    sentence_dur = `dur (s)`, 
    speech_rate = `speechrate (nsyll/dur)`, 
    articulation_rate = `articulation rate (nsyll / phonationtime)`, 
    avg_syll_dur = `ASD (speakingtime/nsyll)`
    ) %>% 
  mutate(
    sentence = str_remove(sentence, ".wav"), 
    sentence = str_replace_all(sentence, "-", " ")) %>% 
  write_csv(here("data", "tidy", "speech_rate_tidy.csv"))

# -----------------------------------------------------------------------------
