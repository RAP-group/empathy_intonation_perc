# -

# Source libs  and helpers ----------------------------------------------------

source(here::here("scripts", "r","01_helpers.R"))

# -----------------------------------------------------------------------------


# Load Speech rate data -------------------------------------------------------

read_csv(here("data", "raw", "speech_rate_raw.csv")) %>% 
  separate(
    col = soundname, 
    into = c("variety", "condition", "sentence_type", "item"), sep = "_"
    ) %>%
  rename(
    speech_rate = `speechrate (nsyll/dur)`, 
    articulation_rate = `articulation rate (nsyll / phonationtime)`, 
    avg_syll_dur = `ASD (speakingtime/nsyll)`
    ) %>% 
  mutate(variety = recode(variety, "puertorican" = "Puerto Rican")) %>%  
  mutate(variety = tools::toTitleCase(variety)) %>% 
  write_csv(here("data", "tidy", "speech_rate_tidy.csv"))

# -----------------------------------------------------------------------------
