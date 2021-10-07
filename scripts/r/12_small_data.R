# Small data ------------------------------------------------------------------
#
#
#
#
#
# -----------------------------------------------------------------------------



# Source helpers and libs -----------------------------------------------------

source(here::here("scripts", "r", "07_load_data.R"))

# -----------------------------------------------------------------------------



# Learner descriptives --------------------------------------------------------

n_learners <- learners$participant %>% unique %>% length
n_removed  <- id_remove$learners %>% nrow

# -----------------------------------------------------------------------------



# Speech rate info ------------------------------------------------------------

mono_speech_rates_df <- sr %>% 
  pivot_longer(cols = c("speech_rate", "articulation_rate", "avg_syll_dur"), 
  names_to = "metric", values_to = "val") 

mono_speech_rates_avg <- sr %>% 
  pivot_longer(cols = c("speech_rate", "articulation_rate", "avg_syll_dur"), 
  names_to = "metric", values_to = "val") %>% 
  group_by(speaker_variety, metric) %>% 
  summarize(avg_val = mean(val), med_val = median(val), .groups = "drop") %>% 
  pivot_longer(cols = c("avg_val", "med_val"), names_to = "measure", 
    values_to = "val")

mono_speech_rates_printy <- sr %>% 
  pivot_longer(cols = c("speech_rate", "articulation_rate", "avg_syll_dur"), 
  names_to = "metric", values_to = "val") %>% 
  group_by(speaker_variety, metric) %>% 
  summarize(avg_val = mean(val), med_val = median(val), .groups = "drop") %>% 
  pivot_longer(cols = c("avg_val", "med_val"), names_to = "measure", 
    values_to = "val") %>% 
  printy::super_split(speaker_variety, metric)

# -----------------------------------------------------------------------------





# Time to complete tasks ------------------------------------------------------

time <- bind_rows(
  read_csv(here("data", "raw", "prolific_export_602aa93df732e9107ec837da_l2_reset.csv")), 
  read_csv(here("data", "raw", "prolific_export_602ab2f3ccc37d12d58ba79b_sp_mono.csv")), 
  read_csv(here("data", "raw", "prolific_export_605b45a4d25f287719a41696_l2_e.csv")),
  read_csv(here("data", "raw", "prolific_export_6057c54e64b0b4e5c128b4f9_l2_ne.csv")),
  read_csv(here("data", "raw", "prolific_export_60568611ec04488a9e9da93b_en_mono.csv"))
  ) %>% 
  select(status, time_taken) %>% 
  filter(status == "APPROVED") %>% 
  mutate(min = time_taken / 60) 

t_mean   <- time$min %>% mean
t_median <- time$min %>% median

# -----------------------------------------------------------------------------






# LexTALE stuff ---------------------------------------------------------------
lt_dat <- learners %>% 
  distinct(participant, lextale_tra) %>% 
  summarize(mean_lt = mean(lextale_tra), sd_lt = sd(lextale_tra), 
    min_lt = min(lextale_tra), max_lt = max(lextale_tra)) %>% 
  pivot_longer(cols = everything(), names_to = "desc", values_to = "val") %>% 
  mutate_if(is.numeric, round, digits = 2) %>% 
  split(.$desc)


# Empathy stuff ---------------------------------------------------------------

eq_dat <- learners %>% 
  summarize(mean_eq = mean(eq_score), sd_eq = sd(eq_score), 
    min_eq = min(eq_score), max_eq = max(eq_score)) %>% 
  pivot_longer(cols = everything(), names_to = "desc", values_to = "val") %>% 
  mutate_if(is.numeric, round, digits = 2) %>% 
  split(.$desc)


