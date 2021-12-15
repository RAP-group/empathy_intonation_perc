# Small data ------------------------------------------------------------------
#
# - script to be loaded in main.Rmd
# - used for reporting "smaller" descriptives (such as N, means, etc.)
# - tables and model summaries are also loaded here for in-prose reporting
#
# -----------------------------------------------------------------------------



# Source helpers and libs -----------------------------------------------------

source(here::here("scripts", "r", "07_load_data.R"))

# -----------------------------------------------------------------------------



# Learner descriptives --------------------------------------------------------

n_learners <- learners$participant %>% unique %>% length
n_returned <- id_return %>% length
n_removed  <- id_remove$learners %>% nrow

# -----------------------------------------------------------------------------




# Speech rate info ------------------------------------------------------------

mono_speech_rates_df <- sr %>% 
  pivot_longer(cols = c("speech_rate", "articulation_rate", "avg_syll_dur"), 
  names_to = "metric", values_to = "val") 

# Speech rate information of stimuli
mono_speech_rates_avg <- sr %>% 
  pivot_longer(cols = c("speech_rate", "articulation_rate", "avg_syll_dur"), 
  names_to = "metric", values_to = "val") %>% 
  group_by(speaker_variety, metric) %>% 
  summarize(avg_val = mean(val), .groups = "drop") %>% 
  pivot_wider(names_from = metric, values_from = avg_val) %>% 
  mutate(
    speaker_variety = if_else(speaker_variety == "castilian", "Penninsular", .$speaker_variety), 
    speaker_variety = str_to_title(speaker_variety)) %>% 
  rename(Variety = speaker_variety, `Articulation rate` = articulation_rate, 
    `Avg. syllable duration` = avg_syll_dur, `Speech rate` = speech_rate)

# List of mean/median values for in-prose reporting
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
  read_csv(here("data", "raw", "prolific_export_602aa93df732e9107ec837da_l2_reset.csv"), 
    col_select = c("status", "time_taken")), 
  read_csv(here("data", "raw", "prolific_export_602ab2f3ccc37d12d58ba79b_sp_mono.csv"), 
    col_select = c("status", "time_taken")), 
  read_csv(here("data", "raw", "prolific_export_605b45a4d25f287719a41696_l2_e.csv"), 
    col_select = c("status", "time_taken")),
  read_csv(here("data", "raw", "prolific_export_6057c54e64b0b4e5c128b4f9_l2_ne.csv"), 
    col_select = c("status", "time_taken")),
  read_csv(here("data", "raw", "prolific_export_60568611ec04488a9e9da93b_en_mono.csv"), 
    col_select = c("status", "time_taken"))
  ) %>% 
  select(status, time_taken) %>% 
  filter(status == "APPROVED") %>% 
  mutate(min = time_taken / 60) 

# Mean and median time to complete all tasks
t_mean   <- time$min %>% mean
t_median <- time$min %>% median
t_sd     <- time$min %>% sd

# -----------------------------------------------------------------------------






# LexTALE stuff ---------------------------------------------------------------

# List of mean, sd, min, max for in-prose printing
lt_dat <- learners %>% 
  distinct(participant, lextale_tra) %>% 
  summarize(mean_lt = mean(lextale_tra), sd_lt = sd(lextale_tra), 
    min_lt = min(lextale_tra), max_lt = max(lextale_tra)) %>% 
  pivot_longer(cols = everything(), names_to = "desc", values_to = "val") %>% 
  mutate_if(is.numeric, round, digits = 2) %>% 
  split(.$desc)

# -----------------------------------------------------------------------------




# Empathy stuff ---------------------------------------------------------------

# List of mean, sd, min, max for in-prose printing
eq_dat <- learners %>% 
  summarize(mean_eq = mean(eq_score), sd_eq = sd(eq_score), 
    min_eq = min(eq_score), max_eq = max(eq_score)) %>% 
  pivot_longer(cols = everything(), names_to = "desc", values_to = "val") %>% 
  mutate_if(is.numeric, round, digits = 2) %>% 
  split(.$desc)

# -----------------------------------------------------------------------------


