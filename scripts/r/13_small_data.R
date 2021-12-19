# Small data ------------------------------------------------------------------
#
# - script to be loaded in main.Rmd
# - used for reporting "smaller" descriptives (such as N, means, etc.)
# - tables and model summaries are also loaded here for in-prose reporting
#
# -----------------------------------------------------------------------------



# Source helpers, and libs ----------------------------------------------------

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
  dplyr::filter(status == "APPROVED") %>% 
  mutate(min = time_taken / 60) 

# Mean time to complete all tasks
t_mean <- time$min %>% mean

# -----------------------------------------------------------------------------




# LexTALE and Empathy quotient descriptives -----------------------------------

lt_eq_descriptives <- read_csv(here("tables", "lextale_empathy_descriptives.csv"))

# -----------------------------------------------------------------------------


# RT stuff --------------------------------------------------------------------

# N trials in which RT > 10
n_rt_10_plus <- learners %>% filter(rt_adj > 10) %>% nrow

# % of RT > 10 RTs
perc_rt_10_plus <- round((n_rt_10_plus / nrow(learners)) * 100, 2)

# N trials in which RT is negative (before offset of sentence)
n_rt_0_minus <- learners %>% filter(rt_adj < 0) %>% nrow

# % of negative RTs
perc_rt_neg <- round((n_rt_0_minus / nrow(learners)) * 100, 2)

# Accuracy of negative RTs
perc_rt_neg_correct <- filter(learners, rt_adj < 0) %>% 
  summarize(a = mean(is_correct) * 100) %>% pull() %>% round(., 2)
