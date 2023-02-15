# Small data ------------------------------------------------------------------
#
# Author: Joseph V. Casillas
# Last update: 20230215
#
# - script to be loaded in main.Rmd
# - used for reporting "smaller" descriptives (such as N, means, etc.)
# - tables and model summaries are also loaded here for in-prose reporting
#
# -----------------------------------------------------------------------------



# Source helpers, libs, and posteriors ----------------------------------------

source(here::here("scripts", "r", "07_load_data.R"))
accuracy_summary <- read_csv(here("tables", "learner_response_01.csv"))
learner_response_01 <- readRDS(here("models", "learner_response_01.rds"))
ddm_summary <- read_csv(here("tables", "ddm_bs_dr.csv"))
variety_familiarity_summary <- 
  read_csv(here("tables", "learner_variety_match_response_model_summary.csv"))
learner_native_risefall_accuracy <- 
  read_csv(here("tables", "learner_native_risefall_accuracy.csv"))

# -----------------------------------------------------------------------------



# Learner descriptives --------------------------------------------------------

n_learners <- learners$participant %>% unique %>% length
n_returned <- id_return %>% length
n_removed  <- id_remove$learners %>% nrow
n_natives  <- natives$participant %>% unique %>% length

# -----------------------------------------------------------------------------




# Learner accuracy mod descriptives -------------------------------------------

# Get model posterior
lr01_post <- as_tibble(learner_response_01)

# Get lextale effect posterior for wh- questions and declaratives
lt_wh <- transmute(lr01_post, 
  lex_wh = b_lextale_std + `b_sentence_typeinterrogativeMpartialMwh:lextale_std`) %>% 
  my_posterior_summary()

lt_bf <- transmute(lr01_post, 
  lex_wh = b_lextale_std + `b_sentence_typedeclarativeMbroadMfocus:lextale_std`) %>% 
  my_posterior_summary()

lt_nf <- transmute(lr01_post, 
  lex_wh = b_lextale_std + `b_sentence_typedeclarativeMnarrowMfocus:lextale_std`) %>% 
  my_posterior_summary()

# Get EQ effect posterior for wh- questions and declaratives
eq_wh <- transmute(lr01_post, 
  eq_wh = b_eq_std + `b_sentence_typeinterrogativeMpartialMwh:eq_std`) %>% 
  my_posterior_summary()

eq_bf <- transmute(lr01_post, 
  eq_wh = b_eq_std + `b_sentence_typedeclarativeMbroadMfocus:eq_std`) %>% 
  my_posterior_summary()

eq_nf <- transmute(lr01_post, 
  eq_wh = b_eq_std + `b_sentence_typedeclarativeMnarrowMfocus:eq_std`) %>% 
  my_posterior_summary()

# Get LT x EQ effect posterior for wh- questions and declaratives
lt_eq_wh <- transmute(lr01_post, 
  lt_eq_wh = `b_lextale_std:eq_std` + `b_sentence_typeinterrogativeMpartialMwh:lextale_std:eq_std`) %>% 
  my_posterior_summary()

lt_eq_bf <- transmute(lr01_post, 
  lt_eq_bf = `b_lextale_std:eq_std` + `b_sentence_typedeclarativeMbroadMfocus:lextale_std:eq_std`) %>% 
  my_posterior_summary()

lt_eq_nf <- transmute(lr01_post, 
  lt_eq_nf = `b_lextale_std:eq_std` + `b_sentence_typedeclarativeMnarrowMfocus:lextale_std:eq_std`) %>% 
  my_posterior_summary()

# Mean difference in accuracy between wh- and y/n questions
wh_yn_diff <- transmute(lr01_post, 
  yn = plogis(b_Intercept), 
  wh = plogis(b_Intercept + b_sentence_typeinterrogativeMpartialMwh), 
  diff = (wh - yn) * 100
  ) %>% 
  select(diff) %>% 
  my_posterior_summary() %>% 
  str_replace("\\(", "(Mean difference: ")


# -----------------------------------------------------------------------------




# Speech rate info ------------------------------------------------------------

# Grand mean 
sr_grand_mean <- sr %>% 
  summarize(avg = mean(articulation_rate)) %>% 
  pull() %>% 
  round(digits = 2)

# Pivot to long form
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




# Variety familiarity ---------------------------------------------------------

familiarity_hold <- learners %>% 
  group_by(participant, spn_variety) %>% 
  distinct(spn_variety) %>% 
  group_by(spn_variety) %>% 
  summarize(n_v = n()) %>% 
  mutate(sum_n = sum(n_v), perc = round((n_v / sum_n), 2), 
    spn_variety = str_replace(spn_variety, " ", "")) %>% 
  arrange(desc(perc)) 

familiarity_table <- familiarity_hold %>% 
  transmute(Variety = case_when(
    spn_variety == "UnitedStates" ~ "U.S. Spanish", 
    spn_variety == "Iam not familiar with Spanish" ~ "Not familiar", 
    spn_variety == "CostaRica" ~ "Costa Rica", 
    spn_variety == "PuertoRico" ~ "Puerto Rico", 
    spn_variety == "DominicanRepublic" ~ "Dominican Republic", 
    spn_variety == "Spain" ~ "Peninsular", 
    TRUE ~ .$spn_variety), 
    n = n_v, Proportion = perc)

familiarity <- familiarity_hold %>% 
  mutate(perc = perc * 100) %>% 
  split(.$spn_variety)

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

# Posterior means and HDIs
lt_eq_descriptives <- 
  read_csv(here("tables", "lextale_empathy_descriptives.csv"))

# Just lextale and eq for each participant
eq_lt_df <- learners %>% 
  select(participant, eq_score, lextale_tra, lextale_std, eq_std) %>% 
  distinct() 

# Fit linear regression to estimate correlation
eq_lt_mod <- brm(
  data = eq_lt_df, 
  family = gaussian,
  formula = lextale_std ~ eq_std,
  prior = c(prior(normal(0, 0.1), class = Intercept), 
            prior(cauchy(0, 0.1), class = sigma)), 
  chains = 4, cores = 4, seed = 1, 
  file = here("models", "eq_lextale_cor")
  )

#
# Lextale x EQ correleations
#

# From accuracy model
r_eq_lt_bf <- report_posterior(accuracy_summary, param = "Broad focus:LexTALE:EQ")
r_eq_lt_nf <- report_posterior(accuracy_summary, param = "Narrow focus:LexTALE:EQ")
r_eq_lt_yn <- report_posterior(accuracy_summary, param = "LexTALE:EQ")
r_eq_lt_wh <- report_posterior(accuracy_summary, param = "Wh- question:LexTALE:EQ")

# Get posterior of lextale x eq correlation from grouping effects
r_eq_lt_random_post <- learner_response_01 %>% 
  as_draws_df() %>% 
  select(r_lt_eq = cor_speaker_variety__lextale_std__eq_std) %>% 
  pull() %>% 
  describe_posterior() %>% 
  as_tibble() %>% 
  mutate_if(is.numeric, specify_decimal, k = 3) %>% 
  transmute(Parameter, Median, HDI = glue("[{CI_low}, {CI_high}]"), 
            `% in ROPE` = ROPE_Percentage, MPE = pd)

# Get posterior of lextale x eq correlation
r_eq_lt_post <- eq_lt_mod %>% 
  as_draws_df() %>% 
  select(b_eq_std) %>% 
  pull() %>% 
  describe_posterior() %>% 
  as_tibble() %>% 
  mutate_if(is.numeric, specify_decimal, k = 3) %>% 
  transmute(Parameter, Median, HDI = glue("[{CI_low}, {CI_high}]"), 
            `% in ROPE` = ROPE_Percentage, MPE = pd) %>% 
  report_posterior(param = "Posterior")

# -----------------------------------------------------------------------------


# RT stuff --------------------------------------------------------------------

#
# This is the justification for using adjusted RTs
#

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
