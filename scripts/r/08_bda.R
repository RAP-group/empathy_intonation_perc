# Models ----------------------------------------------------------------------
#
# This script sources libraries and helpers and loads all tidy data
#
# -----------------------------------------------------------------------------



# Source libraries, helpers, load data ----------------------------------------

source(here::here("scripts", "r", "07_load_data.R"))

# -----------------------------------------------------------------------------




# Native Spanish speakers -----------------------------------------------------

#
# Response data
#

# Set regularizing, weakly informative priors
nat_response_priors <- c(
  prior(normal(0, 2), class = "Intercept"), 
  prior(normal(0, 1.5), class = "sd")
)

# Sample from priors
native_ppred_response_00 <- brm(
  is_correct ~ 1 + 
    (1 | participant ) + 
    (1 | speaker_variety/sentence_type) + 
    (1 | sentence_type) + 
    (1 | item), 
  data = natives, 
  prior = nat_response_priors, 
  sample_prior = "only",
  warmup = 1000, iter = 2000, chains = 6, 
  family = "bernoulli", 
  cores = parallel::detectCores(), 
  file = here("models", "native_ppred_response_00")
)

# Wrangle and plot prior predictive checks
ppred_response_samples_natives <- posterior_samples(native_ppred_response_00) %>%
  rename(
    intercept = b_Intercept, 
    participant_sd = sd_participant__Intercept, 
    item_sd = sd_item__Intercept, 
    sentence_type_sd = sd_sentence_type__Intercept, 
    speaker_variety_sd = sd_speaker_variety__Intercept, 
    speaker_variety_sentence_type_sd = `sd_speaker_variety:sentence_type__Intercept`)

ppred_response_samples_natives %>% 
  select(intercept:speaker_variety_sentence_type_sd) %>% 
  pivot_longer(intercept:speaker_variety_sentence_type_sd, names_to = "term") %>% 
  mutate(term = fct_relevel(term, "intercept", "participant_sd")) %>%
  ggplot(aes(value)) +
  geom_density(colour = NA, fill = "#325756", alpha = 0.9) +
  facet_wrap(~ term, scales = "free")

# Fit model
native_response_01 <- brm(
  is_correct ~ 1 + 
    (1 | participant ) + 
    (1 | speaker_variety/sentence_type) + 
    (1 | sentence_type) + 
    (1 | item), 
  data = natives, 
  prior = nat_response_priors, 
  warmup = 1000, iter = 2000, chains = 4, 
  family = "bernoulli", 
  cores = parallel::detectCores(), 
  control = list(adapt_delta = 0.99), 
  file = here("models", "native_response_01")
)

pp_check(native_response_01, nsamples = 200)

# Wrangle and plot posteriors
post_response_samples_natives <- posterior_samples(native_response_01) %>%
  rename(
    intercept = b_Intercept, 
    participant_sd = sd_participant__Intercept, 
    item_sd = sd_item__Intercept, 
    sentence_type_sd = sd_sentence_type__Intercept, 
    speaker_variety_sd = sd_speaker_variety__Intercept, 
    speaker_variety_sentence_type_sd = `sd_speaker_variety:sentence_type__Intercept`)

post_response_samples_natives %>% 
  select(intercept:speaker_variety_sentence_type_sd) %>% 
  pivot_longer(intercept:speaker_variety_sentence_type_sd, names_to = "term") %>% 
  mutate(term = fct_relevel(term, "intercept", "participant_sd")) %>%
  ggplot(aes(value)) +
  geom_density(colour = NA, fill = "#325756", alpha = 0.9) +
  facet_wrap(~ term, scales = "free")





#
# RT data
#

# Set regularizing, weakly informative priors
nat_rt_priors <- c(
  set_prior("normal(6, 0.5)", class = "Intercept"), 
  set_prior("normal(0, 1.5)", class = "sd"), 
  set_prior("normal(0, 3)", class = "sd", group = "sentence_type"), 
  set_prior("normal(0, 0.5)", class = "sigma") 
  )

# Sample from priors
native_ppred_rt_00 <- brm(
  rt_adj ~ 1 + 
    (1 | participant ) + 
    (1 | speaker_variety/sentence_type) + 
    (1 | sentence_type) + 
    (1 | item), 
  data = natives %>% filter(rt_adj > 0, is_correct == 1), 
  prior = nat_rt_priors, 
  sample_prior = "only",
  warmup = 1000, iter = 2000, chains = 6, 
  family = lognormal(), 
  cores = parallel::detectCores(), 
  file = here("models", "native_ppred_rt_00")
)

# Wrangle and plot prior predictive checks
ppred_rt_samples_natives <- posterior_samples(native_ppred_rt_00) %>%
  rename(
    intercept = b_Intercept, 
    participant_sd = sd_participant__Intercept, 
    item_sd = sd_item__Intercept, 
    sentence_type_sd = sd_sentence_type__Intercept, 
    speaker_variety_sd = sd_speaker_variety__Intercept, 
    speaker_variety_sentence_type_sd = `sd_speaker_variety:sentence_type__Intercept`)

ppred_rt_samples_natives %>%
  select(intercept:speaker_variety_sentence_type_sd) %>% 
  pivot_longer(intercept:speaker_variety_sentence_type_sd, names_to = "term") %>% 
  mutate(term = fct_relevel(term, "intercept", "participant_sd")) %>%
  ggplot(aes(value)) +
  geom_density(colour = NA, fill = "#325756", alpha = 0.9) +
  facet_wrap(~ term, scales = "free")

# Fit model
native_rt_01 <- brm(
  formula = rt_adj ~ 1 + 
    (1 | participant ) + 
    (1 | speaker_variety/sentence_type) + 
    (1 | sentence_type) + 
    (1 | item), 
  data = natives %>% filter(rt_adj > 0, is_correct == 1), 
  prior = nat_rt_priors, 
  warmup = 1000, iter = 2000, chains = 4, 
  family = lognormal(), 
  cores = parallel::detectCores(), 
  control = list(adapt_delta = 0.99, max_treedepth = 15), 
  file = here("models", "native_rt_01")
)

# Posterior predictive check
pp_check(native_rt_01, nsamples = 200)

# Wrangle and plot posterior
post_rt_samples_natives <- posterior_samples(native_rt_01) %>%
  rename(
    intercept = b_Intercept, 
    participant_sd = sd_participant__Intercept, 
    item_sd = sd_item__Intercept, 
    sentence_type_sd = sd_sentence_type__Intercept, 
    speaker_variety_sd = sd_speaker_variety__Intercept, 
    speaker_variety_sentence_type_sd = `sd_speaker_variety:sentence_type__Intercept`)

post_rt_samples_natives %>%
  select(intercept:speaker_variety_sentence_type_sd) %>% 
  pivot_longer(intercept:speaker_variety_sentence_type_sd, names_to = "term") %>% 
  mutate(term = fct_relevel(term, "intercept", "participant_sd")) %>%
  ggplot(aes(value)) +
  geom_density(colour = NA, fill = "#325756", alpha = 0.9) +
  facet_wrap(~ term, scales = "free")

# -----------------------------------------------------------------------------









#
# L2 models
#

l2_response_priors <- c(
  prior(normal(0, 2), class = "Intercept"), 
  prior(normal(0, 1.5), class = "sd")
)

learner_response_01_noslope <- brm(
  formula = is_correct ~ lextale_std * eq_std + 
    (1 | participant) + 
    (1 | speaker_variety/sentence_type) + 
    (1 | sentence_type) + 
    (1 | item), 
  data = learners %>% filter(rt_adj < 5), 
  prior = l2_response_priors, 
  warmup = 1000, iter = 2000, chains = 2, 
  family = "bernoulli", 
  cores = 2, 
  control = list(adapt_delta = 0.99, max_treedepth = 20), 
  file = here("models", "learner_response_01_noslope")
)


#  prior(normal(0, 2), class = "Intercept"), 
#  prior(normal(0, 1.5), class = "sd")
  
l2_response_priors <- c(
  prior(normal(0, 0.3), class = b),
  prior(normal(0, 0.1), class = sd), 
  prior(lkj(8), class = cor)
)

learner_response_01 <- brm(
  formula = is_correct ~ 0 + Intercept + sentence_type * lextale_std * eq_std + 
    (1 + sentence_type | participant) + 
    (1 + lextale_std * eq_std | speaker_variety) + 
    (1 | item), 
  data = learners %>% filter(rt_adj < 10),
  prior = l2_response_priors, 
  warmup = 2000, iter = 4000, chains = 4, 
  family = "bernoulli", 
  cores = 4, 
  control = list(adapt_delta = 0.99, max_treedepth = 20), 
  file = here("models", "learner_response_01")
)

conditional_effects(learner_response_01)
bayestestR::describe_posterior(
  learner_response_01, rope_range = c(-0.1, 0.1), rope_ci = 0.95, 
  ci = 0.95)

learner_response_q_01 <- brm(
  formula = is_correct ~ 0 + Intercept + is_question * lextale_std * eq_std + 
    (1 + is_question | participant) + 
    (1 + lextale_std * eq_std | speaker_variety) + 
    (1 | item), 
  data = learners %>% filter(rt_adj < 10, is_question %in% c(-1, 1)),
  prior = l2_response_priors, 
  warmup = 2000, iter = 4000, chains = 4, 
  family = "bernoulli", 
  cores = 4, 
  control = list(adapt_delta = 0.99, max_treedepth = 20), 
  file = here("models", "learner_response_q_01")
)

conditional_effects(learner_response_q_01)
bayestestR::describe_posterior(
  learner_response_q_01, rope_range = c(-0.1, 0.1), rope_ci = 0.95, 
  ci = 0.95)


learner_response_qonly_01 <- brm(
  formula = is_correct ~ 0 + Intercept + q_type * lextale_std * eq_std + 
    (1 + q_type | participant) + 
    (1 + lextale_std * eq_std | speaker_variety) + 
    (1 | item), 
  data = learners %>% filter(rt_adj < 10, is_question == 1),
  prior = l2_response_priors, 
  warmup = 2000, iter = 4000, chains = 4, 
  family = "bernoulli", 
  cores = 4, 
  control = list(adapt_delta = 0.99, max_treedepth = 20), 
  file = here("models", "learner_response_qonly_01")
)

conditional_effects(learner_response_qonly_01)
bayestestR::describe_posterior(
  learner_response_qonly_01, rope_range = c(-0.1, 0.1), rope_ci = 0.95, 
  ci = 0.95)

# -----------------------------------------------------------------------------
