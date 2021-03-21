# Models ----------------------------------------------------------------------
#
# This script sources libraries and helpers and loads all tidy data
#
# -----------------------------------------------------------------------------



# Source libraries, helpers, load data ----------------------------------------

source(here::here("scripts", "r", "07_load_data.R"))

# -----------------------------------------------------------------------------




# Sample from prior distribution ----------------------------------------------

# Regularizing, weakly informative priors
nat_priors <- c(
  prior(normal(0, 2), class = "Intercept"), 
  prior(normal(0, 1.5), class = "sd")
)

native_ppred_response_00 <- brm(
  is_correct ~ 1 + 
    (1 | participant ) + 
    (1 | speaker_variety/sentence_type) + 
    (1 | sentence_type) + 
    (1 | sentence), 
  data = natives, 
  prior = nat_priors, 
  sample_prior = "only",
  warmup = 1000, iter = 2000, chains = 6, 
  family = "bernoulli", 
  cores = parallel::detectCores(), 
  file = here("models", "native_ppred_response_00")
)

ppred_response_samples <- posterior_samples(native_ppred_response_00) %>%
  rename(
    intercept = b_Intercept, 
    participant_sd = sd_participant__Intercept, 
    sentence_sd = sd_sentence__Intercept, 
    sentence_type_sd = sd_sentence_type__Intercept, 
    speaker_variety_sd = sd_speaker_variety__Intercept, 
    speaker_variety_sentence_type_sd = `sd_speaker_variety:sentence_type__Intercept`)

ppred_response_samples %>% 
  select(intercept:speaker_variety_sentence_type_sd) %>% 
  pivot_longer(intercept:speaker_variety_sentence_type_sd, names_to = "term") %>% 
  mutate(term = fct_relevel(term, "intercept", "participant_sd")) %>%
  ggplot(aes(value)) +
  geom_density(colour = NA, fill = "#325756", alpha = 0.9) +
  facet_wrap(~ term, scales = "free")

rt_priors <- c(
  set_prior("normal(6, 0.5)", class = "Intercept"), 
  set_prior("normal(0, 1.5)", class = "sd"), 
  set_prior("normal(0, 3)", class = "sd", group = "sentence_type"), 
  set_prior("normal(0, 0.5)", class = "sigma") 
  )

native_ppred_rt_00 <- brm(
  rt_adj ~ 1 + 
    (1 | participant ) + 
    (1 | speaker_variety/sentence_type) + 
    (1 | sentence_type) + 
    (1 | sentence), 
  data = natives %>% filter(rt_adj > 0, is_correct == 1), 
  prior = rt_priors, 
  sample_prior = "only",
  warmup = 1000, iter = 2000, chains = 6, 
  family = lognormal(), 
  cores = parallel::detectCores(), 
  file = here("models", "native_ppred_rt_00")
)

ppred_rt_samples <- posterior_samples(native_ppred_rt_00) %>%
  rename(
    intercept = b_Intercept, 
    participant_sd = sd_participant__Intercept, 
    sentence_sd = sd_sentence__Intercept, 
    sentence_type_sd = sd_sentence_type__Intercept, 
    speaker_variety_sd = sd_speaker_variety__Intercept, 
    speaker_variety_sentence_type_sd = `sd_speaker_variety:sentence_type__Intercept`)

ppred_rt_samples %>%
  select(intercept:speaker_variety_sentence_type_sd) %>% 
  pivot_longer(intercept:speaker_variety_sentence_type_sd, names_to = "term") %>% 
  mutate(term = fct_relevel(term, "intercept", "participant_sd")) %>%
  ggplot(aes(value)) +
  geom_density(colour = NA, fill = "#325756", alpha = 0.9) +
  facet_wrap(~ term, scales = "free")

# -----------------------------------------------------------------------------





# Fit model -------------------------------------------------------------------

native_response_01 <- brm(
  is_correct ~ 1 + 
    (1 | participant ) + 
    (1 | speaker_variety/sentence_type) + 
    (1 | sentence_type) + 
    (1 | sentence), 
  data = natives, 
  prior = nat_priors, 
  warmup = 2000, iter = 8000, chains = 6, 
  family = "bernoulli", 
  cores = parallel::detectCores(), 
  control = list(adapt_delta = 0.99), 
  file = here("models", "native_response_01")
)


native_rt_01 <- brm(
  formula = rt_adj ~ 1 + 
    (1 | participant ) + 
    (1 | speaker_variety/sentence_type) + 
    (1 | sentence_type) + 
    (1 | sentence), 
  data = natives %>% filter(rt_adj > 0, is_correct == 1), 
  prior = rt_priors, 
  warmup = 2000, iter = 8000, chains = 6, 
  family = lognormal(), 
  cores = parallel::detectCores(), 
  control = list(adapt_delta = 0.99, max_treedepth = 15), 
  file = here("models", "native_rt_01")
)

pp_check(native_response_01, nsamples = 200)
pp_check(native_rt_01, nsamples = 200)

# -----------------------------------------------------------------------------
