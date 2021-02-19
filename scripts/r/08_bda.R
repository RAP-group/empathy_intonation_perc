# Models ----------------------------------------------------------------------
#
# This script sources libraries and helpers and loads all tidy data
#
# -----------------------------------------------------------------------------



# Source libraries, helpers, load data ----------------------------------------

source(here::here("scripts", "r", "07_load_data.R"))

# -----------------------------------------------------------------------------


# Regularizing, weakly informative priors
nat_priors <- c(
  prior(normal(0, 2), class = "Intercept"), 
  prior(cauchy(0, 1), class = "sd")
)

native_ppred_response_00 <- brm(
  is_correct ~ 1 + 
    (1 | participant ) + (1 | sentence) + (1 | speaker_variety), 
  data = natives, 
  prior = nat_priors, 
  sample_prior = "only",
  warmup = 1000, iter = 2000, chains = 6, 
  family = "bernoulli", 
  cores = parallel::detectCores(), 
  file = here("models", "native_ppred_response_00")
)

native_ppred_rt_00 <- brm(
  rt_adj ~ 1 + 
    (1 | participant ) + (1 | sentence) + (1 | speaker_variety), 
  data = natives %>% filter(rt_adj > 0), 
  prior = nat_priors, 
  sample_prior = "only",
  warmup = 1000, iter = 2000, chains = 6, 
  family = shifted_lognormal(), 
  cores = parallel::detectCores(), 
  file = here("models", "native_ppred_rt_00")
)

ppred_response_samples <- posterior_samples(native_ppred_response_00) %>%
  rename(intercept = b_Intercept, 
         participant_sd = sd_participant__Intercept, 
         sentence_sd = sd_sentence__Intercept, 
         speaker_variety_sd = sd_speaker_variety__Intercept)

ppred_rt_samples <- posterior_samples(native_ppred_rt_00) %>%
  rename(intercept = b_Intercept, 
         participant_sd = sd_participant__Intercept, 
         sentence_sd = sd_sentence__Intercept, 
         speaker_variety_sd = sd_speaker_variety__Intercept)

ppred_response_samples %>%
  pivot_longer(intercept:speaker_variety_sd, names_to = "term") %>% 
  mutate(term = fct_relevel(term, "intercept", "participant_sd")) %>%
  ggplot(aes(value)) +
  geom_density(colour = NA, fill = "#325756", alpha = 0.9) +
  facet_wrap(~ term, scales = "free")

ppred_rt_samples %>%
  pivot_longer(intercept:speaker_variety_sd, names_to = "term") %>% 
  mutate(term = fct_relevel(term, "intercept", "participant_sd")) %>%
  ggplot(aes(value)) +
  geom_density(colour = NA, fill = "#325756", alpha = 0.9) +
  facet_wrap(~ term, scales = "free")


# Fit model
native_response_00 <- brm(
  is_correct ~ 1 + 
    (1 | participant ) + (1 | sentence) + (1 | speaker_variety), 
  data = natives, 
  prior = nat_priors, 
  warmup = 1000, iter = 2000, chains = 6, 
  family = "bernoulli", 
  cores = parallel::detectCores(), 
  control = list(adapt_delta = 0.99), 
  file = here("models", "native_response_00")
)

pp_check(native_response_00, nsamples = 200)
pairs(native_response_00)

post_samples <- posterior_samples(native_response_00) %>%
  rename(intercept = b_Intercept, 
         participant_sd = sd_participant__Intercept, 
         sentence_sd = sd_sentence__Intercept, 
         speaker_variety_sd = sd_speaker_variety__Intercept)

# Grouping variable posteriors
posterior_samples(native_response_00) %>% 
  select(starts_with("sd")) %>% 
  gather(key, sd) %>% 
  mutate(key = str_remove(key, "sd_") %>% 
           str_remove(., "__Intercept")) %>% 
  ggplot(aes(x = sd, fill = key)) +
  geom_density(color = "transparent", alpha = 0.9) +
  scale_fill_viridis_d(name = "Group-level variance", option = "D", end = 0.6, 
    labels = c("Participant", "Sentence", "Speaker variety")) + 
  coord_cartesian(xlim = c(0, 3)) + 
  labs(y = "Density", x = expression(beta)) + 
  minimal_adj() + 
  theme(legend.position = c(0.85, 0.75), legend.title = element_text(size = 9), 
    legend.text = element_text(size = 8), legend.key.size = unit(0.5, "cm"))




# By participant

# Get draws for each study
participant_draws <- 
  spread_draws(native_response_00, r_participant[Participant,], b_Intercept) %>% 
  mutate(b_Intercept = r_participant + b_Intercept)

# Get draws for pooled effect
participant_pooled_effect_draws <- 
  spread_draws(native_response_00, b_Intercept) %>% 
  mutate(Participant = "Pooled Effect")

# Combine it and clean up
participant_forest_data <- 
  bind_rows(participant_draws, participant_pooled_effect_draws) %>% 
  ungroup() %>%
  mutate(Participant = reorder(Participant, b_Intercept), 
         Participant = relevel(Participant, "Pooled Effect", after = Inf))

# Calculate mean qi intervals for right margin text
participant_forest_data_summary <- 
  group_by(participant_forest_data, Participant) %>% 
  mean_qi(b_Intercept, .width = 0.95) 

# Calculate mean qi intervals for pooled effect
participant_pooled_summary <- 
  group_by(participant_forest_data, Participant) %>% 
  mean_qi(b_Intercept, .width = c(0.5, 0.8, 0.95)) %>% 
  filter(Participant == "Pooled Effect")

# Plot it all
p_post_participant <- participant_forest_data %>% 
  filter(Participant != "Pooled Effect") %>% 
  ggplot() + 
  aes(x = b_Intercept, y = Participant) + 
    geom_text(data = 
    mutate_if(participant_forest_data_summary, is.numeric, round, 2) %>% 
    mutate_at(c("b_Intercept", ".lower", ".upper"), as.character) %>% 
    mutate_at(c("b_Intercept", ".lower", ".upper"), unicode_minus) %>% 
    mutate_at(c("b_Intercept", ".lower", ".upper"), strip_blank),
            aes(label = glue("{b_Intercept} [{.lower}, {.upper}]"), x = Inf), 
            hjust = "inward", family = "Times", alpha = 0) + 
  geom_tile(data = participant_pooled_summary, aes(width = .lower - .upper),
    alpha = 0.75, height = Inf, fill = "#31688EFF") +
  stat_pointinterval(point_fill = "white", shape = 21, alpha = 0.85, 
    .width = c(0.8, 0.95), point_size = 1.5) +
  coord_cartesian(xlim = c(0, 5)) + 
  labs(x = expression(beta), y = NULL) +
  theme_test(base_size = 12) + 
  theme(axis.text.y = element_blank(), axis.ticks.y = element_blank())








# By sentence

# Get draws for each study
sentence_draws <- 
  spread_draws(native_response_00, r_sentence[Sentence,], b_Intercept) %>% 
  mutate(b_Intercept = r_sentence + b_Intercept)

# Get draws for pooled effect
sentence_pooled_effect_draws <- 
  spread_draws(native_response_00, b_Intercept) %>% 
  mutate(Sentence = "Pooled Effect")

# Combine it and clean up
sentence_forest_data <- 
  bind_rows(sentence_draws, sentence_pooled_effect_draws) %>% 
  ungroup() %>%
  mutate(Sentence = reorder(Sentence, b_Intercept), 
         Sentence = relevel(Sentence, "Pooled Effect", after = Inf))

# Calculate mean qi intervals for right margin text
sentence_forest_data_summary <- 
  group_by(sentence_forest_data, Sentence) %>% 
  mean_qi(b_Intercept, .width = 0.95) 

# Calculate mean qi intervals for pooled effect
sentence_pooled_summary <- 
  group_by(sentence_forest_data, Sentence) %>% 
  mean_qi(b_Intercept, .width = c(0.5, 0.8, 0.95)) %>% 
  filter(Sentence == "Pooled Effect")

# Plot it all
p_post_sentence <- sentence_forest_data %>% 
  ggplot() + 
  aes(x = b_Intercept, y = Sentence) + 
  geom_text(data = 
    mutate_if(sentence_forest_data_summary, is.numeric, round, 2) %>% 
    mutate_at(c("b_Intercept", ".lower", ".upper"), as.character) %>% 
    mutate_at(c("b_Intercept", ".lower", ".upper"), unicode_minus) %>% 
    mutate_at(c("b_Intercept", ".lower", ".upper"), strip_blank),
            aes(label = glue("{b_Intercept} [{.lower}, {.upper}]"), x = Inf), 
            hjust = "inward", family = "Times") + 
  geom_tile(data = sentence_pooled_summary, aes(width = .lower - .upper),
    alpha = 0.2, height = Inf, fill = "#31688EFF") +
  stat_pointinterval(point_fill = "white", shape = 21, .width = c(0.8, 0.95)) +
  coord_cartesian(xlim = c(0, 6)) + 
  labs(x = expression(italic("beta")), y = NULL) +
  minimal_adj(base_size = 12) + 
  theme(axis.text.y = element_text(hjust = 0))






# By speaker variety

# Get draws for each study
speaker_variety_draws <- 
  spread_draws(native_response_00, r_speaker_variety[Speaker_variety,], b_Intercept) %>% 
  mutate(b_Intercept = r_speaker_variety + b_Intercept)

# Get draws for pooled effect
speaker_variety_pooled_effect_draws <- 
  spread_draws(native_response_00, b_Intercept) %>% 
  mutate(Speaker_variety = "Pooled Effect")

# Combine it and clean up
speaker_variety_forest_data <- 
  bind_rows(speaker_variety_draws, speaker_variety_pooled_effect_draws) %>% 
  ungroup() %>%
  mutate(Speaker_variety = reorder(Speaker_variety, b_Intercept), 
         Speaker_variety = relevel(Speaker_variety, "Pooled Effect", after = Inf))

# Calculate mean qi intervals for right margin text
speaker_variety_forest_data_summary <- 
  group_by(speaker_variety_forest_data, Speaker_variety) %>% 
  mean_qi(b_Intercept, .width = 0.95) 

# Calculate mean qi intervals for pooled effect
speaker_variety_pooled_summary <- 
  group_by(speaker_variety_forest_data, Speaker_variety) %>% 
  mean_qi(b_Intercept, .width = c(0.5, 0.8, 0.95)) %>% 
  filter(Speaker_variety == "Pooled Effect")

# Plot it all
p_post_speaker_variety <- speaker_variety_forest_data %>% 
  ggplot() + 
  aes(x = b_Intercept, y = Speaker_variety) + 
  geom_text(data = 
    mutate_if(speaker_variety_forest_data_summary, is.numeric, round, 2) %>% 
    mutate_at(c("b_Intercept", ".lower", ".upper"), as.character) %>% 
    mutate_at(c("b_Intercept", ".lower", ".upper"), unicode_minus) %>% 
    mutate_at(c("b_Intercept", ".lower", ".upper"), strip_blank),
            aes(label = glue("{b_Intercept} [{.lower}, {.upper}]"), x = Inf), 
            hjust = "inward", family = "Times") + 
  geom_tile(data = speaker_variety_pooled_summary, aes(width = .lower - .upper),
    alpha = 0.2, height = Inf, fill = "#31688EFF") +
  stat_pointinterval(point_fill = "white", shape = 21, .width = c(0.8, 0.95)) +
  coord_cartesian(xlim = c(0, 6)) + 
  labs(x = expression(italic("beta")), y = NULL) +
  minimal_adj() + 
  theme(axis.text.y = element_text(hjust = 0))

p_post_participant
p_post_sentence
p_post_speaker_variety
