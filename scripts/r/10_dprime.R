# Dprime Models ---------------------------------------------------------------
#
# Author: Joseph V. Casillas
# Last update: 20220123
#
# - This script contains exploratory d' analyses, plots, and tables
#
# -----------------------------------------------------------------------------



# Source libraries, helpers, load data ----------------------------------------

source(here::here("scripts", "r", "07_load_data.R"))

# -----------------------------------------------------------------------------



# Calculate dprime ------------------------------------------------------------

learners_hr_fa <- learners %>% 
  mutate(
    response_type = case_when(
      sentence_type == "interrogative-partial-wh" & correct_response == 1 & response == 1 ~ "hit",
      sentence_type == "interrogative-partial-wh" & correct_response == 1 & response == 0 ~ "miss",
      sentence_type == "interrogative-partial-wh" & correct_response == 0 & response == 0 ~ "cr", 
      sentence_type == "interrogative-partial-wh" & correct_response == 0 & response == 1 ~ "fa",

      sentence_type == "interrogative-total-yn" & correct_response == 1 & response == 1 ~ "hit",
      sentence_type == "interrogative-total-yn" & correct_response == 1 & response == 0 ~ "miss",
      sentence_type == "interrogative-total-yn" & correct_response == 0 & response == 0 ~ "cr",
      sentence_type == "interrogative-total-yn" & correct_response == 0 & response == 1 ~ "fa",

      sentence_type == "declarative-narrow-focus" & correct_response == 1 & response == 1 ~ "hit",
      sentence_type == "declarative-narrow-focus" & correct_response == 1 & response == 0 ~ "miss",
      sentence_type == "declarative-narrow-focus" & correct_response == 0 & response == 0 ~ "cr",
      sentence_type == "declarative-narrow-focus" & correct_response == 0 & response == 1 ~ "fa",

      sentence_type == "declarative-broad-focus" & correct_response == 1 & response == 1 ~ "hit",
      sentence_type == "declarative-broad-focus" & correct_response == 1 & response == 0 ~ "miss",
      sentence_type == "declarative-broad-focus" & correct_response == 0 & response == 0 ~ "cr",
      sentence_type == "declarative-broad-focus" & correct_response == 0 & response == 1 ~ "fa"
    )
  ) %>% 
  mutate(
    is_hit = if_else(response_type == "hit", 1, 0),
    is_fa = if_else(response_type == "fa", 1, 0),
    is_miss = if_else(response_type == "miss", 1, 0),
    is_cr = if_else(response_type == "cr", 1, 0)
  ) 

learners_dp_sv <- learners_hr_fa %>% 
  group_by(participant, speaker_variety, lextale_std, eq_std) %>% 
  summarize(
    n_hit = sum(is_hit),
    n_fa = sum(is_fa),
    n_miss = sum(is_miss),
    n_cr = sum(is_cr), .groups = "drop") %>% 
  mutate(n_targets = n_hit + n_miss,
         n_distractors = n_fa + n_cr) %>% 
  filter(n_distractors != 0) %>% # remove 21 rows where n_fa + n_cr == 0 
  mutate(dprime = dprime(n_hit, n_fa, n_miss, n_cr, adjusted = T), 
    speaker_variety = fct_relevel(speaker_variety, "castilian")) %>% 
  left_join(., 
    group_by(learners, participant, spn_variety) %>% 
    summarize(.groups = "drop"), by = "participant")

learners_dp_st <- learners_hr_fa %>% 
  group_by(participant, sentence_type, lextale_std, eq_std) %>% 
  summarize(
    n_hit = sum(is_hit) + 1,
    n_fa = sum(is_fa) + 1,
    n_miss = sum(is_miss) + 1,
    n_cr = sum(is_cr) + 1, .groups = "drop") %>% 
  mutate(n_targets = n_hit + n_miss,
         n_distractors = n_fa + n_cr) %>% 
  mutate(dprime = dprime(n_hit, n_fa, n_miss, n_cr, adjusted = T), 
    sentence_type = fct_relevel(sentence_type, "interrogative-total-yn")) %>% 
  left_join(., 
    group_by(learners, participant, spn_variety) %>% 
    summarize(.groups = "drop"), by = "participant")

# -----------------------------------------------------------------------------




# Models ----------------------------------------------------------------------

learner_dp_01 <- brm(
  formula = dprime ~ 0 + Intercept + speaker_variety + lextale_std + 
    (1 + speaker_variety | participant), 
  family = "gaussian", 
  data = learners_dp_sv, 
  prior = c(
    prior(normal(2, 1.5), class = b, coef = "Intercept"),
    prior(normal(0, 1.5), class = b),
    prior(cauchy(0, 1), class = sd), 
    prior(lkj(2), class = cor)), 
  warmup = 2000, iter = 4000, chains = 4, cores = 4, 
  backend = "cmdstanr", 
  file = here("models", "learner_dp_01")
)

learner_dp_02 <- brm(
  formula = dprime ~ 0 + Intercept + sentence_type + lextale_std + 
    (1 + sentence_type | participant), 
  family = "gaussian", 
  data = learners_dp_st, 
  prior = c(
    prior(normal(2, 1.5), class = b, coef = "Intercept"),
    prior(normal(0, 1.5), class = b),
    prior(cauchy(0, 1), class = sd), 
    prior(lkj(2), class = cor)), 
  warmup = 2000, iter = 4000, chains = 4, cores = 4, 
  control = list(adapt_delta = 0.9999, max_treedepth = 15), 
  backend = "cmdstanr", 
  file = here("models", "learner_dp_02")
)

# -----------------------------------------------------------------------------





# Exploratory plots -----------------------------------------------------------

plot_dp_variety <- as_tibble(learner_dp_01) %>% 
  select("b_Intercept", starts_with("b_speaker_variety"), -contains(":")) %>% 
  transmute(
    Madrileño = b_Intercept, 
    Andalusian = b_Intercept + b_speaker_varietyandalusian, 
    Argentine = b_Intercept + b_speaker_varietyargentine, 
    Chilean = b_Intercept + b_speaker_varietychilean, 
    Cuban = b_Intercept + b_speaker_varietycuban, 
    Mexican = b_Intercept + b_speaker_varietymexican, 
    Peruvian = b_Intercept + b_speaker_varietyperuvian, 
    `Puerto Rican` = b_Intercept + b_speaker_varietypuertorican
  ) %>% 
  pivot_longer(everything(), names_to = "variety", values_to = "estimate") %>% 
  mutate(variety = fct_reorder(variety, estimate, max)) %>% 
  ggplot(., aes(x = estimate, y = variety)) + 
    stat_halfeye(slab_fill = "#cc0033", pch = 21, point_fill = "white") + 
    scale_y_discrete(position = "right") + 
    labs(y = "Variety", x = "d'") + 
    ds4ling::ds4ling_bw_theme(base_family = "Times", base_size = 13) + 
    theme(axis.title.y = element_text(hjust = 0.05))

plot_dp_utterance <- as_tibble(learner_dp_02) %>% 
  select("b_Intercept", starts_with("b_sentence_type"), -contains(":")) %>% 
  transmute(
    `Interrogative\ny/n` = b_Intercept, 
    `Interrogative\nwh-` = b_Intercept + b_sentence_typeinterrogativeMpartialMwh, 
    `Declarative\nnarrow focus` = b_Intercept + b_sentence_typedeclarativeMnarrowMfocus, 
    `Declarative\nbroad focus` = b_Intercept + b_sentence_typedeclarativeMbroadMfocus
  ) %>% 
  pivot_longer(everything(), names_to = "ut", values_to = "estimate") %>% 
  mutate(ut = fct_reorder(ut, estimate, max)) %>% 
  ggplot(., aes(x = estimate, y = ut)) + 
    stat_halfeye(slab_fill = "#cc0033", pch = 21, point_fill = "white") + 
    labs(y = "Utterance type", x = "d'") + 
    ds4ling::ds4ling_bw_theme(base_family = "Times", base_size = 13)

learner_dp_utterance_variety <- plot_dp_utterance + plot_dp_variety

# -----------------------------------------------------------------------------




# Save plots ------------------------------------------------------------------

devices     <- c('png', 'pdf')
path_to_fig <- file.path(here("figs", "manuscript"))

walk(devices, ~ ggsave(
  filename = glue(path_to_fig, "/learner_dp_utterance_variety.", .x), 
  plot = learner_dp_utterance_variety, 
  device = .x, height = 4, width = 8.5, units = "in"))

# -----------------------------------------------------------------------------




# Tables ----------------------------------------------------------------------

bind_rows(
  as_tibble(learner_dp_02) %>% 
    select("b_Intercept", starts_with("b_sentence_type"), -contains(":")) %>% 
    transmute(
      `Interrogative y/n` = b_Intercept, 
      `Interrogative wh-` = b_Intercept + b_sentence_typeinterrogativeMpartialMwh, 
      `Declarative narrow focus` = b_Intercept + b_sentence_typedeclarativeMnarrowMfocus, 
      `Declarative broad focus` = b_Intercept + b_sentence_typedeclarativeMbroadMfocus
    ) %>% 
    pivot_longer(everything(), names_to = "Parameter", values_to = "estimate") %>% 
    mutate(Model = "Utterance type"), 

  as_tibble(learner_dp_01) %>% 
    select("b_Intercept", starts_with("b_speaker_variety"), -contains(":")) %>% 
    transmute(
      Peninsular = b_Intercept, 
      Andalusian = b_Intercept + b_speaker_varietyandalusian, 
      Argentine = b_Intercept + b_speaker_varietyargentine, 
      Chilean = b_Intercept + b_speaker_varietychilean, 
      Cuban = b_Intercept + b_speaker_varietycuban, 
      Mexican = b_Intercept + b_speaker_varietymexican, 
      Peruvian = b_Intercept + b_speaker_varietyperuvian, 
      `Puerto Rican` = b_Intercept + b_speaker_varietypuertorican
    ) %>% 
    pivot_longer(everything(), names_to = "Parameter", values_to = "estimate") %>% 
    mutate(Model = "Variety")) %>% 
  group_by(Model, Parameter) %>% 
  summarize(Median = median(estimate), lower = pull(hdi(estimate)[2]), 
    upper = pull(hdi(estimate)[3]), MPE = pd(estimate)[1], .groups = "drop") %>% 
  mutate_if(is.numeric, specify_decimal, k = 2) %>% 
  mutate(across(-c("Model", "Parameter"), printy::fmt_minus_sign)) %>% 
  mutate(HDI = glue("[{lower}, {upper}]")) %>% 
  select(Model, Parameter, Median, HDI, MPE) %>% 
  write_csv(here("tables", "dp_utterance_variety.csv"))

# -----------------------------------------------------------------------------
