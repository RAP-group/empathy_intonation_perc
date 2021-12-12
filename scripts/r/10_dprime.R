# Dprime Models ---------------------------------------------------------------
#
# This script sources libraries and helpers and loads all tidy data
#
# -----------------------------------------------------------------------------



# Source libraries, helpers, load data ----------------------------------------

source(here::here("scripts", "r", "07_load_data.R"))

# -----------------------------------------------------------------------------



# Calculate dprime ------------------------------------------------------------

learners_dp <- learners %>% 
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
  ) %>% 
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

# -----------------------------------------------------------------------------




# Models ----------------------------------------------------------------------



learner_dp_01 <- brm(
  formula = dprime ~ 0 + Intercept + speaker_variety + lextale_std + 
    (1 + speaker_variety | participant), 
  family = "gaussian", 
  data = learners_dp, 
  prior = c(
    prior(normal(2, 1.5), class = b, coef = "Intercept"),
    prior(normal(0, 1.5), class = b),
    prior(cauchy(0, 1), class = sd), 
    prior(lkj(2), class = cor)), 
  warmup = 2000, iter = 4000, chains = 4, cores = 4, 
  backend = "cmdstanr", 
  file = here("models", "learner_dp_01")
)

# -----------------------------------------------------------------------------





# Exploratory plots -----------------------------------------------------------

conditional_effects(learner_dp_01)

learners_dp %>% 
  ggplot(., aes(x = eq_std, y = dprime, color = speaker_variety)) + 
    facet_grid(. ~ speaker_variety) + 
    geom_point() + 
    geom_smooth(method = lm, formula = "y ~ x")

learners %>% 
  group_by(participant, spn_variety) %>% 
  summarize(.groups = "drop") %>% 
  group_by(spn_variety) %>% 
  summarize(n = n(), .groups = "drop") %>% 
  arrange(desc(n)) %>% 
  slice(1:4) %>% 
  ggplot(., aes(x = n, y = spn_variety)) + 
    geom_segment(aes(x = 0, xend = n, yend = spn_variety)) + 
    geom_point()

learners_dp %>% 
  mutate(familiarity = case_when(
    speaker_variety == "castilian" & spn_variety == "Spain" ~ "familiar", 
    speaker_variety == "mexican" & spn_variety == "Mexico" ~ "familiar", 
    speaker_variety == "castilian" & spn_variety != "Spain" ~ "unfamiliar", 
    speaker_variety == "mexican" & spn_variety != "Mexico" ~ "unfamiliar", 
    TRUE ~ "nnd"
  )) %>% 
  filter(familiarity != "nnd") %>% 
  ggplot(., aes(x = familiarity, y = dprime, color = familiarity)) + 
    geom_beeswarm(alpha = 0.3) + 
    stat_summary(fun.data = mean_sdl, geom = "pointrange", fun.args = list(mult = 1))
