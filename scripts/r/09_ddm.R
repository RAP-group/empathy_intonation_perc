# DDM ------------------------------------------------------------------------
#
# This script sources libraries and helpers and loads all tidy data
#
# -----------------------------------------------------------------------------



# Source libraries, helpers, load data ----------------------------------------

source(here::here("scripts", "r", "07_load_data.R"))

# -----------------------------------------------------------------------------



# DDM -------------------------------------------------------------------------

# Get subset of questions
learners_ddm <- filter(learners, q_type != 0, rt_raw < 10)

# Formula of the model
Model_f <- bf(
  rt_raw | dec(is_correct) ~ 0 + sentence_type,
  bs ~ 0 + sentence_type, 
  ndt ~ 0 + sentence_type, 
  bias ~ 0 + sentence_type
  ) 

# Take a look at priors
get_prior(Model_f, 
  family = wiener(
    link_bs = "identity", 
    link_ndt = "identity", 
    link_bias = "identity"), 
  data = learners_ddm
  )

# Set weakly informative priors
prior <- c(
  prior("normal(0, 1)", class = "b"),
  prior("normal(0, 5)", class = "b", dpar = "bs"),
  prior("normal(0.2, 1)", class = "b", dpar = "ndt"),
  prior("normal(0.5, 1)", class = "b", dpar = "bias")
)

# Print stan code to check specification
make_stancode(
  Model_f, 
  family = wiener(
    link_bs = "identity", 
    link_ndt = "identity", 
    link_bias = "identity"),
  data = learners_ddm, 
  prior = prior)

# Generate temp data for possible init values
tmp_dat <- make_standata(
  formula = Model_f, 
  family = wiener(
    link_bs = "identity", 
    link_ndt = "identity", 
    link_bias = "identity"), 
  data = learners_ddm, 
  prior = prior)

str(tmp_dat, 1, give.attr = FALSE)

# Create init function to be used when fitting model
initfun <- function() {
  list(
    b = rnorm(tmp_dat$K),
    b_bs = runif(tmp_dat$K_bs, 1, 2), #1, 2
    b_ndt = runif(tmp_dat$K_ndt, 0.1, 0.15), 
    b_bias = rnorm(tmp_dat$K_bias, 0.5, 0.1) 
  )
 }

# Test model on single participant
test <- brm(
  formula = Model_f,
  data = filter(learners_ddm, participant == "558aab19fdf99b65685f0142"),
  prior = prior,
  sample_prior = TRUE,
  inits = initfun,
  family = wiener(
    link_bs = "identity", 
    link_ndt = "identity",
    link_bias = "identity"),
  backend = "cmdstanr", 
  chains = 4, warmup = 10000, iter = 15000, thin = 10, 
  cores = 4, 
  control = list(adapt_delta = 0.99999999), 
  file = here("models", "ddm", "test")
  )

readRDS(here("models", "ddm", "test.rds"))

# Fit model for all participants 
for (subj in unique(learners$participant)) {
  print(subj)
  
  participant_m <- brm(
    formula = Model_f,
    data = filter(learners_ddm, participant == subj), 
    prior = prior,
    sample_prior = TRUE,
    inits = initfun,
    family = wiener(
      link_bs = "identity", 
      link_ndt = "identity",
      link_bias = "identity"),
    backend = "cmdstanr", 
    chains = 4, warmup = 10000, iter = 15000, thin = 10, cores = 4, 
    control = list(adapt_delta = 0.99999999), 
    file = here("models", "ddm", subj), 
  )

  participant_m <- add_criterion(
    participant_m, 
    criterion = c("loo", "bayes_R2"), 
    file = here("models", "ddm", subj))
}







# Function to read all rds files
read_models <- function(filename, n_draws = 100) {
  mods <- readRDS(filename)
  posteriors <- as_draws_df(mods) %>% sample_n(n_draws)
  return(posteriors)
}

# Load full posterior for drift rate and boundary separation
full_posterior <- 
  dir_ls(path = here("models", "ddm"), regexp = "\\.rds$") %>% 
  map_dfr(read_models, n_draws = 5, .id = "participant") %>% 
  filter(participant != here("models", "ddm", "test.rds")) %>% 
  select(participant, starts_with(c("b_bs_", "b_ndt"))) %>% 
  pivot_longer(-participant, names_to = "param", values_to = "estimate") %>% 
  mutate(
    participant = str_remove(participant, here("models", "ddm/")), 
    participant = str_remove(participant, ".rds"), 
    param = str_remove(param, "b_")
  ) %>% 
  separate(param, into = c("effect", "q_type"), sep = "_", extra = "merge") %>% 
  mutate(
    q_type = str_remove(q_type, "sentence_typeinterrogativeM"), 
    q_type = str_remove(q_type, "totalM|partialM"), 
    effect = if_else(effect == "bs", "boundary_separation", "drift_rate")
  )

long_posterior <- full_posterior %>% 
  group_by(participant, effect, q_type) %>% 
  mutate(draw = seq_along(participant)) %>% 
  left_join(., 
    select(learners_ddm, participant, lextale_std, eq_std) %>% distinct, 
    by = "participant"
  )

wide_posterior <- full_posterior %>% 
  group_by(participant, effect, q_type) %>% 
  mutate(draw = seq_along(participant)) %>% 
  pivot_wider(names_from = q_type, values_from = estimate) %>% 
  mutate(diff = yn - wh) %>% 
  left_join(., 
    select(learners_ddm, participant, lextale_std, eq_std) %>% distinct, 
    by = "participant"
  )

wide_posterior_summary <- full_posterior %>% 
  group_by(participant, effect, q_type) %>% 
  mutate(draw = seq_along(participant)) %>% 
  summarize(estimate = mean(estimate), .groups = "drop") %>% 
  pivot_wider(names_from = effect, values_from = estimate) %>% 
  left_join(., 
    select(learners_ddm, participant, lextale_std, eq_std) %>% distinct, 
    by = "participant"
  )

#
# Estimates of boundary sep. and drift rate for each question type
#

long_posterior %>% 
  ggplot(., aes(x = lextale_std, y = estimate, fill = q_type)) + 
    facet_wrap(~ effect, scales = "free_y") + 
    stat_pointinterval(pch = 21, color = "black") + 
    scale_fill_viridis_d()

long_posterior %>% 
  ggplot(., aes(x = eq_std, y = estimate, fill = q_type)) + 
    facet_wrap(~ effect, scales = "free_y") + 
    stat_pointinterval(pch = 21, color = "black") + 
    scale_fill_viridis_d()

#
# Wh - y/n diff. estimates for each question type
#

wide_posterior %>% 
  ggplot(., aes(x = lextale_std, y = diff)) + 
    facet_wrap(~ effect, scales = "free_y") + 
    stat_pointinterval(pch = 21, fill = "white")  

wide_posterior %>% 
  ggplot(., aes(x = eq_std, y = diff)) + 
    facet_wrap(~ effect, scales = "free_y") + 
    stat_pointinterval(pch = 21, fill = "white")  

#
# Point estimates for boundary sep. and drift rate for each question type
#

wide_posterior_summary %>% 
  ggplot(., aes(x = lextale_std, y = boundary_separation, color = q_type)) + 
    geom_point()

wide_posterior_summary %>% 
  ggplot(., aes(x = eq_std, y = boundary_separation, color = q_type)) + 
    geom_point()

wide_posterior_summary %>% 
  ggplot(., aes(x = lextale_std, y = drift_rate, color = q_type)) + 
    geom_point()

wide_posterior_summary %>% 
  ggplot(., aes(x = eq_std, y = drift_rate, color = q_type)) + 
    geom_point()



# Drift rate and boundary separation models -----------------------------------

# Model formula
mod_formula <- bf(estimate ~ 0 + q_type:lextale_std:eq_std + 
    (0 + q_type:lextale_std:eq_std | participant))

# Get priors of model
get_prior(mod_formula, 
  family = gaussian(), 
  data = long_posterior)

# Set weakly informative priors
dr_bs_priors <- c(
  prior(normal(0, 0.1), class = "b"), 
  prior(cauchy(0, 0.1), class = "sd"), 
  prior(lkj(2), class = "cor")
)

# Fit drift rate model
dr_mod <- brm(
  formula = mod_formula, 
  prior = dr_bs_priors, 
  cores = 2, chains = 2, 
  control = list(max_treedepth = 15, adapt_delta = 0.99), 
  backend = "cmdstanr", 
  data = filter(long_posterior, effect == "drift_rate"), 
  file = here("models", "ddm_dr_mod")
  )

# Fit boundary separation model
bs_mod <- brm(
  formula = mod_formula, 
  prior = dr_bs_priors, 
  cores = 4, chains = 4, 
  control = list(max_treedepth = 15, adapt_delta = 0.99), 
  backend = "cmdstanr", 
  data = filter(long_posterior, effect == "boundary_separation"), 
  file = here("models", "ddm_bs_mod")
  )

# -----------------------------------------------------------------------------




# Trial simulation ------------------------------------------------------------
# get estimate for BS and drift rate for each q_type for -2, 2 eq and lex


# YN                 WH
#   low emp low emp    low emp low emp
#   low lex hi lex     low lex hi lex
#   P1      P2         P5      P6
# YN
#   hi emp  hi emp     hi emp  hi emp
#   low lex hilex      low lex hilex
#   P3      P4         P7      P8
 

## y/n low empathy low lextale
bias              <- 0 
dr_yn_lo_eq_lo_lt <- -0.2 # value from ddm_dr_mod (use predict?)
bs_yn_lo_eq_lo_lt <- 2.36 / 2 # value from ddm_bs_mod
ndt               <- 0.57 # Not sure where this comes from

simd = NULL
for (sim in 1:20){
  Step = 1
  Value = bias
  n = 1
  while (abs(Value[n]) < bs_yn_lo_eq_lo_lt) {
    n = n + 1
    Value[n] = (Value[n - 1] + rnorm(1, 0, abs(dr_yn_lo_eq_lo_lt)))
    Step[n] = n
  }
  dd = data.frame(Sim = sim, Step, Value)
  if (exists("simd")) {
    simd = rbind(simd, dd) 
  } else {
    simd = dd
  } 
}

sim_ddm <- function(q_type, eq, lt, drift_rate, boundary_separation, bias, ndt, n_sims) {
  simd = NULL
  for (sim in 1:n_sims){
    step = 1
    value = bias
    n = 1
    while (abs(value[n]) < boundary_separation) {
      n = n + 1
      value[n] = (value[n - 1] + rnorm(1, 0, abs(drift_rate)))
      step[n] = n
    }
    dd <- tibble(q_type = q_type, eq = eq, lt = lt, sim_n = sim, step, value) %>% 
    mutate(
      sim_n = as.factor(sim_n), 
      value = case_when(
        value > boundary_separation ~ boundary_separation, 
        value < -boundary_separation ~ -boundary_separation, 
        TRUE ~ .$value)
      )
    simd = rbind(simd, dd)
  }
  return(simd)
}

ddm_sims <- bind_rows(
  sim_ddm(q_type = "yn", eq = "low", lt = "low",
    drift_rate = -0.2, boundary_separation = 2.36/2,
    bias = 0, ndt = 0.57, n_sims = 20), 
  sim_ddm(q_type = "yn", eq = "low", lt = "high",
    drift_rate = -0.2, boundary_separation = 2.36/2,
    bias = 0, ndt = 0.57, n_sims = 20), 
  sim_ddm(q_type = "yn", eq = "high", lt = "low",
    drift_rate = -0.2, boundary_separation = 2.36/2,
    bias = 0, ndt = 0.57, n_sims = 20), 
  sim_ddm(q_type = "yn", eq = "high", lt = "high",
    drift_rate = -0.2, boundary_separation = 2.36/2,
    bias = 0, ndt = 0.57, n_sims = 20), 

  sim_ddm(q_type = "wh", eq = "low", lt = "low",
    drift_rate = -0.2, boundary_separation = 2.36/2,
    bias = 0, ndt = 0.57, n_sims = 20), 
  sim_ddm(q_type = "wh", eq = "low", lt = "high",
    drift_rate = -0.2, boundary_separation = 2.36/2,
    bias = 0, ndt = 0.57, n_sims = 20), 
  sim_ddm(q_type = "wh", eq = "high", lt = "low",
    drift_rate = -0.2, boundary_separation = 2.36/2,
    bias = 0, ndt = 0.57, n_sims = 20), 
  sim_ddm(q_type = "wh", eq = "high", lt = "high",
    drift_rate = -0.2, boundary_separation = 2.36/2,
    bias = 0, ndt = 0.57, n_sims = 20)
  ) %>% 
  mutate(
    facet_lab = case_when(
      q_type == "yn" & eq == "low"  & lt == "low"  ~ "EQ: Low, LexTALE: low", 
      q_type == "yn" & eq == "low"  & lt == "high" ~ "EQ: low, LexTALE: high", 
      q_type == "yn" & eq == "high" & lt == "low"  ~ "EQ: high, LexTALE: low", 
      q_type == "yn" & eq == "high" & lt == "high" ~ "EQ: high, LexTALE: high", 
      q_type == "wh" & eq == "low"  & lt == "low"  ~ "EQ: Low, LexTALE: low", 
      q_type == "wh" & eq == "low"  & lt == "high" ~ "EQ: low, LexTALE: high", 
      q_type == "wh" & eq == "high" & lt == "low"  ~ "EQ: high, LexTALE: low", 
      q_type == "wh" & eq == "high" & lt == "high" ~ "EQ: high, LexTALE: high"
    ), 
    facet_lab = fct_relevel(facet_lab, 
      "EQ: Low, LexTALE: low", "EQ: low, LexTALE: high", 
      "EQ: high, LexTALE: low", "EQ: high, LexTALE: high")
    )

lside <- ddm_sims %>% 
  filter(q_type == "yn") %>% 
  ggplot(., aes(x = step, y = value, color = sim_n)) + 
    facet_wrap(~ facet_lab) + 
    scale_y_continuous(breaks = seq(-1, 1, 1)) + 
    geom_line(show.legend = F) + 
    geom_hline(yintercept = 0, lty = 3) + 
    scale_color_viridis_d(option = "C") + 
    labs(y = "Boundary separation", x = "Time step") + 
    theme_minimal()

rside <- ddm_sims %>% 
  filter(q_type == "wh") %>% 
  ggplot(., aes(x = step, y = value, color = sim_n)) + 
    facet_wrap(~ facet_lab) + 
    scale_y_continuous(position = "right", breaks = seq(-1, 1, 1)) + 
    geom_line(show.legend = F) + 
    geom_hline(yintercept = 0, lty = 3) + 
    scale_color_viridis_d(option = "B") + 
    labs(y = "Boundary separation", x = "Time step") + 
    theme_minimal()

lside + rside

