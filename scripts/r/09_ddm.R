# DDM ------------------------------------------------------------------------
#
# Author: Joseph V. Casillas
# Last update: 20211225
#
# - This script does the following: 
#   - Fits a DDM model to each participant
#   - Fits a linear regression model to the boundary separation and 
#     drift rate data
#   - Runs simulations based on the DDM parameters
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

if(F) {

# Load rds files and get model summary estimates for boundary_separation 
# and drift_rate
ddm_estimates <- load_models(path = here("models", "ddm"), "rds") %>% 
  map(fixef) %>% 
  map_df(as_tibble, rownames = "param", .id = "participant") %>% 
  filter(participant != "test") %>% 
  select(participant, param, estimate = Estimate, se = Est.Error) %>% 
  separate(param, into = c("effect", "q_type"), sep = "_", extra = "merge") %>%
  filter(effect %in% c("bs", "ndt")) %>% 
  mutate(
    q_type = str_remove(q_type, "sentence_typeinterrogativeM"), 
    q_type = str_remove(q_type, "totalM|partialM"), 
    effect = if_else(effect == "bs", "boundary_separation", "drift_rate")
  ) %>% 
  left_join(., 
    select(learners_ddm, participant, lextale_std, eq_std) %>% distinct, 
    by = "participant"
  ) %>% 
  mutate(q_sum = if_else(q_type == "yn", 1, -1)) %>% 
  write_csv(here("data", "tidy", "ddm_estimates.csv"))

}

# -----------------------------------------------------------------------------




# Drift rate and boundary separation models -----------------------------------

#
# Measurement error model
#

# Model formula
ddm_mem_formula <- bf(
  estimate | se(se) ~ 1 + q_sum * lextale_std * eq_std + 
    (1 + q_sum * lextale_std * eq_std | participant)
  )

# Get priors of model
get_prior(ddm_mem_formula, 
  family = gaussian(), 
  data = ddm_estimates)

# Set weakly informative priors for drift rate
dr_priors <- c(
  prior(normal(1, 0.5), class = "Intercept"), 
  prior(normal(0, 0.3), class = "b"), 
  prior(cauchy(0, 0.3), class = "sd"), 
  prior(lkj(2), class = "cor")
)

# Set weakly informative priors for boundary separation
bs_priors <- c(
  prior(normal(2, 0.5), class = "Intercept"), 
  prior(normal(0, 0.5), class = "b"), 
  prior(cauchy(0, 0.3), class = "sd"), 
  prior(lkj(2), class = "cor")
)

# Fit measurement error model for drift rate
mem_drift_rate <- brm(
  formula = ddm_mem_formula, 
  prior = dr_priors, 
  family = gaussian(), 
  cores = 4, chains = 4, 
  control = list(max_treedepth = 15, adapt_delta = 0.99), 
  backend = "cmdstanr", 
  data = filter(ddm_estimates, effect == "drift_rate"), 
  file = here("models", "mem_drift_rate")
)

# Fit measurement error model for drift rate
mem_boundary_separation <- brm(
  formula = ddm_mem_formula, 
  prior = bs_priors, 
  family = gaussian(), 
  cores = 4, chains = 4, 
  control = list(max_treedepth = 15, adapt_delta = 0.99), 
  backend = "cmdstanr", 
  data = filter(ddm_estimates, effect == "boundary_separation"), 
  file = here("models", "mem_boundary_separation")
)

# -----------------------------------------------------------------------------




# Trial simulation ------------------------------------------------------------

#
# General structure: 
#

# YN                 WH
#   low emp low emp    low emp low emp
#   low lex hi lex     low lex hi lex
#   P1      P2         P5      P6
# YN
#   hi emp  hi emp     hi emp  hi emp
#   low lex hilex      low lex hilex
#   P3      P4         P7      P8

# Load models
mem_boundary_separation <- readRDS(here("models", "mem_boundary_separation.rds"))
mem_drift_rate <- readRDS(here("models", "mem_drift_rate.rds"))

# Create new data to predict
bs_new_dat <- expand_grid(
  q_sum = c(-1, 1), 
  lextale_std = c(-1, 1), 
  eq_std = c(-1, 1)) %>% 
  mutate(participant = "pop", se = 0.388)

dr_new_dat <- expand_grid(
  q_sum = c(-1, 1), 
  lextale_std = c(-1, 1), 
  eq_std = c(-1, 1)) %>% 
  mutate(participant = "pop", se = 0.089)

# Get predictions, store in list
dr_bs_est <- bind_rows(
  predict(mem_boundary_separation, newdata = new_dat, 
    re_formula = NA, allow_new_levels = T) %>% 
    as_tibble() %>% 
    bind_cols(new_dat, .) %>% 
    mutate(effect = "bs"), 
  predict(mem_drift_rate, newdata = new_dat, 
    re_formula = NA, allow_new_levels = T) %>% 
    as_tibble() %>% 
    bind_cols(new_dat, .) %>% 
    mutate(effect = "dr")
  ) %>% 
  transmute(val = Estimate, effect, 
    q_type = if_else(q_sum == 1, "yn", "wh"), 
    eq = if_else(eq_std == -1, "low", "high"), 
    lt = if_else(lextale_std == -1, "low", "high")) %>% 
  printy::super_split(q_type, eq, lt, effect)

# Example (drift rate estimate for y/n question, low eq, low lt)
# dr_bs_est$yn$low$low$dr

# Run 2000 simulations for each condition
ddm_sims <- bind_rows(
  sim_ddm(q_type = "yn", eq = "low", lt = "low",
    drift_rate = dr_bs_est$yn$low$low$dr$val, 
    boundary_separation = dr_bs_est$yn$low$low$bs$val / 2,
    bias = 0, ndt = 0.57, n_sims = 1000, seed = 20211225), 
  sim_ddm(q_type = "yn", eq = "low", lt = "high",
    drift_rate = dr_bs_est$yn$low$high$dr$val, 
    boundary_separation = dr_bs_est$yn$low$high$bs$val / 2,
    bias = 0, ndt = 0.57, n_sims = 1000, seed = 20211225), 
  sim_ddm(q_type = "yn", eq = "high", lt = "low",
    drift_rate = dr_bs_est$yn$high$low$dr$val, 
    boundary_separation = dr_bs_est$yn$high$low$bs$val / 2,
    bias = 0, ndt = 0.57, n_sims = 1000, seed = 20211225), 
  sim_ddm(q_type = "yn", eq = "high", lt = "high",
    drift_rate = dr_bs_est$yn$high$high$dr$val, 
    boundary_separation = dr_bs_est$yn$high$high$bs$val / 2,
    bias = 0, ndt = 0.57, n_sims = 1000, seed = 20211225), 

  sim_ddm(q_type = "wh", eq = "low", lt = "low",
    drift_rate = dr_bs_est$wh$low$low$dr$val, 
    boundary_separation = dr_bs_est$wh$low$low$bs$val / 2,
    bias = 0, ndt = 0.57, n_sims = 1000, seed = 20211225), 
  sim_ddm(q_type = "wh", eq = "low", lt = "high",
    drift_rate = dr_bs_est$wh$low$high$dr$val, 
    boundary_separation = dr_bs_est$wh$low$high$bs$val / 2,
    bias = 0, ndt = 0.57, n_sims = 1000, seed = 20211225), 
  sim_ddm(q_type = "wh", eq = "high", lt = "low",
    drift_rate = dr_bs_est$wh$high$low$dr$val, 
    boundary_separation = dr_bs_est$wh$high$low$bs$val / 2,
    bias = 0, ndt = 0.57, n_sims = 1000, seed = 20211225), 
  sim_ddm(q_type = "wh", eq = "high", lt = "high",
    drift_rate = dr_bs_est$wh$high$high$dr$val, 
    boundary_separation = dr_bs_est$wh$high$high$bs$val / 2,
    bias = 0, ndt = 0.57, n_sims = 1000, seed = 20211225)
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
    ) %>% 
  group_by(q_type, eq, lt, sim_n) %>% 
  mutate(final_step = max(step), 
    final_val = value[final_step], 
    response = if_else(final_val > 0, "correct", "incorrect")) %>% 
  write_csv(., here("data", "tidy", "ddm_sims.csv"))

# -----------------------------------------------------------------------------
