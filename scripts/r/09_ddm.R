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
learners_ddm <- filter(learners, q_type != 0)

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
 prior("normal(1.5, 1)", class = "b", dpar = "bs"),
 prior("normal(0.5, 0.5)", class = "b", dpar = "ndt"),
 prior("normal(0.5, 0.2)", class = "b", dpar = "bias")
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
  chains = 4, warmup = 1000, iter = 2000, 
  cores = 2, 
  control = list(max_treedepth = 15, step_size = 0.9, adapt_delta = 0.99), 
  file = here("models", "ddm", "test")
  )

# Fit model for all participants 
for (subj in unique(learners$participant)) {
  print(subj)
  
  participant_m <- brm(
    formula = Model_f,
    data = learners_ddm, 
    prior = prior,
    sample_prior = TRUE,
    inits = initfun,
    family = wiener(
      link_bs = "identity", 
      link_ndt = "identity",
      link_bias = "identity"),
    backend = "cmdstanr", 
    chains = 2, warmup = 1000, iter = 2000, 
    cores = 2, 
    control = list(max_treedepth = 15, step_size = 0.9, adapt_delta = 0.99), 
    file = here("models", "ddm", subj), 
  )

  participant_m <- add_criterion(
    participant_m, 
    criterion = c("loo", "bayes_R2"), 
    file = here("models", "ddm", subj))
}

# Function to read all rds files
read_models <- function(filename) {
  mods <- readRDS(filename)
  posteriors <- as_tibble(mods)
  return(posteriors)
}

# Load full posterior for drift rate and boundary separation
full_posterior <- 
  dir_ls(path = here("models", "ddm"), regexp = "\\.rds$") %>% 
  map_dfr(read_models, .id = "participant") %>% 
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
  
tidy_posterior <- full_posterior %>% 
  group_by(participant, effect, q_type) %>% 
  mutate(draw = seq_along(participant)) %>% 
  summarize(estimate = mean(estimate), .groups = "drop") %>% 
  pivot_wider(names_from = effect, values_from = estimate) %>% 
  left_join(., 
    select(learners_ddm, participant, lextale_std, eq_std) %>% distinct, 
    by = "participant"
  )

tidy_posterior %>% 
  ggplot(., aes(x = drift_rate, y = boundary_separation, color = q_type)) + 
    geom_point(alpha = 0.1)





# build models for drift rate (NDT) and boundary separation (bs)

if(F) {

dr_mod_formula <- bf(drift_rate ~ q_type + lextale_std + eq_std + 
    (1 + q_type | participant))

bs_mod_formula <- bf(boundary_separation ~ q_type + lextale_std + eq_std + 
    (1 + q_type | participant))

get_prior(dr_mod_formula, 
  family = gaussian(), 
  data = tidy_posterior)

dr_bs_priors <- c(
  prior(normal(0, 2), class = "Intercept"), 
  prior(normal(0, 1), class = "b"), 
  prior(cauchy(0, 1), class = "sd"), 
  prior(lkj(2), class = "cor")
)

dr_mod <- brm(
  formula = dr_mod_formula, 
  prior = dr_bs_priors, 
  cores = 4, chains = 4, 
  control = list(max_treedepth = 15, adapt_delta = 0.99), 
  backend = "cmdstanr", 
  data = tidy_posterior, 
  )

bs_mod <- brm(
  formula = bs_mod_formula, 
  prior = dr_bs_priors, 
  cores = 4, chains = 4, 
  control = list(max_treedepth = 15, adapt_delta = 0.99), 
  backend = "cmdstanr", 
  data = tidy_posterior, 
  )

}


 
 
