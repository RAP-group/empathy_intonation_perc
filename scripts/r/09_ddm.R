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


# Extracts posterior samples from individual models
read_models <- function(filename) {
  # read data
  d <- readRDS(filename)
  # parse filename; study, diagnosis, subject, trial
  vars = str_match(filename, "(D|N)_(\\d+)_(\\d+)(\\d+)(\\d*)")
  print(vars)
  vars = as.data.frame(t(vars[1:nrow(vars), 1:2]))
  print(vars)
  names(vars) = c("SubjectID", "Language")
  vars$Experiment = "Exp1"
  post <- posterior_samples(d)
  d <- cbind(vars, post)
  # combine all these data
  return(d)
}

CatPerc_data1 <- list.files(pattern = "*.rds") %>%
  purrr::map_df(read_models)


CatPerc1 <- CatPerc_data1[, 1:92]

CatPerc <- CatPerc %>% mutate(
  drift_tNEARmid = `b_Biastaendt:DistanceNEAR` + 
    `simo_Biastaendt:DistanceNEAR:moStep1[1]` + 
    `simo_Biastaendt:DistanceNEAR:moStep1[2]` + 
    `simo_Biastaendt:DistanceNEAR:moStep1[3]` + 
    `simo_Biastaendt:DistanceNEAR:moStep1[4]`, 
  drift_sNEARmid = `b_Biassendt:DistanceNEAR` + 
    `simo_Biassendt:DistanceNEAR:moStep1[1]` + 
    `simo_Biassendt:DistanceNEAR:moStep1[2]` + 
    `simo_Biassendt:DistanceNEAR:moStep1[3]` + 
    `simo_Biassendt:DistanceNEAR:moStep1[4]`,
  drift_tFARmid = `b_Biastaendt:DistanceFAR` + 
    `simo_Biastaendt:DistanceFAR:moStep1[1]` + 
    `simo_Biastaendt:DistanceFAR:moStep1[2]` + 
    `simo_Biastaendt:DistanceFAR:moStep1[3]` + 
    `simo_Biastaendt:DistanceFAR:moStep1[4]`,
  drift_sFARmid = `b_Biassendt:DistanceFAR` + 
    `simo_Biassendt:DistanceFAR:moStep1[1]` + 
    `simo_Biassendt:DistanceFAR:moStep1[2]` + 
    `simo_Biassendt:DistanceFAR:moStep1[3]` + 
    `simo_Biassendt:DistanceFAR:moStep1[4]`, 
  bs_tNEARmid = `b_bs_Biastaendt:DistanceNEAR` + 
    `simo_bs_Biastaendt:DistanceNEAR:moStep1[1]` + 
    `simo_bs_Biastaendt:DistanceNEAR:moStep1[2]` + 
    `simo_bs_Biastaendt:DistanceNEAR:moStep1[3]` + 
    `simo_bs_Biastaendt:DistanceNEAR:moStep1[4]`,
  bs_sNEARmid = `b_bs_Biassendt:DistanceNEAR` + 
    `simo_bs_Biassendt:DistanceNEAR:moStep1[1]` + 
    `simo_bs_Biassendt:DistanceNEAR:moStep1[2]` + 
    `simo_bs_Biassendt:DistanceNEAR:moStep1[3]` + 
    `simo_bs_Biassendt:DistanceNEAR:moStep1[4]`,
  bs_tFARmid = `b_bs_Biastaendt:DistanceFAR` + 
    `simo_bs_Biastaendt:DistanceFAR:moStep1[1]` + 
    `simo_bs_Biastaendt:DistanceFAR:moStep1[2]` + 
    `simo_bs_Biastaendt:DistanceFAR:moStep1[3]` + 
    `simo_bs_Biastaendt:DistanceFAR:moStep1[4]`,
  bs_sFARmid = `b_bs_Biassendt:DistanceFAR` + 
    `simo_bs_Biassendt:DistanceFAR:moStep1[1]` + 
    `simo_bs_Biassendt:DistanceFAR:moStep1[2]` + 
    `simo_bs_Biassendt:DistanceFAR:moStep1[3]` + 
    `simo_Biassendt:DistanceFAR:moStep1[4]`
)
 
 
# from wide to long
CatPerc_short = CatPerc[, c(1:4, 93:100)]
CatPerc_long = reshape(
  CatPerc_short, 
  direction="long", 
  varying = list(
    c("drift_tNEARmid", "drift_sNEARmid", "drift_tFARmid", "drift_sFARmid"), 
    c("bs_tNEARmid", "bs_sNEARmid", "bs_tFARmid","bs_sFARmid")), 
  times = c(
    "drift_tNEARmid", "drift_sNEARmid", "drift_tFARmid", "drift_sFARmid"
    ), 
  v.names=c("drift_rate","bs"))

# Rename the columns and some values for our purposes.
names(CatPerc_long)[names(CatPerc_long) == "time"] <- "Bias"
CatPerc_long$Distance = CatPerc_long$Bias
CatPerc_long$Bias = gsub("drift_s(FAR|NEAR)mid", "sendt", CatPerc_long$Bias)
CatPerc_long$Bias = gsub("drift_t(FAR|NEAR)mid", "taendt", CatPerc_long$Bias)
CatPerc_long$Distance = gsub("drift_(s|t)FARmid", "FAR", CatPerc_long$Distance)
CatPerc_long$Distance = gsub(
  "drift_(s|t)NEARmid", "NEAR", CatPerc_long$Distance
  )

# saves the dataset for later use
write_csv(CatPerc_long, file = "CatPerc_DDM_byparticipant_postsamples.csv")
