# DDM ------------------------------------------------------------------------
#
# This script sources libraries and helpers and loads all tidy data
#
# -----------------------------------------------------------------------------



# Source libraries, helpers, load data ----------------------------------------

source(here::here("scripts", "r", "07_load_data.R"))

# -----------------------------------------------------------------------------


glimpse(learners)

#
# The formula
#

# Formula of ddm removing everything that is between subjects and the subject 
# random effects
Model_f <- bf(
  rt_raw | dec(is_correct) ~ 0 + q_type,
  bs ~ 0 + q_type,
  bias = 0.5) 

get_prior(Model_f, 
  family = wiener(
    link_bs = "identity", 
    link_ndt = "identity", 
    link_bias = "identity"), 
  data = filter(learners, q_type != 0)
  )

prior <- c(
  prior(normal(0, 0.5), class = b),
  prior(normal(1.5, 0.5), class = b, dpar = bs)
 )

make_stancode(
  Model_f, 
  family = wiener(
    link_bs = "identity", 
    link_ndt = "identity", 
    link_bias = "identity"),
  data = filter(learners, q_type != 0), 
  prior = prior)

tmp_dat <- make_standata(
  Model_f, 
  family = wiener(
    link_bs = "identity", 
    link_ndt = "identity", 
    link_bias = "identity"), 
  data = filter(learners, q_type != 0), 
  prior = prior)


str(tmp_dat, 1, give.attr = FALSE)

 
initfun <- function() {
   list(
     b = rnorm(tmp_dat$K),
     b_bs = runif(tmp_dat$K_bs, 1, 2)#, #1, 2
     #temp_ndt_Intercept = runif(tmp_dat$K_ndt, 0.1, 0.15) # 0.1, 0.15
   )
 }

 
for (subj in unique(learners$participant)) {
  print(subj)
  
  participant_m = brm(
    Model_f,
    filter(learners, participant == subj),
    prior = prior,
    sample_prior = TRUE,
    #inits = initfun,
    family = wiener(
      link_bs = "identity", 
      link_ndt = "identity",
      link_bias = "identity"),
    file = here("models", "ddm", paste0("ddm_", subj)), 
    chains = 2, cores = 2, iter = 2000, 
    #backend = "cmdstanr", 
    control = list(max_treedepth = 20, adapt_delta = 0.99))

  participant_m <- add_criterion(
    participant_m, 
    criterion = c("loo", "bayes_R2"), 
    file = paste0("ddm_", subj))
}


# this function extracts posterior samples the individual models
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

read_models <- function(filename) {
  # read data
  d <- readRDS(filename)
  # parse filename; study, diagnosis, subject, trial
  vars = str_match(filename, "(D|N)_(\\d+)_(\\d+)(\\d+)(\\d*)")
  print(vars)
  vars = as.data.frame(t(vars[1:nrow(vars), 1:2]))
  print(vars)
  names(vars) = c("SubjectID", "Language")
  vars$Experiment = "Exp2"
  post <- posterior_samples(d)
  d <- cbind(vars, post)
  # combine all these data
  return(d)
}

CatPerc_data2 <- list.files(pattern = "*.rds") %>%
  purrr::map_df(read_models)

CatPerc1 <- CatPerc_data1[, 1:92]
CatPerc2 <- CatPerc_data2[, 1:92]
CatPerc <- rbind(CatPerc1, CatPerc2)

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
