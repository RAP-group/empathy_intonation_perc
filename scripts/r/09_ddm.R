# DDM ------------------------------------------------------------------------
#
# This script sources libraries and helpers and loads all tidy data
#
# -----------------------------------------------------------------------------



# Source libraries, helpers, load data ----------------------------------------

source(here::here("scripts", "r", "07_load_data.R"))

# -----------------------------------------------------------------------------

library("RWiener")

# we need a function for estimating parameter
estPar <- function(df){
  est = optim(c(1, 0.1, 0.1, 0.1), wiener_deviance, dat = data.frame(df))
  data.frame(a = est$par[1], tau =est$par[2], beta = est$par[3], delta = est$par[4]  ) # return parameter
}

# subsetting data, estimate parameter, then combine all parameters together
learners %>% 
  filter(rt_adj < 10, sentence_type == "declarative-narrow-focus") %>% 
  mutate(resp = if_else(is_correct == 1, 'upper','lower'), q = rt_adj) %>%
  select(participant, q, resp) %>%
  group_by(participant) %>%
  do(data.frame(
      value = optim(c(1, .1, .1, 1), 
      wiener_deviance, 
      dat = data.frame(select(., q, resp)),
      method = "Nelder-Mead")$par, 
      param = c("a","t","b","v")
    )
  )

learners %>% 
  filter(rt_adj < 10, sentence_type == "declarative-narrow-focus") %>% 
  mutate(resp = if_else(is_correct == 1, 'upper','lower'), q = rt_adj) %>%
  select(participant, q, resp, sentence_type) %>%
  group_by(participant, sentence_type) %>% 
  nest() %>%
  mutate(par = data %>% map(estPar)) %>% 
  select(participant, sentence_type, par) %>%
  unnest() 



library(DMCfun)

test_dat <- learners %>% 
  filter(rt_adj < 10) %>% 
  mutate(Subject = as.numeric(as.factor(participant)), RT = rt_adj, 
    Error = if_else(is_correct == 1, 0, 1), 
    Comp = if_else(question_statement == "question", "comp", "incomp"), 
    Comp = as.factor(Comp)) %>% 
  select(Subject, Comp, RT, Error) 


dmcObservedData(test_dat)
fit <- dmcFit(test_dat) # flanker data from Ulrich et al. (2015)
plot(fit, flankerData)





