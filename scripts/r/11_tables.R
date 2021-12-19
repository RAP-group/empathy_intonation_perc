# Tables ----------------------------------------------------------------------
#
#
#
#
#
#
# -----------------------------------------------------------------------------




# Source helpers, libs, and models --------------------------------------------

source(here::here("scripts", "r", "07_load_data.R"))

learner_response_01 <- readRDS(here("models", "learner_response_01.rds"))
learner_response_q_01 <- readRDS(here("models", "learner_response_q_01.rds"))
learner_response_qonly_01 <- readRDS(here("models", "learner_response_qonly_01.rds"))
learner_response_yn_01 <- readRDS(here("models", "learner_response_yn_01.rds"))
ddm_boundary_separation <- readRDS(here("models", "ddm_boundary_separation.rds"))
ddm_drift_rate <- readRDS(here("models", "ddm_drift_rate.rds"))

# -----------------------------------------------------------------------------




# Learner response accuracy ---------------------------------------------------

describe_posterior(learner_response_01, rope_range = c(-0.1, 0.1)) %>% 
  as_tibble() %>% 
  select(-c("CI", "ROPE_CI", "ROPE_low", "ROPE_high", "Rhat")) %>% 
  mutate(Parameter = case_when(
    Parameter == "b_Intercept" ~ "Intercept", 
    Parameter == "b_sentence_typeinterrogativeMpartialMwh" ~ "Int. wh-", 
    Parameter == "b_sentence_typedeclarativeMnarrowMfocus" ~ "Dec. narrow focus", 
    Parameter == "b_sentence_typedeclarativeMbroadMfocus" ~ "Dec. broad focus", 
    Parameter == "b_lextale_std" ~ "LexTALE", 
    Parameter == "b_eq_std" ~ "Empathy quotient", 
    Parameter == "b_sentence_typeinterrogativeMpartialMwh:lextale_std" ~ "Int. wh-:LexTALE", 
    Parameter == "b_sentence_typedeclarativeMnarrowMfocus:lextale_std" ~ "Dec. narrow focus:LexTALE", 
    Parameter == "b_sentence_typedeclarativeMbroadMfocus:lextale_std" ~ "Dec. broad focus:LexTALE", 
    Parameter == "b_sentence_typeinterrogativeMpartialMwh:eq_std" ~ "Int. wh-:Empathy quotient", 
    Parameter == "b_sentence_typedeclarativeMnarrowMfocus:eq_std" ~ "Dec. narrow focus:Empathy quotient", 
    Parameter == "b_sentence_typedeclarativeMbroadMfocus:eq_std" ~ "Dec. broad focus:Empathy quotient", 
    Parameter == "b_lextale_std:eq_std" ~ "LexTALE:Empathy quotient", 
    Parameter == "b_sentence_typeinterrogativeMpartialMwh:lextale_std:eq_std" ~ "Int. wh-:LexTALE:Empathy quotient", 
    Parameter == "b_sentence_typedeclarativeMnarrowMfocus:lextale_std:eq_std" ~ "Dec. narrow focus:LexTALE:Empathy quotient", 
    Parameter == "b_sentence_typedeclarativeMbroadMfocus:lextale_std:eq_std" ~ "Dec. broad focus:LexTALE:Empathy quotient"
  )) %>% 
  mutate_if(is.numeric, specify_decimal, k = 2) %>% 
  mutate(across(-Parameter, printy::fmt_minus_sign)) %>% 
  mutate(HDI = glue("[{CI_low}, {CI_high}]")) %>% 
  select(Parameter, Median, HDI, `% in ROPE` = ROPE_Percentage, MPE = pd, ESS) %>% 
  write_csv(here("tables", "learner_response_01.csv"))

# -----------------------------------------------------------------------------
