# Tables ----------------------------------------------------------------------
#
# Author: Joseph V. Casillas
# Last update: 20211225
#
# - This script loads all models and generates summary tables
# - Tables are save as CSV files (tables/) and can be referenced
#   by row using report_posterior
#
# -----------------------------------------------------------------------------




# Source helpers, libs, and models --------------------------------------------

source(here::here("scripts", "r", "07_load_data.R"))

learner_response_01 <- readRDS(here("models", "learner_response_01.rds"))
learner_response_qonly_01 <- readRDS(here("models", "learner_response_qonly_01.rds"))
mem_boundary_separation <- readRDS(here("models", "mem_boundary_separation.rds"))
mem_drift_rate <- readRDS(here("models", "mem_drift_rate.rds"))
learner_dp_01 <- readRDS(here("models", "learner_dp_01.rds"))

# -----------------------------------------------------------------------------




# Learner response accuracy ---------------------------------------------------

describe_posterior(learner_response_01, rope_range = c(-0.1, 0.1)) %>% 
  as_tibble() %>% 
  select(-c("CI", "ROPE_CI", "ROPE_low", "ROPE_high")) %>% 
  mutate(Parameter = case_when(
    Parameter == "b_Intercept" ~ "Intercept", 
    Parameter == "b_sentence_typeinterrogativeMpartialMwh" ~ "Wh- question", 
    Parameter == "b_sentence_typedeclarativeMnarrowMfocus" ~ "Narrow focus", 
    Parameter == "b_sentence_typedeclarativeMbroadMfocus" ~ "Broad focus", 
    Parameter == "b_lextale_std" ~ "LexTALE", 
    Parameter == "b_eq_std" ~ "Empathy quotient", 
    Parameter == "b_sentence_typeinterrogativeMpartialMwh:lextale_std" ~ "Wh- question:LexTALE", 
    Parameter == "b_sentence_typedeclarativeMnarrowMfocus:lextale_std" ~ "Narrow focus:LexTALE", 
    Parameter == "b_sentence_typedeclarativeMbroadMfocus:lextale_std" ~ "Broad focus:LexTALE", 
    Parameter == "b_sentence_typeinterrogativeMpartialMwh:eq_std" ~ "Wh- question:EQ", 
    Parameter == "b_sentence_typedeclarativeMnarrowMfocus:eq_std" ~ "Narrow focus:EQ", 
    Parameter == "b_sentence_typedeclarativeMbroadMfocus:eq_std" ~ "Broad focus:EQ", 
    Parameter == "b_lextale_std:eq_std" ~ "LexTALE:EQ", 
    Parameter == "b_sentence_typeinterrogativeMpartialMwh:lextale_std:eq_std" ~ "Wh- question:LexTALE:EQ", 
    Parameter == "b_sentence_typedeclarativeMnarrowMfocus:lextale_std:eq_std" ~ "Narrow focus:LexTALE:EQ", 
    Parameter == "b_sentence_typedeclarativeMbroadMfocus:lextale_std:eq_std" ~ "Broad focus:LexTALE:EQ"
  )) %>% 
  mutate(across(-c("Parameter", "ESS"), specify_decimal, k = 2)) %>% 
  mutate(ESS = round(ESS)) %>% 
  mutate(across(-Parameter, printy::fmt_minus_sign)) %>% 
  mutate(HDI = glue("[{CI_low}, {CI_high}]")) %>% 
  select(Parameter, Median, HDI, `% in ROPE` = ROPE_Percentage, MPE = pd, Rhat, ESS) %>% 
  write_csv(here("tables", "learner_response_01.csv"))

# -----------------------------------------------------------------------------




# Boundary separation and drift rate ------------------------------------------

bind_rows(
  describe_posterior(mem_boundary_separation, test = c("p_direction")) %>% 
    as_tibble() %>% 
    select(-CI) %>% 
    mutate(Model = "Boundary separation"), 
  describe_posterior(mem_drift_rate, test = c("p_direction")) %>% 
    as_tibble() %>% 
    select(-CI) %>% 
    mutate(Model = "Drift rate")
  ) %>% 
  mutate(Parameter = case_when(
    Parameter == "b_Intercept" ~ "Intercept", 
    Parameter == "b_q_sum" ~ "Question type", 
    Parameter == "b_lextale_std" ~ "LexTALE", 
    Parameter == "b_eq_std" ~ "EQ", 
    Parameter == "b_q_sum:lextale_std" ~ "Question type:LexTALE", 
    Parameter == "b_q_sum:eq_std" ~ "Question type:EQ", 
    Parameter == "b_lextale_std:eq_std" ~ "LexTALE:EQ", 
    TRUE ~ "Question type:LexTALE:EQ"
  )) %>% 
  mutate(across(-c("Parameter", "ESS", "Model"), specify_decimal, k = 2)) %>% 
  mutate(ESS = round(ESS)) %>% 
  mutate(across(-c("Parameter", "ESS", "Model"), printy::fmt_minus_sign)) %>% 
  mutate(HDI = glue("[{CI_low}, {CI_high}]")) %>% 
  select(Model, Parameter, Median, HDI, MPE = pd, Rhat, ESS) %>% 
  write_csv(here("tables", "ddm_bs_dr.csv"))

# -----------------------------------------------------------------------------




# Learner variety familiarity match model table -------------------------------

# Model summary table
describe_posterior(learner_variety_match_response, rope_range = c(-0.1, 0.1)) %>% 
  as_tibble() %>% 
  select(-c("CI", "ROPE_CI", "ROPE_low", "ROPE_high")) %>% 
  mutate(Parameter = case_when(
    Parameter == "b_Intercept" ~ "Intercept", 
    Parameter == "b_sentence_typeinterrogativeMpartialMwh" ~ "Wh- question", 
    Parameter == "b_sentence_typedeclarativeMnarrowMfocus" ~ "Narrow focus", 
    Parameter == "b_sentence_typedeclarativeMbroadMfocus" ~ "Broad focus", 
    Parameter == "b_is_match_labfamiliar" ~ "Familiar", 
    Parameter == "b_sentence_typeinterrogativeMpartialMwh:is_match_labfamiliar" ~ "Wh- question:Familiar", 
    Parameter == "b_sentence_typedeclarativeMnarrowMfocus:is_match_labfamiliar" ~ "Narrow focus:Familiar", 
    Parameter == "b_sentence_typedeclarativeMbroadMfocus:is_match_labfamiliar" ~ "Broad focus:Famliar"
  )) %>% 
  mutate(across(-c("Parameter", "ESS"), specify_decimal, k = 2)) %>% 
  mutate(ESS = round(ESS)) %>% 
  mutate(across(-Parameter, printy::fmt_minus_sign)) %>% 
  mutate(HDI = glue("[{CI_low}, {CI_high}]")) %>% 
  select(Parameter, Median, HDI, `% in ROPE` = ROPE_Percentage, MPE = pd, Rhat, ESS) %>% 
  write_csv(here("tables", "learner_variety_match_response_model_summary.csv"))

# -----------------------------------------------------------------------------
