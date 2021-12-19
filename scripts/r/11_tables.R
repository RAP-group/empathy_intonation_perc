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






