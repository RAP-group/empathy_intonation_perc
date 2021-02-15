# Combine tasks ---------------------------------------------------------------
#
# This script takes the empathy quotient, lextale, and 2afc data and combines 
# it into a single dataframe for learners. 
#
# -----------------------------------------------------------------------------




# Source libraries and helpers ------------------------------------------------

source(here::here("scripts", "r", "01_helpers.R"))

# -----------------------------------------------------------------------------



# Combine tasks ---------------------------------------------------------------

read_csv(here("data", "tidy", "learners_2afc_tidy.csv")) %>% 
  left_join(., read_csv(here("data", "tidy", "learners_eq_tidy.csv")) %>%
    select(participant, eq_score), by = "participant") %>% 
  left_join(., read_csv(here("data", "tidy", "learners_lextale_tidy.csv")) %>% 
    select(participant, lextale_avg, lextale_tra), by = "participant") %>% 
  write_csv(here("data", "tidy", "learners_all_tasks_tidy.csv"))

# -----------------------------------------------------------------------------
