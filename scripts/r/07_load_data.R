# Load all data ---------------------------------------------------------------
#
# This script sources libraries and helpers and loads all tidy data
#
# -----------------------------------------------------------------------------



# Source libraries and helpers ------------------------------------------------

source(here::here("scripts", "r", "01_helpers.R"))

# -----------------------------------------------------------------------------



# Load all tidy data ----------------------------------------------------------

sr        <- read_csv(here("data", "tidy", "speech_rate_tidy.csv"))
learners  <- read_csv(here("data", "tidy", "learners_all_tasks_tidy.csv"))
natives   <- read_csv(here("data", "tidy", "natives_2afc_tidy.csv"))
full_2afc <- read_csv(here("data", "tidy", "complete_2afc_tidy.csv"))

# -----------------------------------------------------------------------------
