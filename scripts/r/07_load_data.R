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
learners  <- read_csv(here("data", "tidy", "learners_all_tasks_tidy.csv")) %>% 
  mutate(
    question_statement = case_when(
      sentence_type %in% c("interrogative-partial-wh", "interrogative-total-yn") ~ "question", 
      sentence_type %in% c("declarative-narrow-focus", "declarative-broad-focus") ~ "statement"), 
    is_question = if_else(question_statement == "question", 1, -1), 
    lextale_std = (lextale_tra - mean(lextale_tra)) / sd(lextale_tra), 
    eq_std = (eq_score - mean(eq_score)) / sd(eq_score), 
    sentence_type = fct_relevel(sentence_type, 
      "interrogative-total-yn", "interrogative-partial-wh", 
      "declarative-narrow-focus"), 
    q_type = case_when(
      sentence_type == "interrogative-total-yn" ~ 1, 
      sentence_type == "interrogative-partial-wh" ~ -1, 
      TRUE ~ 0)
    )
natives   <- read_csv(here("data", "tidy", "natives_2afc_tidy.csv"))
full_2afc <- read_csv(here("data", "tidy", "complete_2afc_tidy.csv"))
id_remove <- readRDS(here("data", "tidy", "participants_removed_list.Rds"))

# -----------------------------------------------------------------------------
