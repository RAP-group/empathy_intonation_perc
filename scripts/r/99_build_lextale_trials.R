# Build lexTALE trials --------------------------------------------------------
#
# Last update: 2020/10/15
#
# - Create .csv and .xlsx files for the following: 
#    - instructions
#    - practice trials
#    - trials
# - Save files to exp dir
#
# -----------------------------------------------------------------------------


# Source libs and helpers -----------------------------------------------------

source(here::here("scripts", "r", "01_helpers.R"))

# -----------------------------------------------------------------------------




# Build instructions data frame -----------------------------------------------

instructions_text <- tibble(
  instructions_text = c(
  "Great! Now we will move on. The next task consists of about 60 trials, in each of which you will see a string of letters. 
  
  Your task is to decide whether this is a real Spanish word or not.", 
  
  "If you think it is an existing Spanish word, you type '1' (for 'REAL'), and if you think it is not an existing Spanish word, you type '0' (for 'FAKE').", 
  
  "If you are sure that the word exists, even though you don’t know its exact meaning, you may still respond '1' (REAL). 
  
  But if you are not sure if it is an existing word, you should respond '0' (FAKE).
  
  Let's do a practice round..."), 
  continue_text = rep("(press the spacebar to continue)", 
    n = length(instructions_text))
)

# -----------------------------------------------------------------------------




# Build practice trials data frame --------------------------------------------

trial_item_cor_response <- c(
  "depiste" = 0, "sí" = 1, "coné" = 0, "calpar" = 0, "joten" = 0, 
  "sacapuntas" = 1, "priba" = 0, "pelasula" = 0, "bien" = 1, "casa" = 1, 
  "lejo" = 0, "pretantas" = 0)

lextale_practice_trials <- tibble(
  stim = 0, 
  word = names(trial_item_cor_response),
  correct_response = trial_item_cor_response, 
  prac_button_real = rep("REAL", times = 12), 
  prac_button_false = rep("FAKE", times = 12)
)

# -----------------------------------------------------------------------------




# Build trials data frame -----------------------------------------------------

item_cor_response <- c(
  "terzo" = 0, "pellizcar" = 1, "pulmones" = 1, "batillón" = 0, "zapato" = 1, 
  "tergiversar" = 1, "pésimo" = 1, "cadeña" = 0, "hacha" = 1, "antar" = 0, 
  "cenefa" = 1, "asesinato" = 1, "helar" = 1, "yunque" = 1, "regar" = 1, 
  "abracer" = 0, "floroso" = 0, "arsa" = 0, "brecedad" = 0, "ávido" = 1, 
  "capillo" = 0, "lacayo" = 1, "lampera" = 0, "látigo" = 1, "bisagra" = 1, 
  "secuestro" = 1, "acutación" = 0, "merodear" = 1, "decar" = 0, 
  "alardio" = 0, "pandilla" = 1, "fatacidad" = 0, "pauca" = 0, "aviso" = 1, 
  "rompido" = 0, "loro" = 1, "granuja" = 1, "estornudar" = 1, "torpe" = 1, 
  "alfombra" = 1, "rebuscar" = 1, "cadallo" = 0, "canela" = 1, "cuchara" = 1, 
  "jilguero" = 1, "martillo" = 1, "cartinar" = 0, "ladrón" = 1, "ganar" = 1, 
  "flamida" = 0, "candado" = 1, "camisa" = 1, "vegada" = 0, "fomentar" = 1, 
  "nevar" = 1, "musgo" = 1, "tacaño" = 1, "plaudir" = 0, "besar" = 1, 
  "matar" = 1, "seda" = 1, "flaco" = 1, "esposante" = 0, "orgulloso" = 1, 
  "bizcocho" = 1, "hacido" = 0, "cabello" = 1, "alegre" = 1, "engatusar" = 1, 
  "temblo" = 0, "polvoriento" = 1, "pemición" = 0, "hervidor" = 1, 
  "cintro" = 0, "yacer" = 1, "atar" = 1, "tiburón" = 1, "frondoso" = 1, 
  "tropaje" = 0, "hormiga" = 1, "pozo" = 1, "empirador" = 0, "guante" = 1, 
  "escuto" = 0, "laúd" = 1, "barato" = 1, "grodo" = 0, "acantilado" = 1, 
  "prisa" = 1, "clavel" = 1)

lextale_trials <- tibble(
  stim = 1:90, 
  word = names(item_cor_response), 
  correct_response = item_cor_response, 
  button_real = rep("REAL", times = 90), 
  button_false = rep("FAKE", times = 90)
)

# -----------------------------------------------------------------------------




# Save as csv's and xlsx files ------------------------------------------------

write_csv(instructions_text, 
  here("exp", "empathy_intonation_perc", "instructions", "lextale_instructions_text.csv"))
write_csv(lextale_practice_trials, 
  here("exp", "empathy_intonation_perc", "trials", "lextale_practice_trials.csv"))
write_csv(lextale_trials, 
  here("exp", "empathy_intonation_perc", "trials", "lextale_trials.csv"))

write_xlsx(instructions_text, 
  here("exp", "empathy_intonation_perc", "instructions", 
    "lextale_instructions_text.xlsx"), format_headers = F)
write_xlsx(lextale_practice_trials, 
  here("exp", "empathy_intonation_perc", "trials", "lextale_practice_trials.xlsx"), 
  format_headers = F)
write_xlsx(lextale_trials, 
  here("exp", "empathy_intonation_perc", "trials", "lextale_trials.xlsx"), 
  format_headers = F)

# -----------------------------------------------------------------------------
