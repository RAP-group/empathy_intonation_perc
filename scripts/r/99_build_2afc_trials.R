# Build 2AFC trials -----------------------------------------------------------
#
# Last update: 2021/02/07
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

# English
instructions_text <- tibble(
  instructions_text = c(
  "In the following task you are going to hear a series of utterances. 

  You will decide if the utterances sound like questions or not using your keyboard.",  
  
  "If you believe the utterance is a question, type '1' (for 'yes') and if you do not 
  believe it is a question type '0' (for 'no').

  Let's practice a bit..."), 
  continue_text = rep("(press the spacebar to continue)", 2)
)

# Spanish
instructions_sp_text <- tibble(
  instructions_text = c(
  "En la siguiente tarea vas a escuchar una serie de frases cortas. 

  Vas a decidir si suenan como preguntas o no usando el teclado.", 

  "Si crees que el enunciado es una pregunta, teclea '1' (Sí) y si crees que 
  no es una pregunta teclea '0' (no).

  Vamos a practicar..."), 
  continue_text = rep("(presiona la barra espaciadora para continuar)", 2)
)

# -----------------------------------------------------------------------------




# Build practice trials data frame --------------------------------------------
trial_item_path_response <- c(
  "./stim/wavs/castilian_question_filler_Mi-esposo-conoce-al-chico.wav"     = 1, 
  "./stim/wavs/argentine_question_filler_Mi-vecino-habla-mucho.wav"         = 1, 
  "./stim/wavs/chilean_question_filler_Mi-padre-cantaba-bien.wav"           = 1, 
  "./stim/wavs/mexican_question_filler_Felipe-no-tiene-contrasena.wav"      = 1, 
  "./stim/wavs/cuban_statement_filler_Cocinabas-por-la-noche.wav"           = 0, 
  "./stim/wavs/andalusian_statement_filler_Alberto-nunca-se-afeita.wav"     = 0, 
  "./stim/wavs/peruvian_statement_filler_El-nino-habla-con-su-madre.wav"    = 0, 
  "./stim/wavs/puertorican_statement_filler_La-nina-pinta-con-su-amiga.wav" = 0
)

twoafc_practice_trials <- tibble(
  path = names(trial_item_path_response), 
  correct_response = trial_item_path_response
)

# -----------------------------------------------------------------------------




# Build trials data frame -----------------------------------------------------

path_remove <- "/Users/casillas/academia/research/in_progress/empathy_intonation_perc/exp/empathy_intonation_perc/stim/wavs/"
path_add <- "./stim/wavs/"

twoafc_trials <- dir_ls(here("exp", "empathy_intonation_perc", "stim", "wavs"), 
  regexp = "\\.wav$") %>% 
  as_tibble() %>% 
  transmute(file = str_remove(value, pattern = path_remove)) %>% 
  separate(col = file, into = c("variety", "type", "item_type", "dump"), 
    sep = "_", remove = F) %>% 
  filter(item_type != "filler") %>% 
  select(variety, type, item_type, file) %>% 
  group_by(variety) %>% 
  mutate(item_num = seq_along(file)) %>% 
  ungroup() %>% 
  mutate(correct_response = case_when(
    item_type == "declarative-broad-focus"  ~ 0, 
    item_type == "declarative-narrow-focus" ~ 0, 
    item_type == "interrogative-partial-wh" ~ 1, 
    item_type == "interrogative-total-yn"   ~ 1, 
    type == "question"  ~ 1, 
    type == "statement" ~ 0
  )) %>% 
  pivot_wider(id_cols = c("variety", "item_num", "correct_response"), 
    names_from = variety, values_from = file) %>% 
  mutate(across(andalusian:puertorican, ~paste0(path_add, .x)))

# -----------------------------------------------------------------------------




# Save as csv's and xlsx files ------------------------------------------------

write_csv(instructions_text, 
  here("exp", "empathy_intonation_perc", "instructions", "2afc_instructions_text.csv"))
write_csv(instructions_sp_text, 
  here("exp", "empathy_intonation_perc_sp", "instructions", "2afc_instructions_sp_text.csv"))
write_csv(twoafc_practice_trials, 
  here("exp", "empathy_intonation_perc", "trials", "twoafc_practice_trials.csv"))
write_csv(twoafc_trials, 
  here("exp", "empathy_intonation_perc", "trials", "twoafc_trials.csv"))

write_xlsx(instructions_text, 
  here("exp", "empathy_intonation_perc","instructions", "2afc_instructions_text.xlsx"), 
  format_headers = F)
write_xlsx(instructions_sp_text, 
  here("exp", "empathy_intonation_perc_sp","instructions", "2afc_instructions_sp_text.xlsx"), 
  format_headers = F)
write_xlsx(twoafc_practice_trials, 
  here("exp", "empathy_intonation_perc", "trials", "twoafc_practice_trials.xlsx"), 
  format_headers = F)
write_xlsx(twoafc_trials, 
  here("exp", "empathy_intonation_perc", "trials", "twoafc_trials.xlsx"), 
  format_headers = F)

# -----------------------------------------------------------------------------
