# Stimuli acoustics analysis --------------------------------------------------
#
# Author: Joseph V. Casillas
# Last update: 20221012
#
# This script will:
#   - run `5_extractVals_stimuli.praat` to get acoustic data from stimuli
#   - save untidy data to 'data/raw/stimuli_acoustics.csv'
#   - tidy the data derived from the 2AFC stimuli
#   - save tidy data in `data/tidy/stimuli_acoustics_tidy.csv`
#   - generate exploratory plots
#
# -----------------------------------------------------------------------------




# Source libs and helpers -----------------------------------------------------

source(here::here("scripts", "r","01_helpers.R"))

# -----------------------------------------------------------------------------




# Run praat script ------------------------------------------------------------

# Path to script
stim_script <- here("scripts", "praat", "5_extractVals_stimuli.praat")

# Run script, capture output, read as csv
stim_data_raw <- speakr::praat_run(stim_script, capture = TRUE) %>%
  read_csv(na = "--undefined--") %>% 
  write_csv(here("data", "raw", "stimuli_acoustics.csv"))

# -----------------------------------------------------------------------------




# Tidy data -------------------------------------------------------------------
#
# - pivot from wide to long format
# - rename variables
# - separate 'filename' into 'variety', 'condition', 'type', and 'item' cols
# - log transform and standardize
# - pivot from long to wide for metrics
# - save as tidy .csv
#
stim_data <- stim_data_raw %>% 
  pivot_longer(
    cols = f0_00:in_100, 
    names_to = c(".value", "time"), 
    names_sep = "_"
    ) %>% 
  transmute(filename, time = as.numeric(time), duration, f0, f1, f2, 
    intensity = `in`) %>% 
  separate(
    col = filename, 
    into = c("variety", "condition", "type", "item"), 
    sep = "_"
    ) %>% 
  group_by(variety) %>% 
  mutate(across(
    .cols = c("f0", "f1", "f2", "intensity"), 
    .fns = log, 
    .names = "{.col}_log")
    ) %>% 
  mutate(across(
    .cols = c("f0_log", "f1_log", "f2_log", "intensity_log"), 
    .fns = simple_scale, 
    .names = "{.col}_z")
    ) %>% 
  pivot_longer(
    cols = c("duration", "intensity", starts_with("f"), contains("_log")), 
    names_to = "metric", values_to = "val") %>% 
  write_csv(here("data", "tidy", "stimuli_acoustics_tidy.csv"))

# -----------------------------------------------------------------------------




# Data viz --------------------------------------------------------------------

# F0 as a function of sentence type and variety
stim_data %>% 
  filter(metric == "f0_log_z", type != "filler") %>% 
  ggplot() + 
  aes(x = time, y = val, color = variety, fill = variety) + 
  facet_grid(variety ~ type, scales = "free_y") + 
  stat_summary(fun = mean, geom = "line") + 
  stat_summary(fun = mean, geom = "point", color = "white", pch = 21, 
    size = 3, stroke = 1.2) + 
  scale_color_manual(values = viridis::viridis_pal(
    option = "B", begin = 0.25, end = 0.85)(stim_data$variety %>% unique %>% length)) + 
  scale_fill_manual(values = viridis::viridis_pal(
    option = "B", begin = 0.25, end = 0.85)(stim_data$variety %>% unique %>% length)) + 
  scale_x_continuous(labels = scales::percent_format(scale = 1)) + 
  ds4ling::ds4ling_bw_theme(base_size = 12, base_family = "Times") 

# Spectrogram

filename <- "andalusian_match_declarative-broad-focus_Ana-lleva-el-abrigo"
ext_in   <- ".wav"
ext_out  <- ".png"
wav <- here("exp", "empathy_intonation_perc", "stim", "wavsx", paste0(filename, ext_in))
speakr::praat_plot(
  #here("figs", "stimuli", glue(filename, ext_out)),
  "./figs/stimuli/andalusian_match_declarative-broad-focus_Ana-lleva-el-abrigo.png", 
  wav, 
  f0 = TRUE, 
  f0_max = 400 
  )

library(phonTools)
sound <- loadsound()
spectrogram(sound, fs = 44100, colors = TRUE, 
            maintitle = "Welcome", maxfreq = 5500)


library("tuneR")
library("seewave")
library("phonTools")


polo_wav <- tuneR::readWave(wav)
input <- seewave::inputw(wave = polo_wav)
wave <- input$w
head(wave)

wav_df <- wave %>%
  as_tibble() %>%
  rename(hz = V1) %>%
  tibble::rowid_to_column("sample") %>%
  mutate(t = sample / polo_wav@samp.rate)

ggplot(wav_df, aes(t, hz)) + 
  geom_line()

phonTools::spectrogram(wav_df$hz)


# -----------------------------------------------------------------------------
