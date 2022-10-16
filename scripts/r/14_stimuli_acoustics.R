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
    cols = f0_00:f0_100, 
    names_to = c(".value", "time"), 
    names_sep = "_"
    ) %>% 
  transmute(filename, time = as.numeric(time), duration, f0) %>% 
  separate(
    col = filename, 
    into = c("variety", "condition", "type", "item"), 
    sep = "_"
    ) %>% 
  group_by(variety, type) %>% 
  mutate(time_bin = cut(time, seq(0, 100, 5), include.lowest = T, right = F), 
    f0_log = log(f0), 
    f0_log_z = (f0_log - mean(f0_log, na.rm = T)) / sd(f0_log, na.rm = T)) %>% 
  separate(time_bin, into = c("time_bin", "trash"), ",") %>% 
  mutate(time_bin = str_remove(time_bin, "\\["), 
         time_bin = as.numeric(time_bin)) %>% 
  select(-trash) %>% 
  write_csv(here("data", "tidy", "stimuli_acoustics_tidy.csv"))

# -----------------------------------------------------------------------------




# Data viz --------------------------------------------------------------------

stim_data <- read_csv(here("data", "tidy", "stimuli_acoustics_tidy.csv"))

# F0 as a function of sentence type and variety
stim_data %>% 
  filter(type != "filler", f0_log_z <= 2.5, f0_log_z >= -2.5) %>% 
  ggplot() + 
  aes(x = time_bin, y = f0_log_z, color = variety, fill = variety) + 
  facet_grid(variety ~ type) + 
  geom_line(aes(group = item), stat = "smooth", alpha = 0.1, se = F, 
    show.legend = F, formula = "y ~ x", method = "loess") + 
  geom_smooth(method = "loess", span = 0.5, formula = "y ~ x", size = 1.2, 
    show.legend = F) + 
  scale_color_manual(values = viridis::viridis_pal(
    option = "B", begin = 0.3, end = 0.8)(8)) + 
  scale_fill_manual(values = viridis::viridis_pal(
    option = "B", begin = 0.3, end = 0.8)(8)) + 
  scale_x_continuous(labels = scales::percent_format(scale = 1)) + 
  labs(y = NULL, x = NULL) + 
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
