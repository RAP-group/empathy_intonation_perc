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

source(here::here("scripts", "r", "07_load_data.R"))

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

stim_data <- read_csv(here("data", "tidy", "stimuli_acoustics_tidy.csv")) %>% 
  mutate(variety_lab = case_when(
    variety == "andalusian" ~ "Andalusian", 
    variety == "argentine" ~ "Argentine", 
    variety == "castilian" ~ "Madrileño", 
    variety == "chilean" ~ "Chilean", 
    variety == "cuban" ~ "Cuban", 
    variety == "mexican" ~ "Mexican", 
    variety == "peruvian" ~ "Peruvian", 
    variety == "puertorican" ~ "Puerto Rican"), 
    type_lab = case_when(
      type == "interrogative-total-yn" ~ "y/n question", 
      type == "interrogative-partial-wh" ~ "Wh- question", 
      type == "declarative-narrow-focus" ~ "Narrow focus statement", 
      type == "declarative-broad-focus" ~ "Broad focus statement"
    ))

# F0 as a function of sentence type and variety
stimuli_pitch_contours <- stim_data %>% 
  filter(type != "filler", f0_log_z <= 2.5, f0_log_z >= -2.5) %>% 
  ggplot() + 
  aes(x = time_bin, y = f0_log_z, color = variety_lab, fill = variety_lab) + 
  facet_grid(variety_lab ~ type_lab) + 
  geom_line(aes(group = item), stat = "smooth", alpha = 0.15, se = F, 
    show.legend = F, formula = "y ~ x", method = "loess") + 
  stat_smooth(geom = "line", method = "loess", span = 0.5, formula = "y ~ x", size = 1.1, 
    show.legend = F, lineend = "round") + 
  scale_color_manual(values = viridis::viridis_pal(
    option = "B", begin = 0.3, end = 0.8)(8)) + 
  scale_fill_manual(values = viridis::viridis_pal(
    option = "B", begin = 0.3, end = 0.8)(8)) + 
  scale_x_continuous(labels = scales::label_percent(scale = 1)) + 
  coord_cartesian(xlim = c(0, 100)) + 
  labs(y = NULL, x = NULL) + 
  ds4ling::ds4ling_bw_theme(base_size = 12, base_family = "Times") + 
  theme(strip.background = element_rect(fill = NA), 
        axis.text.x = element_text(size = rel(0.75)))

walk(c('png', 'pdf'), ~ ggsave(
  filename = glue(file.path(here("figs", "manuscript")), "/stimuli_pitch_contours.", .x), 
  plot = stimuli_pitch_contours, 
  device = .x, height = 7.5, width = 6.5, units = "in"))

# -----------------------------------------------------------------------------




# Spectrogram stuff -----------------------------------------------------------

# Path to script
plot_script <- here("scripts", "praat", "6_plot_spectrogram.praat")

# Vector of variety names
variety_names <- c(
  "andalusian", "argentine", "castilian", "chilean", "cuban", "mexican", 
  "peruvian", "puertorican"
)

# Vector of utterances
utterances <- c(
  "_match_declarative-broad-focus_Ana-lleva-el-abrigo",
  "_match_declarative-narrow-focus_Ana-lleva-el-abrigo",
  "_match_interrogative-partial-wh_Cuando-lleva-el-abrigo",
  "_match_interrogative-total-yn_Ana-lleva-el-abrigo"
  )

# Generate vector of items
make_vec_of_items <- function(variety) {
  out <- as.character(glue("{variety}{utterances}"))
  return(out)
}

example_items <- map(variety_names, make_vec_of_items) %>% unlist
wavs <- glue('{here("data", "stimuli", "sounds")}/{example_items}.wav')
tgds <- glue('{here("data", "stimuli", "textgrids")}/{example_items}.TextGrid')
outs <- glue('{here("figs", "stimuli")}/{example_items}.png')

cap_hold <- c(
  "\\ -\\ Broad\\ focus", 
  "\\ -\\ Narrow\\ focus", 
  "\\ -\\ Wh-\\ question", 
  "\\ -\\ yes/no\\ question" 
)

variety_caps <- c("Andalusian", "Argentine", "Madrileño", "Chilean", 
  "Cuban", "Mexican", "Peruvian", "Puerto\\ Rican")

make_vec_of_caps <- function(variety) {
  out <- as.character(glue("{variety}{cap_hold}"))
  return(out)
}

caps <- map(variety_caps, make_vec_of_caps) %>% unlist

f0_mins <- c(
  100, 100, 50, 75,   # andalusian
  50, 50, 50, 50,     # argentine  
  10, 100, 50, 30,    # castilian
  70, 70, 70, 70,     # chilean
  100, 100, 100, 100, # cuban
  25, 75, 50, 50,     # mexican
  25, 25, 50, 75,     # peruvian
  100, 100, 50, 50    # puerto rican
  )
f0_maxs <- c(
  500, 300, 350, 500, # andalusian
  200, 200, 200, 200, # argentine 
  350, 350, 400, 450, # castilian
  250, 250, 350, 350, # chilean
  250, 250, 350, 350, # cuban
  400, 300, 550, 550, # mexican
  200, 200, 200, 350, # peruvian
  275, 275, 400, 400  # peurto rican
  )

hz_maxs <- c(rep(5000, 28), rep(4000, 4))

for (i in 1:length(wavs)) {
  speakr::praat_run(
    plot_script, 
    file = outs[i], 
    caption = caps[i], 
    wav = wavs[i], 
    tg = tgds[i], 
    start = 0, 
    end = 0, 
    width = 6, 
    format = "png", 
    pitch = TRUE, 
    pitch_min = f0_mins[i], 
    pitch_max = f0_maxs[i], 
    hz_max = hz_maxs[i]
  )
}

# Items to plot
img_ref <- glue("{utterances[1:4]}.png")

# Function to generate plots
combine_spectrograms <- function(variety) {
  # Pull .png files of utterance types by variety in 2x2 grid
  out <- reduce(map(glue('{here("figs", "stimuli")}/{variety}{img_ref}'), 
        ~ cowplot::ggdraw() + cowplot::draw_image(.x)), `+`
  )
  
  # Save as png and pdf
  walk(c('png', 'pdf'), ~ ggsave(
    filename = glue(here("figs", "stimuli"), glue("/spectrogram_{variety}."), .x), 
    plot = out, 
    device = .x, height = 6, width = 8.5, units = "in"))
}

# Generate plot for each variety
walk(variety_names, ~ combine_spectrograms(variety = .x))

# -----------------------------------------------------------------------------




# Accuracy --------------------------------------------------------------------

# Learner and monolingual accuracy plot
l2_native_accuracy <- bind_rows(
  learners %>% 
    select(participant, speaker_variety, sentence_type, is_correct) %>% 
    mutate(group = "L2"),
  
  natives %>% 
    select(participant, speaker_variety, sentence_type, is_correct) %>% 
    mutate(group = "Native")
  ) %>% 
  mutate(speaker_variety = case_when(
      speaker_variety == "andalusian" ~ "Andalusian", 
      speaker_variety == "argentine" ~ "Argentine", 
      speaker_variety == "castilian" ~ "Madrileño", 
      speaker_variety == "chilean" ~ "Chilean", 
      speaker_variety == "cuban" ~ "Cuban", 
      speaker_variety == "mexican" ~ "Mexican", 
      speaker_variety == "peruvian" ~ "Peruvian", 
      speaker_variety == "puertorican" ~ "Puerto Rican")
  ) %>% 
  group_by(group, sentence_type, speaker_variety) %>% 
  summarize(avg = mean_se(is_correct), .groups = "drop")

learner_native_accuracy <- l2_native_accuracy %>% 
  transmute(group = str_replace(group, "Native", "Monolingual"), 
    Type = case_when(
      sentence_type == "interrogative-total-yn" ~ "y/n\nquestion", 
      sentence_type == "interrogative-partial-wh" ~ "Wh-\nquestion", 
      sentence_type == "declarative-narrow-focus" ~ "Narrow focus\nstatement", 
      sentence_type == "declarative-broad-focus" ~ "Broad focus\nstatement"), 
    Variety = speaker_variety, 
    avg = avg$y, lower = .$avg$ymin, upper = .$avg$ymax) %>% 
  ggplot() + 
  aes(x = Type, y = avg, color = Variety) + 
  facet_grid(. ~ group) + 
  geom_hline(yintercept = 0.5, lty = 3) + 
  geom_errorbar(aes(ymin = lower, ymax = upper), show.legend = F, 
    position = position_dodge(0.5), width = 0) +
  geom_point(position = position_dodge(0.5)) + 
  scale_color_manual(name = NULL, 
    values = viridis::viridis_pal(option = "C", begin = 0.2, end = 0.8)(8)) + 
  coord_cartesian(ylim = c(0, 1.02), xlim = c(0.6, 4.4), expand = F) + 
  scale_y_continuous(labels = scales::percent_format()) + 
  labs(y = "Accuracy", x = NULL) + 
  ds4ling::ds4ling_bw_theme(base_size = 12, base_family = "Times") + 
  theme(legend.position = c(0.1, 0.25), legend.background = element_blank(), 
    legend.key.size = unit(0.35, "cm"), 
    strip.background = element_rect(fill = NA))

walk(c('png', 'pdf'), ~ ggsave(
  filename = glue(file.path(here("figs", "manuscript")), "/learner_native_accuracy.", .x), 
  plot = learner_native_accuracy, 
  device = .x, height = 4, width = 7, units = "in"))

# Learner and monolingual accuracy table
l2_native_accuracy_tbl <- l2_native_accuracy %>% 
  pivot_wider(names_from = "group", values_from = "avg") %>% 
  transmute(
    Type = sentence_type, Variety = speaker_variety, 
    L2 = glue("{specify_decimal(L2$y, 2)} [{specify_decimal(L2$ymin, 2)}, {specify_decimal(L2$ymax, 2)}]"),
    Monolingual = glue("{specify_decimal(Native$y, 2)} [{specify_decimal(Native$ymin, 2)}, {specify_decimal(Native$ymax, 2)}]")
  ) %>% 
  mutate(Type = case_when(
    Type == "interrogative-total-yn" ~ "y/n question", 
    Type == "interrogative-partial-wh" ~ "Wh- question", 
    Type == "declarative-narrow-focus" ~ "Narrow focus statement", 
    Type == "declarative-broad-focus" ~ "Broad focus statement"
  )) %>% 
  write_csv(here("tables", "learner_native_accuracy.csv"))

# Monolingual listener/speaker match plot
variety_matches <- natives %>% 
  mutate(
    variety_match = case_when(
      spn_variety == "Mexico" & speaker_variety == "mexican" ~ 1, 
      spn_variety == "Chile" & speaker_variety == "chilean" ~ 1, 
      spn_variety == "Spain" & speaker_variety == "castilian" ~ 1, 
      spn_variety == "cuban" & speaker_variety == "cuban" ~ 1, 
      spn_variety == "puertorican" & speaker_variety == "puertorican" ~ 1, 
      spn_variety == "andalusian" & speaker_variety == "andalusian" ~ 1, 
      TRUE ~ 0
    ), 
    spn_variety = case_when(
      speaker_variety == "andalusian" ~ "Andalusian", 
      speaker_variety == "argentine" ~ "Argentine", 
      speaker_variety == "castilian" ~ "Madrileño", 
      speaker_variety == "chilean" ~ "Chilean", 
      speaker_variety == "cuban" ~ "Cuban", 
      speaker_variety == "mexican" ~ "Mexican", 
      speaker_variety == "peruvian" ~ "Peruvian", 
      speaker_variety == "puertorican" ~ "Puerto Rican"), 
    Type = case_when(
      sentence_type == "interrogative-total-yn" ~ "y/n\nquestion", 
      sentence_type == "interrogative-partial-wh" ~ "Wh-\nquestion", 
      sentence_type == "declarative-narrow-focus" ~ "Narrow focus\nstatement", 
      sentence_type == "declarative-broad-focus" ~ "Broad focus\nstatement")
  ) %>% 
  filter(variety_match == 1)

native_variety_matched_accuracy <- variety_matches %>% 
  ggplot() + 
  aes(x = Type, y = is_correct, color = spn_variety) + 
  geom_hline(yintercept = 0.5, lty = 3) + 
  stat_summary(fun.data = mean_se, geom = "pointrange", 
               position = position_dodge(0.5)) + 
  scale_color_manual(name = NULL, 
    values = viridis::viridis_pal(option = "C", begin = 0.2, end = 0.8)(6)) + 
  coord_cartesian(ylim = c(0.48, 1.02), xlim = c(0.6, 4.4), expand = F) + 
  scale_y_continuous(labels = scales::percent_format()) + 
  labs(y = "Accuracy", x = NULL) + 
  ds4ling::ds4ling_bw_theme(base_size = 12, base_family = "Times") + 
  theme(legend.position = c(0.1, 0.25), legend.background = element_blank(), 
        legend.key.size = unit(0.35, "cm"), 
        strip.background = element_rect(fill = NA))

walk(c('png', 'pdf'), ~ ggsave(
  filename = glue(here("figs", "manuscript"), "/native_variety_matched_accuracy.", .x), 
  plot = native_variety_matched_accuracy, 
  device = .x, height = 4, width = 8.5, units = "in"))

variety_matches %>% 
  group_by(sentence_type, spn_variety) %>% 
  summarize(avg = mean_se(is_correct), .groups = "drop") %>% 
  transmute(
    Type = sentence_type, Variety = spn_variety, 
    Accuracy = glue("{specify_decimal(avg$y, 2)} [{specify_decimal(avg$ymin, 2)}, {specify_decimal(avg$ymax, 2)}]")
  ) %>% 
  mutate(Type = case_when(
    Type == "interrogative-total-yn" ~ "y/n question", 
    Type == "interrogative-partial-wh" ~ "Wh- question", 
    Type == "declarative-narrow-focus" ~ "Narrow focus statement", 
    Type == "declarative-broad-focus" ~ "Broad focus statement"
  )) %>% 
  pivot_wider(names_from = Type, values_from = Accuracy) %>% 
  write_csv(here("tables", "variety_matched_native_accuracy.csv"))

# -----------------------------------------------------------------------------
