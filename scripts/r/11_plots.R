# Model stuff


post_samples <- posterior_samples(native_response_01) %>% glimpse
  rename(intercept = b_Intercept, 
         participant_sd = sd_participant__Intercept, 
         sentence_type_sd = sd_sentence_type__Intercept, 
         speaker_variety_sd = sd_speaker_variety__Intercept, 
         speaker_variety_sentence_type_sd = `sd_speaker_variety:sentence_type__Intercept`)

# Grouping variable posteriors
posterior_samples(native_response_01) %>% 
  select(starts_with("sd")) %>% 
  gather(key, sd) %>% 
  mutate(key = str_remove(key, "sd_") %>% 
           str_remove(., "__Intercept")) %>% 
  ggplot(aes(x = sd, fill = key)) +
  geom_density(color = "white", alpha = 0.7) +
  scale_fill_viridis_d(name = "Group-level variance", option = "D", end = 0.9, 
    labels = c("Participant", "Sentence type", "Speaker variety", 
    "Speaker variety:sentence type", "Speaker variety:sentence type:sentence")) + 
  coord_cartesian(xlim = c(0, 4)) + 
  labs(y = "Density", x = expression(beta)) + 
  minimal_adj() + 
  theme(legend.position = c(0.85, 0.75), legend.title = element_text(size = 9), 
    legend.text = element_text(size = 8), legend.key.size = unit(0.5, "cm"))




# By participant

# Get draws for each study
participant_draws <- 
  spread_draws(native_response_01, r_participant[Participant,], b_Intercept) %>% 
  mutate(b_Intercept = r_participant + b_Intercept)

# Get draws for pooled effect
participant_pooled_effect_draws <- 
  spread_draws(native_response_01, b_Intercept) %>% 
  mutate(Participant = "Pooled Effect")

# Combine it and clean up
participant_forest_data <- 
  bind_rows(participant_draws, participant_pooled_effect_draws) %>% 
  ungroup() %>%
  mutate(Participant = reorder(Participant, b_Intercept), 
         Participant = relevel(Participant, "Pooled Effect", after = Inf))

# Calculate mean qi intervals for right margin text
participant_forest_data_summary <- 
  group_by(participant_forest_data, Participant) %>% 
  mean_qi(b_Intercept, .width = 0.95) 

# Calculate mean qi intervals for pooled effect
participant_pooled_summary <- 
  group_by(participant_forest_data, Participant) %>% 
  mean_qi(b_Intercept, .width = c(0.5, 0.8, 0.95)) %>% 
  filter(Participant == "Pooled Effect")

# Plot it all
p_post_participant <- participant_forest_data %>% 
  filter(Participant != "Pooled Effect") %>% 
  ggplot() + 
  aes(x = b_Intercept, y = Participant) + 
    geom_text(data = 
    mutate_if(participant_forest_data_summary, is.numeric, round, 2) %>% 
    mutate_at(c("b_Intercept", ".lower", ".upper"), as.character) %>% 
    mutate_at(c("b_Intercept", ".lower", ".upper"), unicode_minus) %>% 
    mutate_at(c("b_Intercept", ".lower", ".upper"), strip_blank),
            aes(label = glue("{b_Intercept} [{.lower}, {.upper}]"), x = Inf), 
            hjust = "inward", family = "Times", alpha = 0) + 
  geom_tile(data = participant_pooled_summary, aes(width = .lower - .upper),
    alpha = 0.75, height = Inf, fill = "#31688EFF") +
  stat_pointinterval(point_fill = "white", shape = 21, alpha = 0.85, 
    .width = c(0.8, 0.95), point_size = 1.5) +
  coord_cartesian(xlim = c(0, 7)) + 
  labs(x = expression(beta), y = NULL) +
  theme_test(base_size = 12) + 
  theme(axis.text.y = element_blank(), axis.ticks.y = element_blank())



# By speaker variety

# Get draws for each study
speaker_variety_draws <- 
  spread_draws(native_response_01, r_speaker_variety[Speaker_variety,], b_Intercept) %>% 
  mutate(b_Intercept = r_speaker_variety + b_Intercept)

# Get draws for pooled effect
speaker_variety_pooled_effect_draws <- 
  spread_draws(native_response_01, b_Intercept) %>% 
  mutate(Speaker_variety = "Pooled Effect")

# Combine it and clean up
speaker_variety_forest_data <- 
  bind_rows(speaker_variety_draws, speaker_variety_pooled_effect_draws) %>% 
  ungroup() %>%
  mutate(Speaker_variety = reorder(Speaker_variety, b_Intercept), 
         Speaker_variety = relevel(Speaker_variety, "Pooled Effect", after = Inf))

# Calculate mean qi intervals for right margin text
speaker_variety_forest_data_summary <- 
  group_by(speaker_variety_forest_data, Speaker_variety) %>% 
  mean_qi(b_Intercept, .width = 0.95) 

# Calculate mean qi intervals for pooled effect
speaker_variety_pooled_summary <- 
  group_by(speaker_variety_forest_data, Speaker_variety) %>% 
  mean_qi(b_Intercept, .width = c(0.5, 0.8, 0.95)) %>% 
  filter(Speaker_variety == "Pooled Effect")

# Plot it all
p_post_speaker_variety <- speaker_variety_forest_data %>% 
  ggplot() + 
  aes(x = b_Intercept, y = Speaker_variety) + 
  geom_text(data = 
    mutate_if(speaker_variety_forest_data_summary, is.numeric, round, 2) %>% 
    mutate_at(c("b_Intercept", ".lower", ".upper"), as.character) %>% 
    mutate_at(c("b_Intercept", ".lower", ".upper"), unicode_minus) %>% 
    mutate_at(c("b_Intercept", ".lower", ".upper"), strip_blank),
            aes(label = glue("{b_Intercept} [{.lower}, {.upper}]"), x = Inf), 
            hjust = "inward", family = "Times") + 
  geom_rect(data = speaker_variety_pooled_summary, 
    aes(xmin = .lower, xmax = .upper, ymin = -Inf, ymax = Inf),
    alpha = 0.2, fill = "#31688EFF") +
  stat_halfeye(point_fill = "white", shape = 21, .width = c(0.8, 0.95)) +
  coord_cartesian(xlim = c(0, 8)) + 
  labs(x = expression(italic("beta")), y = NULL) +
  minimal_adj() + 
  theme(axis.text.y = element_text(hjust = 0))

p_post_participant
p_post_speaker_variety





as_tibble(native_response_01) %>% glimpse()

ranef(native_response_01)$`speaker_variety:sentence_type:sentence`
ranef(native_response_01)$`speaker_variety:sentence_type`
ranef(native_response_01)$speaker_variety
ranef(native_response_01)$sentence_type 
ranef(native_response_01)$participant 





















# SPeech rate stuff


sr_desc <- sr %>% 
  pivot_longer(cols = c("speech_rate", "articulation_rate", "avg_syll_dur"), 
  names_to = "metric", values_to = "val") %>% 
  group_by(variety, metric) %>% 
  summarize(avg_val = mean(val), med_val = median(val), .groups = "drop") %>% 
  pivot_longer(cols = c("avg_val", "med_val"), names_to = "measure", 
    values_to = "val")

sr %>% 
  ggplot() + 
  aes(x = speech_rate) + 
  facet_wrap(~ variety, nrow = 2) + 
  geom_histogram(fill = "lightblue", color = "black", 
    bins = 8, 
    aes(y = ..density..)) + 
  geom_density(size = 1, color = "darkred") + 
  geom_vline(data = filter(sr_desc, metric == "speech_rate"), 
    aes(xintercept = val, color = measure)) + 
  scale_color_viridis_d(name = NULL, option = "D", end = 0.8, 
                        labels = c("Mean", "Median")) + 
  labs(x = "Speech rate", y = "Density") + 
  ds4ling::ds4ling_bw_theme(base_family = "Times", base_size = 16)

sr %>% 
  pivot_longer(cols = c("speech_rate", "articulation_rate", "avg_syll_dur"), 
    names_to = "metric", values_to = "val") %>% 
  group_by(metric) %>% 
  mutate(val_std = (val - mean(val)) / sd(val), 
         variety = fct_relevel(variety, rev)) %>% 
  filter(metric == "speech_rate") %>% 
  ggplot() + 
  aes(x = val_std, y = variety, color = variety) + 
  geom_vline(xintercept = 0, lty = 3) + 
  geom_beeswarm(alpha = 0.2, groupOnX = F, show.legend = F) + 
  stat_summary(fun.data = mean_cl_normal, geom = "pointrange", show.legend = F, 
    pch = 21, color = "black", size = 1.1, aes(fill = variety)) + 
  scale_color_viridis_d(option = "D", end = 0.9) + 
  scale_fill_viridis_d(option = "D", end = 0.9) + 
  coord_cartesian(xlim = c(-2.5, 2.5)) + 
  labs(x = "Speech rate (std)", y = "Spanish variety") + 
  ds4ling::ds4ling_bw_theme(base_family = "Times", base_size = 16)
