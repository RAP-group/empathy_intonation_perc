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
  group_by(speaker_variety, metric) %>% 
  summarize(avg_val = mean(val), med_val = median(val), .groups = "drop") %>% 
  pivot_longer(cols = c("avg_val", "med_val"), names_to = "measure", 
    values_to = "val")

sr %>% 
  ggplot() + 
  aes(x = speech_rate) + 
  facet_wrap(~ speaker_variety, nrow = 2) + 
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
         speaker_variety = fct_relevel(speaker_variety, rev)) %>% 
  filter(metric == "speech_rate") %>% 
  ggplot() + 
  aes(x = val_std, y = speaker_variety, color = speaker_variety) + 
  geom_vline(xintercept = 0, lty = 3) + 
  geom_beeswarm(alpha = 0.2, groupOnX = F, show.legend = F) + 
  stat_summary(fun.data = mean_cl_normal, geom = "pointrange", show.legend = F, 
    pch = 21, color = "black", size = 1.1, aes(fill = speaker_variety)) + 
  scale_color_viridis_d(option = "D", end = 0.9) + 
  scale_fill_viridis_d(option = "D", end = 0.9) + 
  coord_cartesian(xlim = c(-2.5, 2.5)) + 
  labs(x = "Speech rate (std)", y = "Spanish variety") + 
  ds4ling::ds4ling_bw_theme(base_family = "Times", base_size = 16)






# ARESTY POSTER IMG

# % correct by speaker variety
learner_accuracy_by_speaker_variety <- learners %>% 
  filter(rt_adj >= -0.1, rt_adj <= 5) %>% 
  group_by(participant, speaker_variety) %>% 
  summarize(avg_correct = mean(is_correct), .groups = "drop") %>% 
  mutate(
    speaker_variety = fct_recode(speaker_variety, `Puerto Rican` = "puertorican"), 
    speaker_variety = str_to_title(speaker_variety, locale = "en")
    ) %>% 
  ggplot(., aes(x = speaker_variety, y = avg_correct)) + 
    geom_hline(yintercept = nat_d$mean_cor$val, lty = 3) + 
    geom_text(aes(label = label), hjust = 1, nudge_x = 0.5, size = 2.5, family = "Times", 
      data = tibble(speaker_variety = "Puerto Rican", avg_correct = 0.83, 
      label = paste0("Overall\nAvg. = ", round(nat_d$mean_cor$val, 2)))) + 
    stat_summary(aes(fill = speaker_variety), fun.data = mean_cl_boot, 
      geom = "pointrange", pch = 21, size = 0.8, show.legend = F) + 
    scale_fill_brewer(palette = "Set2") + 
    coord_cartesian(ylim = c(0.5, 1)) + 
    labs(y = "Proportion correct", x = NULL, 
         title = "Proportion correct as a function of speaker variety", 
         caption = "Mean ± 95% CI") + 
    ds4ling::ds4ling_bw_theme(base_size = 13, base_family = "Times")

learner_accuracy_by_utterance_type <- learners %>% 
  filter(rt_adj >= -0.1, rt_adj <= 5) %>% 
  group_by(participant, sentence_type) %>% 
  summarize(avg_correct = mean(is_correct), .groups = "drop") %>% 
  mutate(sentence_type = case_when(
      sentence_type == "interrogative-total-yn" ~ "Interrogative\ny/n", 
      sentence_type == "interrogative-partial-wh" ~ "Interrogative\nWh-", 
      sentence_type == "declarative-narrow-focus" ~ "Declarative\nnarrow focus", 
      TRUE ~ "Declarative\nbroad focus")) %>% 
  ggplot(., aes(x = sentence_type, y = avg_correct)) + 
    geom_hline(yintercept = nat_d$mean_cor$val, lty = 3) + 
    geom_text(aes(label = label), hjust = 1, nudge_x = 0.5, size = 2.5, family = "Times", 
      data = tibble(sentence_type = "Interrogative\ny/n", avg_correct = 0.83, 
      label = paste0("Overall\nAvg. = ", round(nat_d$mean_cor$val, 2)))) + 
    stat_summary(aes(fill = sentence_type), fun.data = mean_cl_boot, 
      geom = "pointrange", pch = 21, size = 0.8, show.legend = F) + 
    scale_fill_brewer(palette = "Set2") + 
    labs(y = "Proportion correct", x = NULL, 
         title = "Proportion correct as a function of utterance type", 
         caption = "Mean ± 95% CI") + 
    ds4ling::ds4ling_bw_theme(base_size = 13, base_family = "Times")

lt_me <- conditional_effects(learner_response_01, 
  effects = "lextale_std", 
  re_formula = NA, 
  method = "posterior_epred", 
  spaghetti = TRUE, 
  nsamples = 300)

learner_accuracy_by_lextale <- plot(lt_me, plot = F, 
  line_args = list(size = 3, colour = "white"))[[1]] + 
  coord_cartesian(ylim = c(NA, 1)) + 
  labs(y = "Proportion correct", x = "LexTALE score", 
  title = "Proportion correct as a function of LexTALE score", 
  subtitle = "300 draws from the posterior distribution") +   
  ds4ling::ds4ling_bw_theme(base_size = 13, base_family = "Times")

learner_accuracy_by_st_eq <- learners %>% 
  filter(rt_adj >= -0.1, rt_adj <= 5) %>% 
  mutate(sentence_type = case_when(
      sentence_type == "interrogative-total-yn" ~ "Interrogative\ny/n", 
      sentence_type == "interrogative-partial-wh" ~ "Interrogative\nWh-", 
      sentence_type == "declarative-narrow-focus" ~ "Declarative\nnarrow focus", 
      TRUE ~ "Declarative\nbroad focus")) %>% 
  ggplot(., aes(x = eq_score, y = is_correct, color = sentence_type)) + 
    geom_hline(yintercept = 0.5, lty = 3, color = "black") + 
    geom_jitter(width = 0.3, height = 0.01, alpha = 0.05, pch = 21) + 
    geom_smooth(method = "glm", method.args = list(family = "binomial"), 
      se = F) + 
    geom_smooth(method = "glm", method.args = list(family = "binomial"), 
      show.legend = F) + 
    scale_color_brewer(name = NULL, palette = "Set2") + 
    scale_y_continuous(breaks = seq(0, 1, 0.1)) + 
    coord_cartesian(ylim = c(0.48, 1.0)) + 
    labs(y = "Proportion correct", x = "Empathy quotient", 
    title = "Proportion correct as a function of EQ and utterance type") +  
    ds4ling::ds4ling_bw_theme(base_size = 13, base_family = "Times") + 
    theme(legend.key = element_rect(size = 1, fill = "white", colour = "white"), 
      legend.key.size = unit(0.9, "cm"))



ggsave(
  filename = "learner_accuracy_by_speaker_variety.pdf", 
  plot = learner_accuracy_by_speaker_variety, 
  path = here("figs", "poster"), 
  width = 7, 
  height = 4, 
  units = "in", 
  dpi = 300
  )

ggsave(
  filename = "learner_accuracy_by_utterance_type.pdf", 
  plot = learner_accuracy_by_utterance_type, 
  path = here("figs", "poster"), 
  width = 7, 
  height = 4, 
  units = "in", 
  dpi = 300
  )

ggsave(
  filename = "learner_accuracy_by_lextale.pdf", 
  plot = learner_accuracy_by_lextale, 
  path = here("figs", "poster"), 
  width = 7, 
  height = 6, 
  units = "in", 
  dpi = 300
  )

ggsave(
  filename = "learner_accuracy_by_st_eq.pdf", 
  plot = learner_accuracy_by_st_eq, 
  path = here("figs", "poster"), 
  width = 7, 
  height = 6, 
  units = "in", 
  dpi = 300
  )
