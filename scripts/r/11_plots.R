# Source helpers and libs -----------------------------------------------------

source(here::here("scripts", "r", "07_load_data.R"))

# -----------------------------------------------------------------------------


# Model stuff


post_samples <- posterior_samples(native_response_01) %>% 
  rename(intercept = b_Intercept, 
         participant_sd = sd_participant__Intercept, 
         sentence_type_sd = sd_sentence_type__Intercept, 
         speaker_variety_sd = sd_speaker_variety__Intercept, 
         speaker_variety_sentence_type_sd = `sd_speaker_variety:sentence_type__Intercept`)

# Grouping variable posteriors
posterior_samples(all_mods$native_response_01) %>% 
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



























# ARESTY POSTER IMG

# Accuracy by utterance type --------------------------------------------------

s_types <- c(
  "b_Intercept", 
  "b_sentence_typeinterrogativeMpartialMwh", 
  "b_sentence_typedeclarativeMnarrowMfocus", 
  "b_sentence_typedeclarativeMbroadMfocus"
  )

plot <- learner_response_01 %>% 
  as_tibble() %>% 
  select(b_Intercept, starts_with("b_sentence_type")) %>% 
  transmute(
    `Interrogative\ny/n` = b_Intercept, 
    `Interrogative\nWh-` = b_Intercept + b_sentence_typeinterrogativeMpartialMwh, 
    `Declarative\nNarrow focus` = b_Intercept + b_sentence_typedeclarativeMnarrowMfocus, 
    `Declarative\nBroad focus`  = b_Intercept + b_sentence_typedeclarativeMbroadMfocus
  ) %>% 
  pivot_longer(cols = everything(), names_to = "parameter", values_to = "estimate") %>% 
  mutate(prob = plogis(estimate)) %>% 
  ggplot(., aes(x = parameter, y = prob)) + 
    stat_interval(.width = c(0.1, 0.25, 0.5, 0.75, 1.0), height = 5, show.legend = F) +
    stat_halfeye(position = position_nudge(x = 0.04), slab_alpha = 0.3, 
      interval_alpha = 0, point_size = 0.75, fill = "tan", width = 0.5) + 
    rcartocolor::scale_color_carto_d(palette = "Peach") + 
    coord_cartesian(ylim = c(0.4, 1.00)) + 
    labs(y = "Proportion correct", x = NULL, 
      title = "Response accuracy", 
      subtitle = "Range and distribution of correct responses for each utterance type") + 
    ds4ling::ds4ling_bw_theme(base_size = 13, base_family = "Times") + 
    theme(panel.grid.minor = element_blank(),
          panel.grid.major.x = element_blank(),
          panel.grid.major.y = element_line(size = 0.1),
          axis.text.x = element_text(size = 11, face = "bold"),
          axis.text.y = element_text(size = 9, color = "grey65"))

legend_text <- 
  tibble(
    xt = c(5, 4.125, 3.125, 1.875, 0.625, 7.95), 
    yt = rep(1.02, 6), 
    text = c("10%", "25%", "50%", "75%", "100%", 
    "of participant accuracy falls in this range"))

legend <- ggplot(data = tibble(x = 0:10, y = rep(1, 11)), aes(x, y)) + 
  stat_interval(.width = c(0.1, 0.25, 0.5, 0.75, 1.0), show.legend = F) + 
  rcartocolor::scale_color_carto_d(palette = "Peach") + 
  coord_cartesian(ylim = c(0.9, 1.1)) + 
  geom_text(data = legend_text, aes(xt, yt, label = text), 
    family = "Times", color = "grey65", size = 2.75) + 
  theme_void()

plot_ins <- cowplot::ggdraw(plot) + 
  cowplot::draw_plot(legend, x = .23, y = -0.01, width = .6, height = .35)

learner_accuracy_by_utterance_type <- plot_ins + plot_layout(widths = 1)

ggsave(
  filename = "learner_accuracy_by_utterance_type.pdf", 
  plot = learner_accuracy_by_utterance_type, 
  path = here("figs", "poster"), 
  width = 7, 
  height = 4, 
  units = "in", 
  dpi = 300
  )


# Accuracy by speaker variety -------------------------------------------------

learner_accuracy_by_speaker_variety <- learner_response_01 %>% 
  as_tibble() %>% 
  select("b_Intercept", 
    starts_with("r_speaker_variety[") & contains(",Intercept")) %>% 
  transmute(
    Andalusian    = `r_speaker_variety[andalusian,Intercept]`, 
    Argentine     = `r_speaker_variety[argentine,Intercept]`, 
    Castilian     = `r_speaker_variety[castilian,Intercept]`, 
    Chilean       = `r_speaker_variety[chilean,Intercept]`, 
    Cuban         = `r_speaker_variety[cuban,Intercept]`, 
    Mexican       = `r_speaker_variety[mexican,Intercept]`, 
    Peruvian      = `r_speaker_variety[peruvian,Intercept]`, 
   `Puerto Rican` = `r_speaker_variety[puertorican,Intercept]`
  ) %>% 
  pivot_longer(everything(), names_to = "parameter", values_to = "estimate") %>% 
  mutate(parameter = fct_reorder(parameter, estimate, min)) %>% 
  ggplot(., aes(x = estimate, y = parameter)) + 
    geom_vline(xintercept = 0, lty = 3) +
    stat_halfeye(position = position_nudge(y = 0.04), slab_alpha = 0.2, 
      interval_alpha = 1, pch = 21, fill = "darkred", height = 0.8) + 
    expand_limits(y = c(-0.25, 9.25)) + 
    geom_text(data = tibble(x = -0.1, y = 0.05, text = sprintf("less accurate")), 
      aes(x, y, label = text), size = 3, hjust = 1, family = "Times") + 
    geom_text(data = tibble(x = 0.1, y = 0.05, text = "more accurate"), 
      aes(x, y, label = text), size = 3, hjust = 0, family = "Times") + 
    geom_curve(aes(x = -0.55, xend = -0.65, y = 0.025, yend = 0.03), size = 0.3, 
      curvature = 0, arrow = arrow(length = unit(0.15, "cm"))) + 
    geom_curve(aes(x = 0.59, xend = 0.69, y = 0.025, yend = 0.03), size = 0.3, 
      curvature = 0, arrow = arrow(length = unit(0.15, "cm"))) + 
    labs(x = "Log odds", y = NULL, 
      title = "Response accuracy", 
      subtitle = "Partially pooled estimates of accurate responses for each speaker variety") + 
    ds4ling::ds4ling_bw_theme(base_size = 13, base_family = "Times") 

ggsave(
  filename = "learner_accuracy_by_speaker_variety.pdf", 
  plot = learner_accuracy_by_speaker_variety, 
  path = here("figs", "poster"), 
  width = 7, 
  height = 4, 
  units = "in", 
  dpi = 300
  )


# Accuracy by lextale ---------------------------------------------------------

lt_me <- conditional_effects(learner_response_01, 
  effects = "lextale_std", 
  re_formula = NA, 
  method = "posterior_epred", 
  spaghetti = TRUE, 
  nsamples = 300)



learner_accuracy_by_lextale <- plot(lt_me, plot = F, 
  line_args = list(size = 3), 
  spaghetti_args = list(colour = alpha("#440154FF", 0.1)))[[1]] + 
  coord_cartesian(ylim = c(0.4, 1), expand = F) + 
  geom_hline(yintercept = 0.5, lty = 3) + 
  scale_y_continuous(position = "right") + 
  labs(y = NULL, x = "LexTALE score", 
  title = "Proportion correct as a function of LexTALE score", 
  subtitle = "300 draws from the posterior distribution") + 
  annotate("text", x = -1, y = 0.97, label = "(B)", family = "Times") +
  ds4ling::ds4ling_bw_theme(base_size = 12, base_family = "Times")

ggsave(
  filename = "learner_accuracy_by_lextale.pdf", 
  plot = learner_accuracy_by_lextale, 
  path = here("figs", "poster"), 
  width = 7, 
  height = 6, 
  units = "in", 
  dpi = 300
  )



# Accuracy by utterance type and empathy --------------------------------------

eq_st_me <- conditional_effects(learner_response_01, 
  effects = "eq_std:sentence_type", 
  re_formula = NA, 
  method = "posterior_epred", 
  spaghetti = TRUE, 
  nsamples = 300)

sentence_labs <- c(
  "Interrogative\ny/n", 
  "Interrogative\nWh-", 
  "Declarative\nnarrow focus", 
  "Declarative\nbroad focus")

utterance_colors <- c(
  "#440154"
)


learner_accuracy_eq_by_st <- plot(eq_st_me, plot = F, 
  line_args = list(size = 3))[[1]] + 
  coord_cartesian(ylim = c(0.4, 1), expand = F) + 
  geom_hline(yintercept = 0.5, lty = 3) + 
  scale_color_manual(
    name = NULL, 
    values = alpha(viridis_pal()(4), 0.1), 
    labels = sentence_labs) + 
  labs(y = "Proportion correct", x = "Empathy quotient", 
  title = "Proportion correct as a function of EQ and utterance type", 
  subtitle = "300 draws from the posterior distribution") + 
  annotate("text", x = -2, y = 0.98, label = "(A)", family = "Times") +
  ds4ling::ds4ling_bw_theme(base_size = 12, base_family = "Times") + 
  theme(
    legend.position = c(0.5, 0.07), 
    legend.direction = "horizontal", 
    legend.key.size = unit(0.7, "cm"), 
    legend.text.align = 0.5) + 
  guides(color = guide_legend(override.aes = list(fill = NA, size = 2)))

# Combine with accuracy by lextale for abstracts
plot_abstract_hls <- learner_accuracy_eq_by_st + learner_accuracy_by_lextale 

ggsave(
  filename = "plot_abstract_hls.png", 
  plot = plot_abstract_hls, 
  path = here("figs", "abstract"), 
  width = 11, 
  height = 5, 
  units = "in", 
  dpi = 300
  )

