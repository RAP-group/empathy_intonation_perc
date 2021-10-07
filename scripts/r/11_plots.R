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




























# Accuracy by utterance type --------------------------------------------------

s_types <- c(
  "b_Intercept", 
  "b_sentence_typeinterrogativeMpartialMwh", 
  "b_sentence_typedeclarativeMnarrowMfocus", 
  "b_sentence_typedeclarativeMbroadMfocus"
  )

learner_accuracy_by_utterance_type <- learner_response_01 %>% 
  as_tibble() %>% 
  select(b_Intercept, starts_with("b_sentence_type")) %>% 
  transmute(
    `Interrogative\ny/n` = b_Intercept, 
    `Interrogative\nWh-` = b_Intercept + b_sentence_typeinterrogativeMpartialMwh, 
    `Declarative\nNarrow focus` = b_Intercept + b_sentence_typedeclarativeMnarrowMfocus, 
    `Declarative\nBroad focus`  = b_Intercept + b_sentence_typedeclarativeMbroadMfocus
  ) %>% 
  pivot_longer(cols = everything(), names_to = "parameter", values_to = "estimate") %>% 
  ggplot(., aes(x = parameter, y = estimate)) + 
    coord_cartesian(ylim = c(-1, NA)) + 
    geom_hline(yintercept = 0, lty = 3) + 
    stat_slab(fill = "#cc0033", alpha = 0.87, 
      aes(fill_ramp = stat(cut_cdf_qi(cdf, .width = c(.5, .65, .8, 1.0), 
        labels = scales::percent_format())))) + 
    stat_pointinterval(pch = 21, fill = "white", point_size = 2.2, 
      .width = c(0.95, 0.65)) +
    ggdist::scale_fill_ramp_discrete(range = c(1, 0.3), na.translate = F) +
    labs(y = "Log odds", x = NULL, 
      title = "Response accuracy", 
      subtitle = "Probability of a correct response for each utterance type in the logistic space") + 
    ds4ling::ds4ling_bw_theme(base_size = 13, base_family = "Times") + 
    theme(legend.position = c(0.5, 0.1), legend.direction = "horizontal", 
      legend.title = element_text(size = 8, color = "grey45"), 
      legend.spacing.x = unit(0,"cm"), legend.background = element_blank(), 
      strip.background = element_rect(fill = NA)) + 
    guides(fill_ramp = guide_legend(keywidth = 0.5, keyheight = 0.1, 
      default.unit = "inch", title.hjust = 0.5, reverse = T, 
      title = "of participant accuracy falls in this range",
      label.position = "bottom", title.position = "bottom")) 

ggsave(
  filename = "learner_accuracy_by_utterance_type.png", 
  plot = learner_accuracy_by_utterance_type, 
  path = here("figs", "slides"), 
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
    geom_vline(xintercept = 0, lty = 3, color = "black") +
    stat_halfeye(position = position_nudge(y = 0.04), slab_alpha = 0.2, 
      interval_alpha = 1, pch = 21, fill = "darkred", height = 0.8) + 
    expand_limits(y = c(-0.25, 9.25)) + 
    geom_text(data = tibble(x = -0.1, y = 0.05, text = sprintf("less accurate")), 
      aes(x, y, label = text), color = "black", size = 3, hjust = 1, family = "Times") + 
    geom_text(data = tibble(x = 0.1, y = 0.05, text = "more accurate"), 
      aes(x, y, label = text), color = "black", size = 3, hjust = 0, family = "Times") + 
    geom_curve(aes(x = -0.55, xend = -0.65, y = 0.025, yend = 0.03), size = 0.3, 
      curvature = 0, color = "black", arrow = arrow(length = unit(0.15, "cm"))) + 
    geom_curve(aes(x = 0.59, xend = 0.69, y = 0.025, yend = 0.03), size = 0.3, 
      curvature = 0, color = "black", arrow = arrow(length = unit(0.15, "cm"))) + 
    labs(x = "Log odds", y = NULL, 
      title = "Response accuracy", 
      subtitle = "Partially pooled estimates for each speaker variety") + 
    ds4ling::ds4ling_bw_theme(base_size = 13, base_family = "Times") 

ggsave(
  filename = "learner_accuracy_by_speaker_variety.png", 
  plot = learner_accuracy_by_speaker_variety, 
  path = here("figs", "slides"), 
  width = 7, 
  height = 4, 
  units = "in", 
  dpi = 300
  )


# Accuracy by lextale ---------------------------------------------------------

lt_me <- conditional_effects(learner_response_01_noslope, 
  effects = "lextale_std", 
  re_formula = NA, 
  method = "posterior_linpred", 
  spaghetti = TRUE, 
  nsamples = 300, 
  #int_conditions = list(lextale_std = c(-1.5, 0, 2, 4))
  )

learner_accuracy_by_lextale <- plot(lt_me, plot = F, 
  line_args = list(size = 3), 
  spaghetti_args = list(colour = alpha("#440154FF", 0.1)))[[1]] + 
  coord_cartesian(ylim = c(-1, 6), expand = F) + 
  scale_x_continuous(breaks = seq(-1, 4)) + 
  geom_hline(yintercept = 0, lty = 3) + 
  labs(y = "Log odds", x = "LexTALE score") + 
  annotate("text", x = -1, y = 5.75, label = "(A)", family = "Times") +
  ds4ling::ds4ling_bw_theme(base_size = 12, base_family = "Times")

# Get interaction with utterance type
lt_st_me <- conditional_effects(learner_response_01, 
  effects = "lextale_std:sentence_type", 
  re_formula = NA, 
  method = "posterior_linpred", 
  spaghetti = TRUE, 
  nsamples = 300, 
  #int_conditions = list(lextale_std = c(-1.5, 0, 2, 4))
  )

sentence_labs <- c(
  "Interrogative\ny/n", 
  "Interrogative\nWh-", 
  "Declarative\nnarrow focus", 
  "Declarative\nbroad focus")

utterance_colors <- c(
  "#440154"
)

learner_accuracy_lt_by_st <- plot(lt_st_me, plot = F, 
  line_args = list(size = 3))[[1]] + 
  coord_cartesian(ylim = c(-1, 6), expand = F) + 
  scale_y_continuous(position = "right") + 
  geom_hline(yintercept = 0, lty = 3) + 
  scale_color_manual(
    name = NULL, 
    values = alpha(viridis::viridis_pal()(4), 0.1), 
    labels = sentence_labs) + 
  labs(y = NULL, x = "LexTALE score") + 
  annotate("text", x = -1, y = 5.75, label = "(B)", family = "Times") +
  ds4ling::ds4ling_bw_theme(base_size = 12, base_family = "Times") + 
  theme(
    legend.background = element_blank(), 
    legend.position = c(0.5, 0.06), 
    legend.direction = "horizontal", 
    legend.key.size = unit(0.7, "cm"), 
    legend.text.align = 0.5) + 
  guides(color = guide_legend(override.aes = list(fill = NA, size = 2)))

lt_st_combine <- learner_accuracy_by_lextale + learner_accuracy_lt_by_st + 
  plot_annotation(
  title = 'Response accuracy',
  subtitle = "Probability of a correct response in the logistic space as a function of LexTALE score (A) and LexTALE score for each utterance type (B)",
  caption = "Colored lines represent 300 draws from the posterior distribution"
)

ggsave(
  filename = "lextale_utterance_type_combine.png", 
  plot = lt_st_combine, 
  path = here("figs", "slides"), 
  width = 11, 
  height = 5, 
  units = "in", 
  dpi = 300
  )

# -----------------------------------------------------------------------------







# Accuracy by utterance type and empathy --------------------------------------
eq_me <- conditional_effects(learner_response_01_noslope, 
  effects = "eq_std", 
  re_formula = NA, 
  method = "posterior_linpred", 
  spaghetti = TRUE, 
  nsamples = 500, 
  #int_conditions = list(lextale_std = c(-1.5, 0, 2, 4))
  )

learner_accuracy_by_eq <- plot(eq_me, plot = F, 
  line_args = list(size = 5, fill = 'black'), 
  spaghetti_args = list(colour = alpha("#440154FF", 0.1)))[[1]] + 
  coord_cartesian(ylim = c(-1, 4.5), expand = F) + 
  geom_hline(yintercept = 0, lty = 3) + 
  labs(y = "Log odds", x = "Empathy quotient") + 
  annotate("text", x = -2, y = 4.25, label = "(A)", family = "Times") +
  ds4ling::ds4ling_bw_theme(base_size = 12, base_family = "Times")

eq_st_me <- conditional_effects(learner_response_01, 
  effects = "eq_std:sentence_type", 
  re_formula = NA, 
  method = "posterior_linpred", 
  spaghetti = TRUE, 
  nsamples = 300)

learner_accuracy_eq_by_st <- plot(eq_st_me, plot = F, 
  line_args = list(size = 3))[[1]] + 
  coord_cartesian(ylim = c(-1, 4.5), expand = F) + 
  scale_y_continuous(position = "right") + 
  geom_hline(yintercept = 0, lty = 3) + 
  scale_color_manual(
    name = NULL, 
    values = alpha(viridis::viridis_pal()(4), 0.1), 
    labels = sentence_labs) + 
  labs(y = NULL, x = "Empathy quotient") + 
  annotate("text", x = -2, y = 4.25, label = "(B)", family = "Times") +
  ds4ling::ds4ling_bw_theme(base_size = 12, base_family = "Times") + 
  theme(
    legend.background = element_blank(), 
    legend.position = c(0.5, 0.09), 
    legend.direction = "horizontal", 
    legend.key.size = unit(0.7, "cm"), 
    legend.text.align = 0.5) + 
  guides(color = guide_legend(override.aes = list(fill = NA, size = 2)))

# Combine with accuracy by lextale for abstracts
empathy_st_combine <- learner_accuracy_by_eq + learner_accuracy_eq_by_st + 
  plot_annotation(
  title = 'Response accuracy',
  subtitle = "Probability of a correct response in the logistic space as a function of (A) empathy quotient and (B) emapthy quotient for each utterance type",
  caption = "Colored lines represent 300 draws from the posterior distribution"
)

ggsave(
  filename = "empathy_utterance_type_combine.png", 
  plot = empathy_st_combine, 
  path = here("figs", "slides"), 
  width = 11, 
  height = 5, 
  units = "in", 
  dpi = 300
  )

# -----------------------------------------------------------------------------








conditions <- data.frame(
  q_type = setNames(c(1, -1), c("Interrogative\ny/n", "Interrogative\nWh-"))
)

int_conditions <- list(
  eq_std = setNames(c(-2, -1, 0, 1, 2), 
    c("-2", "-1", "0", "1", "2"))
)

lt_eq_3way <- conditional_effects(learner_response_qonly_01, 
  effects = "lextale_std:eq_std", 
  re_formula = NA, 
  method = "posterior_epred", 
  spaghetti = TRUE, 
  nsamples = 300, 
  ncol = 2, 
  conditions = conditions, 
  int_conditions = int_conditions
  )

lt_eq_3way_ce <- plot(lt_eq_3way, plot = F, 
  line_args = list(size = 1.5))[[1]] + 
  coord_cartesian(xlim = c(-1, 4.2), ylim = c(0.3, 1), expand = F) + 
  geom_hline(yintercept = 0.5, lty = 3) + 
  scale_color_manual(
    name = "Empathy\nquotient", 
    values = alpha(viridis::viridis_pal(option = "B", end = 0.8)(5), 0.1)) + 
  labs(y = "P(correct)", x = "LexTALE score", 
    title = "Response accuracy", 
    subtitle = "Probability of a correct response as a function of empathy quotient and LexTALE score for each question type") + 
  ds4ling::ds4ling_bw_theme(base_size = 13, base_family = "Times") + 
  theme(
    legend.position = "bottom", 
    legend.background = element_blank(), 
    legend.key.size = unit(0.7, "cm"), 
    legend.text.align = 0.5, 
    legend.title = element_text(size = 10, color = "grey45"), 
    legend.spacing.x = unit(0,"cm"), 
    strip.background = element_rect(fill = NA)) + 
  guides(color = guide_legend(keywidth = 0.5, keyheight = 0.1, 
      default.unit = "inch", title.hjust = 0.5, reverse = T, 
      title = "Empathy quotient",
      label.position = "bottom", title.position = "bottom", 
      override.aes = list(fill = NA, size = 3))) 

ggsave(
  filename = "lt_eq_3way_ce.png", 
  plot = lt_eq_3way_ce, 
  path = here("figs", "slides"), 
  width = 11, 
  height = 6, 
  units = "in", 
  dpi = 300
  )








# Speech rate by variety
plot_speech_rate <- mono_speech_rates_df %>% 
  select(speaker_variety, metric, val) %>% 
  filter(metric == "articulation_rate") %>% 
  mutate(
    speaker_variety = case_when(
      speaker_variety == "andalusian" ~ "Andalusian", 
      speaker_variety == "argentine" ~ "Argentine", 
      speaker_variety == "castilian" ~ "Castilian", 
      speaker_variety == "chilean" ~ "Chilean", 
      speaker_variety == "cuban" ~ "Cuban", 
      speaker_variety == "mexican" ~ "Mexican", 
      speaker_variety == "peruvian" ~ "Peruvian", 
      speaker_variety == "puertorican" ~ "Puerto Rican"
  ), 
    speaker_variety = fct_reorder(speaker_variety, val, .fun = mean), 
    val = (val - mean(val)) / sd(val)) %>% 
  ggplot(., aes(x = val, y = speaker_variety)) + 
    geom_vline(xintercept = 0, lty = 3) +
    stat_halfeye(position = position_nudge(y = 0.04), slab_alpha = 0.2, 
      interval_alpha = 1, pch = 21, fill = "darkred", height = 0.8) + 
    expand_limits(y = c(-0.25, 9.25)) + 
    geom_text(aes(x, y, label = text), data = tibble(x = -0.2, y = 0.05,
      text = sprintf("slower")), color = "black", size = 3, hjust = 1, family = "Times") + 
    geom_text(aes(x, y, label = text), data = tibble(x = 0.2, y = 0.05,
      text = "faster"), color = "black", size = 3, hjust = 0, family = "Times") +
    geom_curve(aes(x = -0.7, xend = -1, y = 0.03, yend = 0.03), 
      size = 0.3, color = "black", curvature = 0, arrow = arrow(length = unit(0.15, "cm"))) +
    geom_curve(aes(x = 0.7, xend = 1, y = 0.03, yend = 0.03),
      size = 0.3, color = "black", curvature = 0, arrow = arrow(length = unit(0.15, "cm"))) +
    labs(y = "Speaker variety", x = "z-Articulation rate") + 
    ds4ling::ds4ling_bw_theme(base_size = 13, base_family = "Times") + 
    NULL

ggsave(
  filename = "speech_rate.png", 
  plot = plot_speech_rate, 
  path = here("figs", "slides"), 
  width = 7, 
  height = 4, 
  units = "in", 
  dpi = 300
  )







learners %>% 
  select(participant, lextale_tra) %>% 
  distinct() %>% 
  ggplot(., aes(x = lextale_tra)) + 
    geom_histogram(fill = "grey", color = "black", 
      binwidth = 3.54)

learners %>% 
  select(participant, eq_score) %>% 
  distinct() %>% 
  ggplot(., aes(x = eq_score)) + 
    geom_histogram(fill = "grey", color = "black", 
      binwidth = 3.54)

random_speaker_check <- learners %>% 
  select(participant, speaker_variety) %>% 
  group_by(participant, speaker_variety) %>% 
  tally() %>% 
  ungroup() %>% 
  mutate(prop = n / 8, 
         speaker_variety = tools::toTitleCase(speaker_variety)) %>% 
  ggplot(., aes(x = speaker_variety, y = n)) + 
    geom_hline(yintercept = 8, lty = 3, color = "black") + 
    geom_violin(alpha = 0.3) + 
    stat_summary(fun.data = mean_sdl, geom = "pointrange", pch = 21, 
      color = "black", fill = "white", size = 1, fun.args = list(mult = 1)) + 
    coord_cartesian(ylim = c(-5, 20))  + 
    labs(y = "Mean", x = "Speaker variety", 
      title = "Average stimuli tokens from each variety.", 
      subtitle = glue("(n = {n_participant})"), 
      caption = "Mean +/- SD") + 
    ds4ling::ds4ling_bw_theme(base_size = 13, base_family = "Times") + 
    NULL


ggsave(
  filename = "random_speaker_check.png", 
  plot = random_speaker_check, 
  path = here("figs", "slides"), 
  width = 9, 
  height = 5, 
  units = "in", 
  dpi = 300
  )
