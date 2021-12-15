# Plots -----------------------------------------------------------------------
#
# - All plots presented in slides and manuscripts are generated from this file
# - Default output is to png and pdf
#
# -----------------------------------------------------------------------------




# Source helpers, libs, and models --------------------------------------------

source(here::here("scripts", "r", "07_load_data.R"))

learner_response_01 <- readRDS(here("models", "learner_response_01.rds"))
learner_response_q_01 <- readRDS(here("models", "learner_response_q_01.rds"))
learner_response_qonly_01 <- readRDS(here("models", "learner_response_qonly_01.rds"))
learner_response_yn_01 <- readRDS(here("models", "learner_response_yn_01.rds"))

# -----------------------------------------------------------------------------




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
      #title = "Response accuracy", 
      #subtitle = "Probability of a correct response for each utterance type in the logistic space"
      ) + 
    ds4ling::ds4ling_bw_theme(base_size = 13, base_family = "Times") + 
    theme(legend.position = c(0.5, 0.1), legend.direction = "horizontal", 
      legend.title = element_text(size = 8, color = "grey45"), 
      legend.spacing.x = unit(0,"cm"), legend.background = element_blank(), 
      strip.background = element_rect(fill = NA)) + 
    guides(fill_ramp = guide_legend(keywidth = 0.5, keyheight = 0.1, 
      default.unit = "inch", title.hjust = 0.5, reverse = T, 
      title = "of participant accuracy falls in this range",
      label.position = "bottom", title.position = "bottom")) 

# -----------------------------------------------------------------------------




# Accuracy by speaker variety -------------------------------------------------

learner_accuracy_by_speaker_variety <- learner_response_01 %>% 
  as_tibble() %>% 
  select("b_Intercept", 
    starts_with("r_speaker_variety[") & contains(",Intercept")) %>% 
  transmute(
    Andalusian    = `r_speaker_variety[andalusian,Intercept]`, 
    Argentine     = `r_speaker_variety[argentine,Intercept]`, 
    Peninsular    = `r_speaker_variety[castilian,Intercept]`, 
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
      #title = "Response accuracy", 
      #subtitle = "Partially pooled estimates for each speaker variety"
      ) + 
    ds4ling::ds4ling_bw_theme(base_size = 13, base_family = "Times") 

# -----------------------------------------------------------------------------




# Accuracy by lextale ---------------------------------------------------------

# Get interaction with utterance type
lt_st_me <- conditional_effects(learner_response_01, 
  effects = "lextale_std:sentence_type", 
  re_formula = NA, 
  method = "posterior_linpred", 
  spaghetti = TRUE, 
  ndraws = 300, 
  #int_conditions = list(lextale_std = c(-1.5, 0, 2, 4))
  )[[1]]

# Set labs for plot
sentence_labs <- c(
  "Interrogative\ny/n", 
  "Interrogative\nWh-", 
  "Declarative\nnarrow focus", 
  "Declarative\nbroad focus")

# Pick some colorsf
utterance_colors <- c(
  "#440154"
)

# Main plot
learner_accuracy_lextale_by_utterance_type <- plot(lt_st_me, plot = F, 
  line_args = list(size = 5))[[1]] + 
  geom_line(aes(group = effect2__), size = 2, 
    color = rep(viridis::viridis_pal(option = "A", end = 0.85)(4), each = 100)) +
  coord_cartesian(ylim = c(-1, 6), expand = F) + 
  geom_hline(yintercept = 0, lty = 3) + 
  scale_color_manual(name = NULL, labels = sentence_labs, 
    values = alpha(viridis::viridis_pal(option = "A", end = 0.9)(4), 0.1)) + 
  labs(y = "Log odds", x = "LexTALE score") + 
  ds4ling::ds4ling_bw_theme(base_size = 12, base_family = "Times") + 
  theme(
    legend.background = element_blank(), 
    legend.position = c(0.5, 0.06), 
    legend.direction = "horizontal", 
    legend.key.size = unit(0.7, "cm"), 
    legend.text.align = 0.5) + 
  guides(color = guide_legend(override.aes = list(fill = NA, size = 2)))

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
plot_speech_rate <- sr %>% 
  pivot_longer(cols = c("speech_rate", "articulation_rate", "avg_syll_dur"), 
  names_to = "metric", values_to = "val") %>% 
  select(speaker_variety, metric, val) %>% 
  filter(metric == "articulation_rate") %>% 
  mutate(
    speaker_variety = case_when(
      speaker_variety == "andalusian" ~ "Andalusian", 
      speaker_variety == "argentine" ~ "Argentine", 
      speaker_variety == "castilian" ~ "Peninsular", 
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








# Save plots ------------------------------------------------------------------

devices     <- c('png', 'pdf')
path_to_fig <- file.path(here("figs", "manuscript"))

walk(devices, ~ ggsave(
  filename = glue(path_to_fig, "/learner_accuracy_by_utterance_type.", .x), 
  plot = learner_accuracy_by_utterance_type, 
  device = .x, height = 4, width = 7, units = "in"))

walk(devices, ~ ggsave(
  filename = glue(path_to_fig, "/learner_accuracy_by_speaker_variety.", .x), 
  plot = learner_accuracy_by_speaker_variety, 
  device = .x, height = 4, width = 7, units = "in"))

walk(devices, ~ ggsave(
  filename = glue(path_to_fig, "/learner_accuracy_lextale_by_utterance_type.", .x), 
  plot = learner_accuracy_lextale_by_utterance_type, 
  device = .x, height = 4, width = 7, units = "in"))






# -----------------------------------------------------------------------------
