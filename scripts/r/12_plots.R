# Plots -----------------------------------------------------------------------
#
# Author: Joseph V. Casillas
# Last update: 20211218
#
# - All plots presented in slides and manuscripts are generated from this file
#   and saved in their own folder, i.e., figs/slides, figs/manuscript, etc.
# - Default output is to png and pdf
#
# -----------------------------------------------------------------------------




# Source helpers, libs, and models --------------------------------------------

source(here::here("scripts", "r", "07_load_data.R"))

learner_response_01 <- readRDS(here("models", "learner_response_01.rds"))
learner_response_q_01 <- readRDS(here("models", "learner_response_q_01.rds"))
learner_response_qonly_01 <- readRDS(here("models", "learner_response_qonly_01.rds"))
learner_response_yn_01 <- readRDS(here("models", "learner_response_yn_01.rds"))
ddm_boundary_separation <- readRDS(here("models", "ddm_boundary_separation.rds"))
ddm_drift_rate <- readRDS(here("models", "ddm_drift_rate.rds"))

# -----------------------------------------------------------------------------




# Empathy quotient and LexTALE descriptives -----------------------------------

lt_mod <- brm(
  formula = lextale_tra ~ 1, 
  data = learners %>% select(participant, lextale_tra) %>% 
    group_by(participant, lextale_tra) %>% distinct(),
  prior = prior(normal(10, 20), class = "Intercept")
)

filter(learners, participant == "midd01")

 learners %>% 
  select(participant, lextale_tra, eq_score) %>% 
  group_by(participant, lextale_tra, eq_score) %>% 
  distinct() %>% 
  pivot_longer(-participant, names_to = "metric", values_to = "val") %>% 
  ggplot(., aes(x = val, fill = metric)) + 
    geom_histogram(color = "black", alpha = 0.5, binwidth = fd_bw) + 
    scale_fill_viridis_d(option = "B", begin = 0.2, end = 0.8) + 
    minimal_adj() + 
    theme(legend.position = c(0.8, 0.8))

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
  mutate(estimate = plogis(estimate)) %>% 
  ggplot(., aes(x = parameter, y = estimate)) + 
    coord_cartesian(ylim = c(NA, 1)) + 
    stat_slab(fill = "#cc0033", alpha = 0.87, 
      aes(fill_ramp = stat(cut_cdf_qi(cdf, .width = c(.5, .65, .8, 1.0), 
        labels = scales::percent_format())))) + 
    stat_pointinterval(pch = 21, fill = "white", point_size = 2.2, 
      .width = c(0.95, 0.65)) +
    ggdist::scale_fill_ramp_discrete(range = c(1, 0.3), na.translate = F) +
    labs(y = "P(correct)", x = NULL) + 
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
    geom_text(data = tibble(x = -0.1, y = 0.05, text = sprintf("-accurate")), 
      aes(x, y, label = text), color = "black", size = 3, hjust = 1, family = "Times") + 
    geom_text(data = tibble(x = 0.1, y = 0.05, text = "+accurate"), 
      aes(x, y, label = text), color = "black", size = 3, hjust = 0, family = "Times") + 
    geom_curve(aes(x = -0.69, xend = -0.99, y = 0.025, yend = 0.03), size = 0.3, 
      curvature = 0, color = "black", arrow = arrow(length = unit(0.15, "cm"))) + 
    geom_curve(aes(x = 0.69, xend = 0.99, y = 0.025, yend = 0.03), size = 0.3, 
      curvature = 0, color = "black", arrow = arrow(length = unit(0.15, "cm"))) + 
    labs(x = expression(paste(beta, "-Response")), y = NULL) + 
    ds4ling::ds4ling_bw_theme(base_size = 13, base_family = "Times") 

# -----------------------------------------------------------------------------




# RT by speaker variety -------------------------------------------------------

learner_rt_by_speaker_variety <- learner_rt_01 %>% 
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
  mutate(parameter = fct_relevel(parameter, "Cuban", "Puerto Rican", 
    "Argentine", "Peruvian", "Andalusian", "Chilean", "Mexican", 
    "Peninsular")) %>% 
  ggplot(., aes(x = estimate, y = parameter)) + 
    coord_cartesian(xlim = c(-0.12, 0.12)) + 
    scale_y_discrete(position = "right") + 
    geom_vline(xintercept = 0, lty = 3, color = "black") +
    stat_halfeye(position = position_nudge(y = 0.04), slab_alpha = 0.2, 
      interval_alpha = 1, pch = 21, fill = "darkred", height = 0.8) + 
    expand_limits(y = c(-0.25, 9.25)) + 
    geom_text(data = tibble(x = -0.01, y = 0.05, text = sprintf("faster")), 
      aes(x, y, label = text), color = "black", size = 3, hjust = 1, family = "Times") + 
    geom_text(data = tibble(x = 0.01, y = 0.05, text = "slower"), 
      aes(x, y, label = text), color = "black", size = 3, hjust = 0, family = "Times") + 
    geom_curve(aes(x = -0.04, xend = -0.06, y = 0.025, yend = 0.03), size = 0.3, 
      curvature = 0, color = "black", arrow = arrow(length = unit(0.15, "cm"))) + 
    geom_curve(aes(x = 0.04, xend = 0.06, y = 0.025, yend = 0.03), size = 0.3, 
      curvature = 0, color = "black", arrow = arrow(length = unit(0.15, "cm"))) + 
    labs(x = expression(paste(beta, "-Response time")), y = NULL) + 
    ds4ling::ds4ling_bw_theme(base_size = 13, base_family = "Times") 

learner_accuracy_rt_by_speaker_variety <- 
  learner_accuracy_by_speaker_variety + learner_rt_by_speaker_variety

# -----------------------------------------------------------------------------




# Accuracy by utterance type and lextale --------------------------------------

# Get interaction with utterance type
lt_st_me <- conditional_effects(learner_response_01, 
  effects = "lextale_std:sentence_type", 
  re_formula = NA, 
  method = "posterior_epred", 
  spaghetti = TRUE, 
  ndraws = 300, 
  int_conditions = list(lextale_std = c(-2.1, -1, 0, 1, 2.1))
  )

# Set labs for plot
sentence_labs <- c(
  "Interrogative\ny/n", 
  "Interrogative\nWh-", 
  "Declarative\nnarrow focus", 
  "Declarative\nbroad focus")

# Main plot
learner_accuracy_lextale_by_utterance_type <- plot(lt_st_me, plot = F, 
  line_args = list(size = 4))[[1]] + 
  scale_x_continuous(expand = c(0, 0)) + 
  coord_cartesian(ylim = c(0.39, 1.01)) + 
  geom_line(aes(group = effect2__), size = 1.5, 
    color = rep(viridis::viridis_pal(option = "A", end = 0.85)(4), each = 5)) +
  geom_hline(yintercept = 0.5, lty = 3) + 
  scale_color_manual(name = NULL, labels = sentence_labs, 
    values = alpha(viridis::viridis_pal(option = "A", end = 0.9)(4), 0.1)) + 
  labs(y = "P(correct)", x = "LexTALE score") + 
  ds4ling::ds4ling_bw_theme(base_size = 12, base_family = "Times") + 
  theme(
    legend.background = element_blank(), 
    legend.position = c(0.5, 0.08), 
    legend.direction = "horizontal", 
    legend.key.size = unit(0.7, "cm"), 
    legend.text.align = 0.5) + 
  guides(color = guide_legend(override.aes = list(fill = NA, size = 2)))

# -----------------------------------------------------------------------------




# Accuracy by utterance type and empathy --------------------------------------

# Get interaction with utterance type
eq_st_me <- conditional_effects(learner_response_01, 
  effects = "eq_std:sentence_type", 
  re_formula = NA, 
  method = "posterior_epred", 
  spaghetti = TRUE, 
  int_conditions = list(eq_std = c(-2.1, -1, 0, 1, 2.1)), 
  ndraws = 300)

learner_accuracy_empathy_by_utterance_type <- plot(eq_st_me, plot = F, 
  line_args = list(size = 4))[[1]] + 
  coord_cartesian(ylim = c(0.39, 1.01)) + 
  scale_x_continuous(expand = c(0, 0)) + 
  scale_y_continuous(position = "right") + 
  geom_line(aes(group = effect2__), size = 1.5, 
    color = rep(viridis::viridis_pal(option = "A", end = 0.85)(4), each = 5)) +
  geom_hline(yintercept = 0.5, lty = 3) + 
  scale_color_manual(name = NULL, labels = sentence_labs, 
    values = alpha(viridis::viridis_pal(option = "A", end = 0.9)(4), 0.1)) + 
  labs(y = NULL, x = "Empathy quotient") + 
  ds4ling::ds4ling_bw_theme(base_size = 12, base_family = "Times") + 
  theme(
    legend.background = element_blank(), 
    legend.position = c(0.5, 0.09), 
    legend.direction = "horizontal", 
    legend.key.size = unit(0.7, "cm"), 
    legend.text.align = 0.5) + 
  guides(color = guide_legend(override.aes = list(fill = NA, size = 2)))

learner_accuracy_lt_eq_comb <- learner_accuracy_lextale_by_utterance_type + 
  learner_accuracy_empathy_by_utterance_type & theme(legend.position = "bottom") 
learner_accuracy_lt_eq_by_utterance_type <- 
  learner_accuracy_lt_eq_comb + plot_layout(guides = "collect")

# -----------------------------------------------------------------------------




# Lextale x Empathy interaction for questions ---------------------------------

# Set conditions for facetting
conditions <- data.frame(
  q_type = setNames(c(1, -1), c("Interrogative\ny/n", "Interrogative\nWh-"))
)

# Get wide range of EQ estimates
int_conditions <- list(
  eq_std = setNames(c(-1, 0, 1), 
    c("-1", "0", "1"))
)

# Generate plot
lt_eq_3way <- conditional_effects(learner_response_qonly_01, 
  effects = "lextale_std:eq_std", 
  re_formula = NA, 
  method = "posterior_epred", 
  spaghetti = TRUE, 
  ndraws = 300, 
  ncol = 2, 
  conditions = conditions, 
  int_conditions = int_conditions
  )

learner_accuracy_3way <- plot(lt_eq_3way, plot = F, 
  line_args = list(size = 5))[[1]] + 
  geom_line(aes(group = effect2__), size = 2, 
    color = rep(viridis::viridis_pal(option = "B", end = 0.8)(3), each = 100, times = 2)) +
  coord_cartesian(xlim = c(-1, 4.2), ylim = c(0.3, 1)) + 
  scale_x_continuous(expand = c(0, 0)) + 
  geom_hline(yintercept = 0.5, lty = 3) + 
  scale_color_manual(
    name = "Empathy\nquotient", 
    values = alpha(viridis::viridis_pal(option = "B", end = 0.8)(3), 0.1)) + 
  labs(y = "P(correct)", x = "LexTALE score") + 
  ds4ling::ds4ling_bw_theme(base_size = 13, base_family = "Times") + 
  theme(
    legend.position = c(0.8, 0.15), 
    legend.background = element_blank(), 
    legend.direction = "horizontal", 
    legend.key.size = unit(0.7, "cm"), 
    legend.text.align = 0.5, 
    legend.title = element_text(size = 10, color = "grey45"), 
    legend.spacing.x = unit(0,"cm"), 
    strip.background = element_rect(fill = NA)) + 
  guides(color = guide_legend(keywidth = 0.5, keyheight = 0.1, 
      default.unit = "inch", title.hjust = 0.5, reverse = T, 
      title = "Empathy quotient",
      label.position = "bottom", title.position = "top", 
      override.aes = list(fill = NA, size = 2))) 

# -----------------------------------------------------------------------------




# DDM Boundary separation and drift rate --------------------------------------

ddm_bs_dr_estimates <- bind_rows(
  as_draws_df(ddm_drift_rate) %>% 
    select(starts_with("b_")) %>% 
    pivot_longer(everything(), names_to = "params", values_to = "estimate") %>% 
    mutate(effect = "dr"),
  as_draws_df(ddm_boundary_separation) %>% 
    select(starts_with("b_")) %>% 
    pivot_longer(everything(), names_to = "params", values_to = "estimate") %>% 
    mutate(effect = "bs")) %>% 
  mutate(labs = case_when(
    params == "b_Intercept" ~ "Intercept", 
    params == "b_q_sum" ~ "Question type", 
    params == "b_lextale_std" ~ "LexTALE", 
    params == "b_eq_std" ~ "EQ", 
    params == "b_q_sum:lextale_std" ~ "Question type x\nLexTALE", 
    params == "b_q_sum:eq_std" ~ "Question type x\nEQ", 
    params == "b_lextale_std:eq_std" ~ "LexTALE x EQ", 
    params == "b_q_sum:lextale_std:eq_std" ~ "Question type x\nLexTALE x EQ"), 
    labs = fct_relevel(labs, "Intercept", "Question type", "LexTALE", "EQ", 
      "Question type x\nLexTALE", "Question type x\nEQ"))%>% 
  ggplot(., aes(x = estimate, y = labs, shape = effect)) + 
    stat_halfeye(aes(fill = effect), point_fill = "white", point_size = 1.5, 
       slab_alpha = 0.7, position = position_dodge(0.5)) +
    scale_shape_manual(name = NULL, values = c(21, 24), 
      labels = c("Boundary shift", "Drift rate")) + 
    scale_fill_viridis_d(name = NULL, option = "B", begin = 0.2, end = 0.8, 
      labels = c("Boundary shift", "Drift rate")) + 
    scale_y_discrete(limits = rev, position = "left") + 
    labs(y = NULL, x = expression(beta)) + 
    ds4ling::ds4ling_bw_theme(base_size = 13, base_family = "Times") + 
    theme(legend.position = c(0.8, 0.2), legend.background = element_blank())

# -----------------------------------------------------------------------------




# DDM explanation -------------------------------------------------------------

# Generate skewed RT-like data
my_skewed_x <- fGarch::rsnorm(100, mean = 2.5, sd = 1.5, xi = 3.5)

# Correct response dist
inset_top <- tibble(x = my_skewed_x) %>%
  ggplot(., aes(x = x)) + 
    geom_density(fill = viridis::viridis_pal(option = "B", begin = 0.25)(1)) + 
    theme_void()

# Incorrect response dist
inset_bottom <- tibble(x = my_skewed_x) %>%
  ggplot(., aes(x = x)) + 
    geom_density(fill = viridis::viridis_pal(option = "B", begin = 0.75)(1)) + 
    scale_y_reverse() + 
    theme_void()

# Main plot
main_plot <- sim_ddm(q_type = "yn", eq = "low", lt = "low", seed = 20180119, 
  drift_rate = 0.11, boundary_separation = 1.25, bias = 0, ndt = 0, n_sims = 2) %>% 
  ggplot(., aes(x = step, y = value, color = sim_n)) + 
    geom_vline(xintercept = 1) + 
    geom_hline(yintercept = c(-1.25, 1.25), lty = 3) + 
    geom_line(show.legend = F, size = 1.25) + 
    geom_segment(inherit.aes = F, aes(x = 1, xend = 98, y = 0, yend = 0), 
      lty = 2, size = 0.25) + 
    coord_cartesian(ylim = c(-2, 2), xlim = c(-5, 100)) + 
    scale_x_continuous(breaks = c(1, seq(10, 100, 10)), 
      labels = c("0", seq(10, 100, 10))) + 
    scale_y_continuous(breaks = NULL) + 
    labs(y = expression(alpha), x = "Time step") + 
    annotate("text", x = -4.5, y = 0.15, size = 5, label = expression(tau)) + 
    geom_segment(inherit.aes = F, aes(x = -5, y = 0, xend = 0, yend = 0), 
      arrow = arrow(length = unit(0.2, "cm")), size = 0.5) + 
    geom_segment(inherit.aes = F, aes(x = -5, y = 0, xend = -9, yend = 0), 
      arrow = arrow(length = unit(0.2, "cm"))) + 
    annotate("text", x = 101, y = 0, size = 5, label = expression(beta)) + 
    geom_segment(inherit.aes = F, aes(x = 101, y = 0.2, xend = 101, yend = 0.5), 
      arrow = arrow(length = unit(0.2, "cm")), size = 0.5) + 
    geom_segment(inherit.aes = F, aes(x = 101, y = -0.2, xend = 101, yend = -0.5), 
      arrow = arrow(length = unit(0.2, "cm"))) + 
    annotate("text", x = 32, y = 0.25, size = 5, label = expression(delta)) + 
    geom_segment(inherit.aes = F, aes(x = 30.5, y = 0.35, xend = 25, yend = 0.7), 
      arrow = arrow(length = unit(0.2, "cm"))) + 
    annotate("text", x = 79, y = 1.4, size = 4, label = "Correct response", hjust = 0) + 
    annotate("text", x = 79, y = -1.4, size = 4, label = "Incorrect response", hjust = 0) + 
    scale_color_viridis_d(option = "B", begin = 0.25, end = 0.75) + 
    theme_minimal(base_family = 'Times', base_size = 13) + 
    theme(panel.grid.major = element_line(size = 0.2),
          panel.grid.minor = element_blank(), 
      axis.title.y = element_text(size = rel(.9), hjust = 0.5))

# Combine plots
ddm_explanation <- main_plot + annotation_custom(
  grob = ggplotGrob(inset_top), 
  ymin = 1.25, ymax = 2, xmin = -2.5, xmax = 75) + 
  annotation_custom(
  grob = ggplotGrob(inset_bottom), 
  ymin = -1.25, ymax = -2, xmin = -2.5, xmax = 75)

# -----------------------------------------------------------------------------




# DDM simulations -------------------------------------------------------------

ddm_yn <- ddm_sims %>% 
  filter(q_type == "yn", step < 20) %>% 
  ggplot(., aes(x = step, y = value)) + 
    facet_wrap(~ facet_lab) + 
    scale_y_continuous(breaks = seq(-1.5, 1.5, 1), 
      labels = seq(-1.5, 1.5, 1)) + 
    coord_cartesian(xlim = c(0, 25), ylim = c(-1.6, 1.6)) + 
    geom_line(aes(group = sim_n), show.legend = F, color = "grey50", 
      alpha = 0.15, size = 0.15) + 
    stat_summary(aes(group = response), fun = mean, geom = "line", 
      color = "white", size = 2) +
    stat_summary(aes(group = response), fun = mean, geom = "line", 
      color = "#cc0033", size = 1) +
    geom_hline(yintercept = 0) + 
    geom_vline(xintercept = 0) + 
    labs(title = "y/n questions", y = "Boundary separation", x = "Time step") + 
    minimal_adj(base_size = 13)

ddm_wh <- ddm_sims %>% 
  filter(q_type == "wh") %>% 
  ggplot(., aes(x = step, y = value)) + 
    facet_wrap(~ facet_lab) + 
    scale_y_continuous(position = "right", breaks = seq(-1.5, 1.5, 1), 
      labels = seq(-1.5, 1.5, 1)) + 
    coord_cartesian(xlim = c(0, 25), ylim = c(-1.6, 1.6)) + 
    geom_line(aes(group = sim_n), show.legend = F, color = "grey50", 
      alpha = 0.15, size = 0.15) + 
    stat_summary(aes(group = response), fun = mean, geom = "line", 
      color = "white", size = 2) +
    stat_summary(aes(group = response), fun = mean, geom = "line", 
      color = "#cc0033", size = 1) +
    geom_hline(yintercept = 0) + 
    geom_vline(xintercept = 0) + 
    labs(title = "wh- questions", y = NULL, x = "Time step") + 
    minimal_adj(base_size = 13)

ddm_simulations <- ddm_yn + ddm_wh

# -----------------------------------------------------------------------------




# Speech rate by variety ------------------------------------------------------

# Plot speech rate for supplementary materials
sm_speech_rate <- sr %>% 
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

# -----------------------------------------------------------------------------




# Check randomization of talkers across trials --------------------------------

sm_random_speaker_check <- learners %>% 
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
      subtitle = glue("(n = {n_learners})"), 
      caption = "Mean +/- SD") + 
    ds4ling::ds4ling_bw_theme(base_size = 13, base_family = "Times") + 
    NULL

# -----------------------------------------------------------------------------








# Save plots ------------------------------------------------------------------

devices     <- c('png', 'pdf')
path_to_fig <- file.path(here("figs", "manuscript"))

walk(devices, ~ ggsave(
  filename = glue(path_to_fig, "/learner_accuracy_by_utterance_type.", .x), 
  plot = learner_accuracy_by_utterance_type, 
  device = .x, height = 4, width = 7, units = "in"))

walk(devices, ~ ggsave(
  filename = glue(path_to_fig, "/learner_accuracy_rt_by_speaker_variety.", .x), 
  plot = learner_accuracy_rt_by_speaker_variety, 
  device = .x, height = 4, width = 9, units = "in"))

walk(devices, ~ ggsave(
  filename = glue(path_to_fig, "/learner_accuracy_lt_eq_by_utterance_type.", .x), 
  plot = learner_accuracy_lt_eq_by_utterance_type, 
  device = .x, height = 4, width = 7, units = "in"))

walk(devices, ~ ggsave(
  filename = glue(path_to_fig, "/learner_accuracy_3way.", .x), 
  plot = learner_accuracy_3way, 
  device = .x, height = 4, width = 7, units = "in"))

walk(devices, ~ ggsave(
  filename = glue(path_to_fig, "/learner_accuracy_3way.", .x), 
  plot = learner_accuracy_3way, 
  device = .x, height = 4, width = 7, units = "in"))

walk(devices, ~ ggsave(
  filename = glue(path_to_fig, "/ddm_bs_dr_estimates.", .x), 
  plot = ddm_bs_dr_estimates, 
  device = .x, height = 4, width = 7, units = "in"))

walk(devices, ~ ggsave(
  filename = glue(path_to_fig, "/ddm_explanation.", .x), 
  plot = ddm_explanation, 
  device = .x, height = 4, width = 7, units = "in"))

walk(devices, ~ ggsave(
  filename = glue(path_to_fig, "/ddm_simulations.", .x), 
  plot = ddm_simulations, 
  device = .x, height = 6, width = 10, units = "in"))

walk(devices, ~ ggsave(
  filename = glue(path_to_fig, "/sm_speech_rate.", .x), 
  plot = sm_speech_rate, 
  device = .x, height = 4, width = 7, units = "in"))

walk(devices, ~ ggsave(
  filename = glue(path_to_fig, "/sm_random_speaker_check.", .x), 
  plot = sm_random_speaker_check, 
  device = .x, height = 4, width = 7, units = "in"))

# -----------------------------------------------------------------------------
