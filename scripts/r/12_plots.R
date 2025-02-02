# Plots -----------------------------------------------------------------------
#
# Author: Joseph V. Casillas
# Last update: 202302112
#
# - All plots presented in slides and manuscripts are generated from this file
#   and saved in their own folder, i.e., figs/slides, figs/manuscript, etc.
# - Default output is to png and pdf
#
# -----------------------------------------------------------------------------




# Source helpers, libs, and models --------------------------------------------

source(here::here("scripts", "r", "07_load_data.R"))

learner_response_01 <- readRDS(here("models", "learner_response_01.rds"))
learner_response_qonly_01 <- readRDS(here("models", "learner_response_qonly_01.rds"))
learner_rt_01 <- readRDS(here("models", "learner_rt_01.rds"))
mem_boundary_separation <- readRDS(here("models", "mem_boundary_separation.rds"))
mem_drift_rate <- readRDS(here("models", "mem_drift_rate.rds"))
ddm_sims <- read_csv(here("data", "tidy", "ddm_sims.csv"))
learner_variety_match_response <- readRDS(here("models", "learner_variety_match_response.rds"))

# -----------------------------------------------------------------------------





# Accuracy model forest plot --------------------------------------------------

simp_y_labs <- c("Intercept", "Wh- question", "Narrow focus statement", 
  "Broad focus statement", "LexTALE", "EQ", "Wh- question x LexTALE", 
  "Narrow focus statement x LexTALE", "Broad focus statement x LexTALE", 
  "Wh- question x EQ", "Narrow focus statement x EQ", "Broad focus statement x EQ", 
  "LexTALE x EQ", "Wh- question x LexTALE x EQ", "Narrow focus statement x LexTALE x EQ", 
  "Broad focus statement x LexTALE x EQ")

simp_labs_tib <- tibble(y = simp_y_labs, x = -2.5) %>% 
  mutate(y = fct_relevel(y, "Intercept", "Wh- question", "Narrow focus statement", 
      "Broad focus statement", "LexTALE", "EQ", "Wh- question x LexTALE", 
      "Narrow focus statement x LexTALE", "Broad focus statement x LexTALE", "Wh- question x EQ", 
      "Narrow focus statement x EQ", "Broad focus statement x EQ", 
      "LexTALE x EQ", "Wh- question x LexTALE x EQ", 
      "Narrow focus statement x LexTALE x EQ", "Broad focus statement x LexTALE x EQ"))

learner_accuracy_forest <- as_tibble(learner_response_01) %>% 
  select(starts_with("b_")) %>% 
  pivot_longer(everything(), names_to = "Parameter", values_to = "Estimate") %>% 
  mutate(Parameter = case_when(
    Parameter == "b_Intercept" ~ "Intercept", 
    Parameter == "b_sentence_typeinterrogativeMpartialMwh" ~ "Wh- question", 
    Parameter == "b_sentence_typedeclarativeMnarrowMfocus" ~ "Narrow focus statement", 
    Parameter == "b_sentence_typedeclarativeMbroadMfocus" ~ "Broad focus statement", 
    Parameter == "b_lextale_std" ~ "LexTALE", 
    Parameter == "b_eq_std" ~ "EQ", 
    Parameter == "b_sentence_typeinterrogativeMpartialMwh:lextale_std" ~ "Wh- question x LexTALE", 
    Parameter == "b_sentence_typedeclarativeMnarrowMfocus:lextale_std" ~ "Narrow focus statement x LexTALE", 
    Parameter == "b_sentence_typedeclarativeMbroadMfocus:lextale_std" ~ "Broad focus statement x LexTALE", 
    Parameter == "b_sentence_typeinterrogativeMpartialMwh:eq_std" ~ "Wh- question x EQ", 
    Parameter == "b_sentence_typedeclarativeMnarrowMfocus:eq_std" ~ "Narrow focus statement x EQ", 
    Parameter == "b_sentence_typedeclarativeMbroadMfocus:eq_std" ~ "Broad focus statement x EQ", 
    Parameter == "b_lextale_std:eq_std" ~ "LexTALE x EQ", 
    Parameter == "b_sentence_typeinterrogativeMpartialMwh:lextale_std:eq_std" ~ "Wh- question x LexTALE x EQ", 
    Parameter == "b_sentence_typedeclarativeMnarrowMfocus:lextale_std:eq_std" ~ "Narrow focus statement x LexTALE x EQ", 
    Parameter == "b_sentence_typedeclarativeMbroadMfocus:lextale_std:eq_std" ~ "Broad focus statement x LexTALE x EQ"), 
    Parameter = fct_relevel(Parameter, "Intercept", "Wh- question", 
      "Narrow focus statement", "Broad focus statement", "LexTALE", "EQ", 
      "Wh- question x LexTALE", "Narrow focus statement x LexTALE", 
      "Broad focus statement x LexTALE", "Wh- question x EQ", "Narrow focus statement x EQ", 
      "Broad focus statement x EQ", "LexTALE x EQ", "Wh- question x LexTALE x EQ", 
      "Narrow focus statement x LexTALE x EQ", "Broad focus statement x LexTALE x EQ")) %>% 
  ggplot(., aes(x = Estimate, y = Parameter)) + 
    coord_cartesian(xlim = c(-2.75, 2.75)) + 
    scale_x_continuous(expand = c(0, 0)) + 
    geom_vline(xintercept = 0, lty = 3) + 
    geom_text(data = simp_labs_tib, hjust = 0, vjust = 0.5, size = 2.25, 
      aes(y = y, x = x, label = y), family = "Times") + 
    stat_halfeye(slab_alpha = 0.5, pch = 21, point_fill = "white", 
      slab_fill = viridis::viridis_pal(option = "B", begin = 0.25)(1), 
      point_size = 1.5) + 
    scale_y_discrete(limits = rev) + 
    ds4ling::ds4ling_bw_theme(base_size = 12, base_family = "Times") + 
    theme(axis.text.y = element_blank(), axis.ticks.y = element_blank())

# -----------------------------------------------------------------------------




# Accuracy by utterance type --------------------------------------------------

learner_accuracy_by_utterance_type <- learner_response_01 %>% 
  as_tibble() %>% 
  select(b_Intercept, starts_with("b_sentence_type")) %>% 
  transmute(
    `y/n\nquestion` = b_Intercept, 
    `Wh-\nquestion` = b_Intercept + b_sentence_typeinterrogativeMpartialMwh, 
    `Narrow focus\nstatement` = b_Intercept + b_sentence_typedeclarativeMnarrowMfocus, 
    `Broad focus\nstatement`  = b_Intercept + b_sentence_typedeclarativeMbroadMfocus
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
    scale_y_continuous(position = "right") + 
    labs(y = "P(correct)", x = "Utterance type") + 
    ds4ling::ds4ling_bw_theme(base_size = 12, base_family = "Times") + 
    theme(axis.title.y = element_text(size = rel(.9), hjust = 0.05), 
      axis.text.x = element_text(size = rel(0.9)), 
      legend.position = c(0.5, 0.1), legend.direction = "horizontal", 
      legend.title = element_text(size = 8.5, color = "grey45"), 
      legend.spacing.x = unit(0,"cm"), legend.background = element_blank(), 
      strip.background = element_rect(fill = NA)) + 
    guides(fill_ramp = guide_legend(keywidth = 0.5, keyheight = 0.12, 
      default.unit = "inch", title.hjust = 0.5, reverse = T, 
      title = "of participant accuracy falls in this range",
      label.position = "bottom", title.position = "bottom")) 

learner_accuracy_forest_utterance_type <- 
  learner_accuracy_forest + learner_accuracy_by_utterance_type

# -----------------------------------------------------------------------------




# Accuracy by speaker variety -------------------------------------------------

learner_accuracy_by_speaker_variety <- learner_response_01 %>% 
  as_tibble() %>% 
  select("b_Intercept", 
    starts_with("r_speaker_variety[") & contains(",Intercept")) %>% 
  transmute(
    Andalusian    = `r_speaker_variety[andalusian,Intercept]`, 
    Argentine     = `r_speaker_variety[argentine,Intercept]`, 
    Madrileño     = `r_speaker_variety[castilian,Intercept]`, 
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
    labs(x = expression(paste(beta, "-Accuracy")), y = NULL) + 
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
    Madrileño     = `r_speaker_variety[castilian,Intercept]`, 
    Chilean       = `r_speaker_variety[chilean,Intercept]`, 
    Cuban         = `r_speaker_variety[cuban,Intercept]`, 
    Mexican       = `r_speaker_variety[mexican,Intercept]`, 
    Peruvian      = `r_speaker_variety[peruvian,Intercept]`, 
   `Puerto Rican` = `r_speaker_variety[puertorican,Intercept]`
  ) %>% 
  pivot_longer(everything(), names_to = "parameter", values_to = "estimate") %>% 
  mutate(parameter = fct_relevel(parameter, "Cuban", "Puerto Rican", 
    "Argentine", "Peruvian", "Andalusian", "Chilean", "Mexican", 
    "Madrileño")) %>% 
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
  "y/n\nquestion", 
  "Wh-\nquestion", 
  "Narrow focus\nstatement", 
  "Broad focus\nstatement")

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

# Facet labels
facet_replace_3way <- tibble(
  is_correct = 0.95, 
  lextale_std = -0.9, 
  cond__ = c("Interrogative\ny/n", "Interrogative\nWh-"), 
  labs = c("y/n question", "Wh- question")
) %>% 
  mutate(cond__ = fct_relevel(cond__, "Interrogative\ny/n"))

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
  geom_text(inherit.aes = F, data = facet_replace_3way, hjust = 0, 
    aes(x = lextale_std, y = is_correct, label = labs), family = "Times") + 
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
    strip.background = element_blank(), strip.text = element_blank()) + 
  guides(color = guide_legend(keywidth = 0.5, keyheight = 0.1, 
      default.unit = "inch", title.hjust = 0.5, reverse = T, 
      title = "Empathy quotient",
      label.position = "bottom", title.position = "top", 
      override.aes = list(fill = NA, size = 2))) 

# -----------------------------------------------------------------------------




# MEM Boundary separation and drift rate --------------------------------------

mem_bs_dr_estimates <- bind_rows(
  as_tibble(mem_drift_rate) %>% 
    select(starts_with("b_")) %>% 
    pivot_longer(everything(), names_to = "params", values_to = "estimate") %>% 
    mutate(effect = "dr"),
  as_tibble(mem_boundary_separation) %>% 
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
    scale_x_continuous(expand = c(0, 0)) + 
    coord_cartesian(xlim = c(-0.25, 2.05)) + 
    geom_vline(xintercept = 0, lty = 3) + 
    stat_halfeye(aes(fill = effect), point_size = 1.5, stroke = 0.5, 
      position = position_dodge(0.5), point_fill = "white") +
    scale_shape_manual(name = NULL, values = c(21, 24), 
      labels = c("Boundary shift", "Drift rate")) + 
    scale_fill_viridis_d(name = NULL, option = "B", begin = 0.3, end = 0.75, 
      labels = c("Boundary shift", "Drift rate")) + 
    scale_y_discrete(limits = rev, position = "left") + 
    labs(y = NULL, x = "Estimate") + 
    ds4ling::ds4ling_bw_theme(base_size = 13, base_family = "Times") + 
    theme(legend.position = c(0.8, 0.2), legend.background = element_blank())

# -----------------------------------------------------------------------------




# DDM explanation -------------------------------------------------------------

# Generate skewed RT-like data
my_skewed_x <- fGarch::rsnorm(100, mean = 2.5, sd = 1.5, xi = 3.5)

# Correct response dist
inset_top <- tibble(x = my_skewed_x) %>%
  ggplot(., aes(x = x)) + 
    geom_density(fill = viridis::viridis_pal(option = "B", begin = 0.3)(1)) + 
    theme_void()

# Incorrect response dist
inset_bottom <- tibble(x = my_skewed_x) %>%
  ggplot(., aes(x = x)) + 
    geom_density(fill = viridis::viridis_pal(option = "B", begin = 0.75)(1)) + 
    scale_y_reverse() + 
    theme_void()

threshold_labs <- tibble(
  step = 80, value = c(1.4, -1.4), 
  labs = c("Correct response", "Incorrect response")
)

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
    geom_text(inherit.aes = F, data = threshold_labs, hjust = 0, 
      aes(x = step, y = value, label = labs), family = "Times") + 
    scale_color_viridis_d(option = "B", begin = 0.3, end = 0.75) + 
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

# Calculate mean decision thresholds
bs_means <- ddm_sims %>% 
  group_by(q_type, eq, lt, facet_lab) %>% 
  summarize(bs_max = max(value), bs_min = min(value), .groups = "drop") %>% 
  pivot_longer(cols = c("bs_max", "bs_min"), 
    names_to = "bound", values_to = "threshold") %>% 
  mutate(facet_lab = fct_relevel(facet_lab, "EQ: Low, LexTALE: low", "EQ: low, LexTALE: high", "EQ: high, LexTALE: low"))



ddm_yn <- ddm_sims %>% 
  filter(q_type == "yn") %>% 
  mutate(facet_lab = fct_relevel(facet_lab, "EQ: Low, LexTALE: low", 
    "EQ: low, LexTALE: high", "EQ: high, LexTALE: low")) %>% 
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
    geom_hline(yintercept = 0, color = "black") + 
    geom_vline(xintercept = 0, color = "black") + 
    geom_hline(data = filter(bs_means, q_type == "yn"), 
      aes(yintercept = threshold), color = "grey35", lty = 3) + 
    labs(title = "y/n questions", y = "Boundary separation", x = "Time step") + 
    minimal_adj(base_size = 13)

ddm_wh <- ddm_sims %>% 
  filter(q_type == "wh") %>% 
  mutate(facet_lab = fct_relevel(facet_lab, "EQ: Low, LexTALE: low", 
    "EQ: low, LexTALE: high", "EQ: high, LexTALE: low")) %>% 
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
    geom_hline(yintercept = 0, color = "black") + 
    geom_vline(xintercept = 0, color = "black") + 
    geom_hline(data = filter(bs_means, q_type == "wh"), 
      aes(yintercept = threshold), color = "grey35", lty = 3) + 
    labs(title = "wh- questions", y = NULL, x = "Time step") + 
    minimal_adj(base_size = 13)

ddm_simulations <- ddm_yn + ddm_wh

# -----------------------------------------------------------------------------




# Speech rate by variety ------------------------------------------------------

sr_subset <- select(sr, speaker_variety, articulation_rate) %>% 
  mutate(val = (articulation_rate - mean(articulation_rate)) / sd(articulation_rate))

native_stim_sr <- brm(
  formula = val ~ 1 + (1|speaker_variety), 
  prior = c(
    prior(normal(0,1), class = Intercept), 
    prior(cauchy(0, 0.1), class = sd), 
    prior(cauchy(0, 0.1), class = sigma)
  ), 
  data = sr_subset, 
  backend = "cmdstanr", 
  file = here("models", "native_stim_sr")
)

# Plot speech rate for supplementary materials
sm_speech_rate <- as_tibble(native_stim_sr) %>% 
  select(starts_with("r_")) %>% 
  pivot_longer(everything(), names_to = "speaker_variety", values_to = "val") %>% 
  mutate(
    speaker_variety = case_when(
      speaker_variety == "r_speaker_variety[andalusian,Intercept]" ~ "Andalusian", 
      speaker_variety == "r_speaker_variety[argentine,Intercept]" ~ "Argentine", 
      speaker_variety == "r_speaker_variety[castilian,Intercept]" ~ "Madrileño", 
      speaker_variety == "r_speaker_variety[chilean,Intercept]" ~ "Chilean", 
      speaker_variety == "r_speaker_variety[cuban,Intercept]" ~ "Cuban", 
      speaker_variety == "r_speaker_variety[mexican,Intercept]" ~ "Mexican", 
      speaker_variety == "r_speaker_variety[peruvian,Intercept]" ~ "Peruvian", 
      speaker_variety == "r_speaker_variety[puertorican,Intercept]" ~ "Puerto Rican"
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
         speaker_variety = tools::toTitleCase(speaker_variety), 
         speaker_variety = str_replace(speaker_variety, "Castilian", "Madrileño")) %>% 
  ggplot(., aes(x = speaker_variety, y = n, fill = speaker_variety)) + 
    geom_hline(yintercept = 8, lty = 3, color = "black") + 
    stat_summary(fun.data = mean_sdl, geom = "pointrange", pch = 21, 
      color = "black", size = 1, fun.args = list(mult = 1), 
      show.legend = F) + 
    scale_fill_viridis_d(option = "B", begin = 0.3) + 
    scale_y_continuous(breaks = seq(0, 20, 4)) + 
    coord_cartesian(ylim = c(0, 20))  + 
    labs(y = "Avg. tokens", x = "Speaker variety", 
      title = "Average stimuli tokens from each variety.", 
      subtitle = glue("(n participants = {length(unique(learners$participant))}, n trials = {nrow(learners)})"), 
      caption = "Mean ±1SD") + 
    ds4ling::ds4ling_bw_theme(base_size = 13, base_family = "Times")

# -----------------------------------------------------------------------------




# Plot learner variety familiarity analyses -----------------------------------

# Get posterior and tidy
learner_matches_post <- learner_variety_match_response %>% 
  as_draws_df() %>% 
  select(contains("b_")) %>% 
  transmute(
    un_yn = b_Intercept, 
    un_wh = b_Intercept + b_sentence_typeinterrogativeMpartialMwh, 
    un_nf = b_Intercept + b_sentence_typedeclarativeMnarrowMfocus, 
    un_bf = b_Intercept + b_sentence_typedeclarativeMbroadMfocus, 
    fm_yn = b_Intercept + b_is_match_labfamiliar, 
    fm_wh = b_Intercept + b_is_match_labfamiliar + b_sentence_typeinterrogativeMpartialMwh + `b_sentence_typeinterrogativeMpartialMwh:is_match_labfamiliar`, 
    fm_nf = b_Intercept + b_is_match_labfamiliar + b_sentence_typedeclarativeMnarrowMfocus + `b_sentence_typedeclarativeMnarrowMfocus:is_match_labfamiliar`, 
    fm_bf = b_Intercept + b_is_match_labfamiliar + b_sentence_typedeclarativeMbroadMfocus + `b_sentence_typedeclarativeMbroadMfocus:is_match_labfamiliar` 
  ) %>% 
  mutate(across(everything(), plogis)) %>% 
  pivot_longer(cols = everything(), names_to = "metric", values_to = "estimate") %>% 
  separate(metric, into = c("familiarity", "type"), sep = "_") %>% 
  group_by(familiarity, type) %>% 
  mutate(draw = seq_along(familiarity)) %>% 
  pivot_wider(names_from = "familiarity", values_from = "estimate") %>% 
  mutate(diff = fm - un, 
         type = case_when(
           type == "yn" ~ "y/n question", 
           type == "wh" ~ "Wh- question", 
           type == "nf" ~ "Narrow focus statement", 
           type == "bf" ~ "Broad focus statement"), 
         type_plot = case_when(
           type == "y/n question" ~ "y/n\nquestion", 
           type == "Wh- question" ~ "Wh-\nquestion", 
           type == "Narrow focus statement" ~ "Narrow focus\nstatement", 
           type == "Broad focus statement" ~ "Broad focus\nstatement")
  )

# Plot posterior conditional effects
learner_variety_familiarity_accuracy <- learner_matches_post %>% 
  pivot_longer(cols = c("un", "fm"), names_to = "familiarity", values_to = "estimate") %>% 
  mutate(familiarity = if_else(familiarity == "un", "Unfamiliar", "Familiar"), 
         familiarity = fct_relevel(familiarity, "Unfamiliar")) %>% 
  ggplot() + 
  aes(x = type_plot, y = estimate, color = familiarity, shape = familiarity) + 
  geom_hline(yintercept = 0.5, lty = 3) + 
  stat_pointinterval(position = position_dodge(0.5), fill = "white") + 
  scale_shape_manual(name = NULL, values = c(21, 23)) + 
  scale_color_manual(name = NULL, 
    values = viridis::viridis_pal(option = "C", begin = 0.2, end = 0.8)(4)) + 
  scale_y_continuous(labels = scales::percent_format()) + 
  labs(y = "Accuracy", x = NULL) + 
  ds4ling::ds4ling_bw_theme(base_size = 12, base_family = "Times") + 
  theme(legend.position = c(0.15, 0.25), legend.background = element_blank()) + 
  guides(color = guide_legend(override.aes = list(size = 5)))

# Plot posterior differences
learner_variety_familiarity_diff <- learner_matches_post %>% 
  ggplot() + 
  aes(y = diff, x = type_plot, fill = type) + 
  geom_hline(yintercept = 0, lty = 3) + 
  stat_eye(show.legend = F, pch = 21, point_fill = "white", slab_alpha = 0.7) + 
  scale_fill_manual(name = NULL, 
    values = viridis::viridis_pal(option = "C", begin = 0.2, end = 0.8)(4)) + 
  scale_y_continuous(position = "right") + 
  labs(y = "Posterior difference\n(familiar - unfamiliar)", x = NULL) + 
  ds4ling::ds4ling_bw_theme(base_size = 12, base_family = "Times") + 
  theme(axis.title.y = element_text(size = rel(.9), hjust = 0.05))

learner_variety_familiarity <- 
  learner_variety_familiarity_accuracy + learner_variety_familiarity_diff

# Posterior conditional effects summary table
# (this is the above plot in table format)
learner_matches_post %>% 
  select(-draw, -type_plot, -draw) %>% 
  group_by(type) %>% 
  median_hdi() %>% 
  mutate_if(is.numeric, specify_decimal, k = 2) %>% 
  transmute(
    `Sentence type` = type, 
    Unfamiliar = glue("{un} [{un.lower}, {un.upper}]"), 
    Familiar = glue("{fm} [{fm.lower}, {fm.upper}]"), 
    Difference = glue("{diff} [{diff.lower}, {diff.upper}]")
  ) %>% 
  write_csv(here("tables", "learner_variety_match_response_cond_effects.csv"))

# -----------------------------------------------------------------------------




# Save plots ------------------------------------------------------------------

devices     <- c('png', 'pdf')
path_to_fig <- file.path(here("figs", "manuscript"))

walk(devices, ~ ggsave(
  filename = glue(path_to_fig, "/learner_accuracy_forest_utterance_type.", .x), 
  plot = learner_accuracy_forest_utterance_type, 
  device = .x, height = 4, width = 8.5, units = "in"))

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
  filename = glue(path_to_fig, "/ddm_bs_dr_estimates.", .x), 
  plot = mem_bs_dr_estimates, 
  device = .x, height = 4, width = 7, units = "in"))

walk(devices, ~ ggsave(
  filename = glue(path_to_fig, "/ddm_explanation.", .x), 
  plot = mem_explanation, 
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

walk(devices, ~ ggsave(
  filename = glue(path_to_fig, "/learner_variety_familiarity.", .x), 
  plot = learner_variety_familiarity, 
  device = .x, height = 4, width = 8.5, units = "in"))

# -----------------------------------------------------------------------------
