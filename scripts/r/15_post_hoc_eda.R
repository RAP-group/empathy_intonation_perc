# Post-hoc EDA ----------------------------------------------------------------
#
# Author: Joseph V. Casillas
# Last update: 20230214
#
# This script will:
#   - run post hoc EDA as suggested by reviewer 2
#
# -----------------------------------------------------------------------------




# Source libs and helpers -----------------------------------------------------

source(here::here("scripts", "r", "07_load_data.R"))

# -----------------------------------------------------------------------------




# Data prep -------------------------------------------------------------------

# Make subset of yes/no questions
# Add column is_caribbean for cuban/puertorican vs other
#  - this accounts for the rise vs. falls in the y/n questions
# Add column 'tune' for rising/falling tunes in the caribbean varieties
#  - this subset is for looking specificially at the caribbean varieties
post_hoc_subset_natives <- natives %>% 
  filter(
    sentence_type  == "interrogative-total-yn", 
  ) %>% 
  mutate(is_caribbean = if_else(
    speaker_variety %in% c("cuban", "puertorican"), 
    "Caribbean", "Non-Caribbean")
  )

# Same for learners
post_hoc_subset_learners <- learners %>% 
  filter(
    sentence_type  == "interrogative-total-yn", 
  ) %>% 
  mutate(is_caribbean = if_else(
    speaker_variety %in% c("cuban", "puertorican"), 
    "Caribbean", "Non-Caribbean")
  ) 

# -----------------------------------------------------------------------------




# Descriptives and models -----------------------------------------------------

#
# Natives
#
post_hoc_subset_natives %>% 
  group_by(is_caribbean) %>% 
  summarize(avg = mean(is_correct), sd = sd(is_correct))

ph_natives_tib <- post_hoc_subset_natives %>% 
  mutate(pr_cu_other = if_else(!(speaker_variety %in% c("cuban", "puertorican")), 
                               "other", .$speaker_variety)) %>% 
  group_by(pr_cu_other) %>% 
  summarize(avg = mean(is_correct), sd = sd(is_correct))

# Learners accuracy as a function of tune (`is_caribbean` bc they fall, others rise)
post_hoc_eda_natives_tune <- 
  brm(
    formula = is_correct ~ is_caribbean + (1 | participant), 
    cores = 4, threads = threading(2), backend = "cmdstanr",
    data = post_hoc_subset_natives, 
    family = bernoulli(), 
    file = here("models", "post_hoc_eda_natives_tune")
  )

post_hoc_eda_natives_tune_posterior <- emmeans::emmeans(
  object = post_hoc_eda_natives_tune, 
  spec = ~ is_caribbean, 
  epred = TRUE
  ) %>% 
  gather_emmeans_draws()

#
# Learners
#

# Learners are better at yn q's if they dont have falling intonation
post_hoc_subset_learners %>% 
  group_by(is_caribbean) %>% 
  summarize(avg = mean(is_correct), sd = sd(is_correct))

ph_learners_tib <- post_hoc_subset_learners %>% 
  mutate(pr_cu_other = if_else(!(speaker_variety %in% c("cuban", "puertorican")), 
                               "other", .$speaker_variety)) %>% 
  group_by(pr_cu_other) %>% 
  summarize(avg = mean(is_correct), sd = sd(is_correct))

# Look at just caribbean varieties
# There are a few rises in these varieties, but accuracy isn't much different
# This subset labels the few items with rises
post_hoc_subset_learners %>% 
  mutate(tune = case_when(
    speaker_variety == "cuban" & sentence == "El bebe comia muy bien" ~ "rise", 
    speaker_variety == "cuban" & sentence == "La maestra vive en Paris" ~ "rise", 
    speaker_variety == "puertorican" & sentence == "Mi madre come la fruta" ~ "rise", 
    TRUE ~ "fall"
    )
  ) %>% 
  filter(is_caribbean == "Caribbean") %>% 
  group_by(tune) %>% 
  summarize(avg = mean(is_correct), sd = sd(is_correct))

learner_native_risefall_accuracy <- bind_rows(
  ph_natives_tib %>% 
    pivot_wider(names_from = pr_cu_other, values_from = c("avg", "sd")) %>% 
    mutate(Group = "Native listeners"), 

  ph_learners_tib %>% 
    pivot_wider(names_from = pr_cu_other, values_from = c("avg", "sd")) %>% 
    mutate(Group = "L2 learners")
  ) %>% 
  mutate_if(is.numeric, round, digits = 2) %>% 
  mutate(
    Group, 
    Cuban = glue("{avg_cuban} ({sd_cuban})"), 
    `Puerto Rican` = glue("{avg_puertorican} ({sd_puertorican})"), 
    Other = glue("{avg_other} ({sd_other})")
  ) %>% 
  write_csv(here("tables", "learner_native_risefall_accuracy.csv"))

# -----------------------------------------------------------------------------




# Plots -----------------------------------------------------------------------

# Accuracy as a function of empathy quotient
caribbean_learners <- post_hoc_subset %>% 
  group_by(participant, is_caribbean, speaker_variety, eq_score) %>% 
  summarize(prop = mean(is_correct), .groups = "drop") %>% 
  ggplot() + 
  aes(x = eq_score, y = prop, color = is_caribbean) + 
  geom_jitter(alpha = 0.1, height = 0.01, width = 0.01) + 
  geom_smooth(method = "lm", formula = "y ~ x", fullrange = T, show.legend = F) + 
  scale_color_manual(name = NULL, values = viridis::viridis_pal(
    option = "B", begin = 0.3, end = 0.8)(2)) + 
  coord_cartesian(ylim = c(0, 1)) + 
  labs(y ="Accuracy", x = "L2 listeners\nEmpathy quotient") + 
  ds4ling::ds4ling_bw_theme(base_size = 12) + 
  theme(legend.position = "bottom", 
        axis.text.x = element_text(size = rel(0.75))) + 
  guides(color = guide_legend(override.aes = list(alpha = 1))) 

# Native accuracy as a function of is_caribbean
caribbean_natives <- post_hoc_eda_natives_tune_posterior %>% 
  ggplot() + 
  aes(x = 1, y = .value, color = is_caribbean) + 
  stat_eye(show.legend = F) + 
  scale_color_manual(name = NULL, values = viridis::viridis_pal(
    option = "B", begin = 0.3, end = 0.8)(2)) + 
  coord_cartesian(ylim = c(0, 1)) + 
  scale_y_continuous(position = "right") + 
  labs(y = NULL, x = "Native\nlisteners") + 
  ds4ling::ds4ling_bw_theme(base_size = 12) + 
  theme(strip.background = element_rect(fill = NA), 
        axis.text.x = element_blank(), axis.ticks.x = element_blank())

learner_native_caribbean_accuracy <- caribbean_learners + caribbean_natives + 
  plot_layout(widths = c(5, 1), guides = 'keep') & 
  theme(text = element_text('Palatino'))

walk(c('png', 'pdf'), ~ ggsave(
  filename = glue(file.path(here("figs", "manuscript")), "/learner_native_caribbean_accuracy.", .x), 
  plot = learner_native_caribbean_accuracy, 
  device = .x, height = 4.5, width = 6.5, units = "in", dpi = 300))

# -----------------------------------------------------------------------------
