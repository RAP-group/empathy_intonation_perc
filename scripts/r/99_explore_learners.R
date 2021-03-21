# Source libs, helpers, and data ----------------------------------------------

source(here::here("scripts", "r", "07_load_data.R"))

# -----------------------------------------------------------------------------

#
# descriptives
#

# n participants
n_participant <- learners$participant %>% unique() %>% length
n_trials <- learners %>% nrow()

# n participants by eng variety
learners %>% 
  group_by(eng_variety) %>%
  summarize(., totals = n_distinct(participant)) %>% 
  add_row(eng_variety = "total", totals = sum(.$totals))

# Test randomization of stim
learners %>% 
  select(participant, speaker_variety) %>% 
  group_by(participant, speaker_variety) %>% 
  tally() %>% 
  ungroup() %>% 
  mutate(prop = n / 8, 
         speaker_variety = tools::toTitleCase(speaker_variety)) %>% 
  ggplot(., aes(x = speaker_variety, y = n)) + 
    geom_hline(yintercept = 8, size = 2, color = "white") + 
    geom_violin() + 
    stat_summary(fun.data = mean_sdl, geom = "pointrange", pch = 21, 
      fill = "white", size = 1, fun.args = list(mult = 1)) + 
    coord_cartesian(ylim = c(-5, 20))  + 
  labs(y = "Mean", x = "Speaker variety", 
    title = "Average stimuli tokens from each variety.", 
    subtitle = glue("(n = {n_participant})"), 
    caption = "Mean +/- SD")

# Test randomization of stim across eng varieties
learners %>% 
  group_by(participant, eng_variety, speaker_variety) %>% 
  summarize(
    total_trials = max(seq_along(speaker_variety)),  
    total_correct = sum(is_correct), 
    errors = total_trials - total_correct, .groups = "drop") %>% 
  ggplot(., aes(x = eng_variety, y = total_trials, color = speaker_variety)) + 
    geom_hline(yintercept = 8, size = 3, color = "white") + 
    stat_summary(fun.data = mean_sdl, fun.args = list(mult = 1), 
      geom = "pointrange", position = position_dodge(0.5)) + 
    coord_cartesian(ylim = c(0, 20))

# n participants removed
(id_remove$learners %>% nrow()) + (id_remove$learners %>% nrow())


#
# RTs 
#

# RT descriptives
nat_d <- learners %>% 
  summarize(
    mean_rt = mean(rt_adj), 
    median_rt = median(rt_adj), 
    mean_cor = mean(is_correct), 
    sd_cor = sd(is_correct)) %>% 
  pivot_longer(cols = everything(), names_to = "metric", values_to = "val") %>% 
  split(.$metric)

# Dist. of RTs
learners %>% 
  ggplot(., aes(x = rt_adj)) + 
    geom_histogram(fill = "grey40", color = "black") 

# How many RTs over X seconds?
sum(learners$rt_adj > 5); sum(learners$rt_adj > 10)

# How many RTs < 0?
sum(learners$rt_adj <= 0)
sum(learners$rt_adj <= 0) / nrow(learners)

# Accuracy when RT is <= 0
learners %>% 
  filter(rt_adj <= 0) %>% 
  summarize(mean_cor = mean(is_correct))

# adjusted RTs w/ mean/median
learners %>% 
  filter(rt_adj > 0, rt_adj <= 5) %>% 
  ggplot(., aes(x = rt_adj)) + 
    geom_histogram(bins = 50, fill = "grey40", color = "black") + 
    geom_vline(xintercept = c(nat_d$mean_rt$val, nat_d$median_rt$val), 
      color = c("red", "blue"))

# Transformed RTs
learners %>% 
  filter(rt_adj > 0) %>% 
  ggplot(., aes(x = log(rt_adj))) + 
    geom_histogram(bins = 40, fill = "grey40", color = "black")

# Adj RTs by speaker variety
learners %>% 
  filter(rt_adj > 0, rt_adj <= 5) %>% 
  ggplot(., aes(x = rt_adj, y = speaker_variety, 
    fill = 0.5 - abs(0.5 - stat(ecdf)))) +
    stat_density_ridges(geom = "density_ridges_gradient", calc_ecdf = TRUE) + 
    scale_fill_viridis_c(name = "Tail probability", direction = -1)

# Adj RTs by variety
learners %>% 
  ggplot(., aes(x = speaker_variety, y = rt_adj)) + 
    geom_jitter(alpha = 0.05, width = 0.2) + 
    stat_summary(fun.data = mean_sdl, geom = "pointrange", 
      fun.args = list(mult = 1), pch = 21, fill = "white") + 
    geom_hline(yintercept = nat_d$median_rt$val, lty = 3, color = "darkred") 

# Adj RTs with subset
learners %>% 
  filter(rt_adj <= 5 & rt_adj >= 0, is_correct == 1) %>% 
  ggplot(., aes(x = speaker_variety, y = rt_adj)) + 
    geom_violin() + 
    stat_summary(fun.data = mean_sdl, geom = "pointrange", 
      fun.args = list(mult = 1), pch = 21, fill = "white") + 
    geom_hline(yintercept = nat_d$median_rt$val, lty = 3, color = "darkred") + 
    coord_cartesian(ylim = c(-0.5, NA))

# Adj RTs over trials
learners %>% 
  filter(rt_adj <= 5 & rt_adj >= 0) %>% 
  ggplot(., aes(x = trial_n, y = rt_adj)) + 
    stat_summary(fun.data = mean_se, geom = "pointrange", 
      pch = 21, fill = "white")

# Adj RTs over trials
learners %>% 
  filter(rt_adj <= 5 & rt_adj >= 0) %>% 
  ggplot(., aes(x = trial_n, y = rt_adj)) + 
    stat_summary(fun = mean, geom = "line", aes(group = speaker_variety), 
      color = "grey50", alpha = 0.5) + 
    stat_summary(fun.data = mean_se, geom = "ribbon", fill = "darkred", alpha = 0.2) + 
    stat_summary(fun = mean, geom = "line", color = "darkred", size = 1.5) + 
    coord_cartesian(ylim = c(0, NA))



#
# Accuracy
#

# Dist. of correct responses
learners %>% 
  group_by(participant) %>% 
  summarize(mean_cor = mean(is_correct)) %>% 
  arrange(desc(mean_cor)) %>% 
  ggplot(., aes(mean_cor)) + 
    geom_histogram(binwidth = 0.015, fill = "grey40", color = "black")

# % correct by speaker variety
learners %>% 
  filter(rt_adj <= 5 & rt_adj >= 0) %>% 
  ggplot(., aes(x = speaker_variety, y = is_correct)) + 
    geom_hline(yintercept = 0.5, size = 3, color = "white") + 
    geom_hline(yintercept = nat_d$mean_cor$val, lty = 3) + 
    stat_summary(fun.data = mean_cl_boot, geom = "pointrange") + 
    coord_cartesian(ylim = c(0.5, 1))

# % correct by trials
learners %>% 
  filter(rt_adj <= 5 & rt_adj >= 0) %>% 
  ggplot(., aes(x = trial_n, y = is_correct)) + 
    stat_summary(fun.data = mean_se, geom = "pointrange", 
      pch = 21, fill = "white")

# % correct by speaker variety over trials
learners %>% 
  filter(rt_adj <= 5 & rt_adj >= 0) %>% 
  ggplot(., aes(x = trial_n, y = is_correct)) + 
    stat_summary(fun = mean, geom = "line", aes(group = speaker_variety), 
      color = "grey50", alpha = 0.5) + 
    stat_summary(fun.data = mean_se, geom = "ribbon", fill = "darkred", alpha = 0.2) + 
    stat_summary(fun = mean, geom = "line", color = "darkred", size = 1.5) + 
    coord_cartesian(ylim = c(0.5, 1.1))

# % correct over trials by speaker variety
learners %>% 
  filter(rt_adj <= 5 & rt_adj >= 0) %>% 
  ggplot(., aes(x = trial_n, y = is_correct)) + 
    facet_grid(speaker_variety ~ .) + 
    stat_summary(fun = mean, geom = "line") + 
    coord_cartesian(ylim = c(0.2, 1.1))


#
# Error analysis
#

# Error descriptives
learners %>% 
  summarize(correct = sum(is_correct), 
            incorrect = sum(is_correct == 0), 
            avg = mean(is_correct), 
            check = correct / (correct + incorrect))

# N errors by speaker variety
learners %>% 
  filter(is_correct == 0) %>% 
  group_by(speaker_variety) %>% 
  summarize(
    errors = n()) %>% 
  mutate(speaker_variety = fct_reorder(speaker_variety, errors, max)) %>% 
  ggplot(., aes(x = speaker_variety, y = errors)) + 
    geom_bar(stat = "identity", fill = "grey40", color = "black")

# N errors by speaker variety
learners %>% 
  filter(is_correct == 0) %>% 
  group_by(eng_variety, speaker_variety) %>% 
  summarize(errors = n(), .groups = "drop") %>%
  mutate(speaker_variety = fct_reorder(speaker_variety, errors, max)) %>% 
  ggplot(., aes(x = speaker_variety, y = errors)) + 
    facet_grid(. ~ eng_variety) + 
    geom_bar(stat = "identity", color = "black") + 
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))

# Error rate by listener variety and speaker variety
learners %>% 
  group_by(participant, eng_variety, speaker_variety) %>% 
  summarize(
    total_trials = max(seq_along(speaker_variety)),  
    total_correct = sum(is_correct), 
    errors = total_trials - total_correct, .groups = "drop") %>% 
  mutate(speaker_variety = fct_reorder(speaker_variety, errors, max)) %>% 
  ggplot(., aes(x = eng_variety, y = errors, color = speaker_variety)) + 
    stat_summary(fun.data = mean_se, geom = "pointrange", 
      position = position_dodge(0.5)) + 
    scale_color_viridis_d(name = "Speaker variety") + 
    labs(x = "Listener variety", y = "Error rate")


#
# Empathy
#

learners %>% 
  summarize(mean_eq = mean(eq_score), sd_eq = sd(eq_score), 
    min_eq = min(eq_score), max_eq = max(eq_score))

learners %>% 
  ggplot(., aes(x = eq_score)) + 
    geom_histogram(fill = "grey", color = "black", binwidth = 2)

learners %>% 
  filter(rt_adj <= 5) %>% 
  ggplot(., aes(x = eq_score, y = is_correct)) + 
    geom_point() + 
    geom_smooth(method = "glm", method.args = list(family = "binomial"))

learners %>% 
  filter(rt_adj <= 5) %>% 
  ggplot(., aes(x = eq_score, y = is_correct)) + 
    facet_grid(. ~ speaker_variety) + 
    geom_point() + 
    geom_smooth(method = "glm", method.args = list(family = "binomial"))

learners %>% 
  filter(rt_adj <= 5) %>% 
  ggplot(., aes(x = eq_score, y = is_correct, color = sentence_type)) + 
    facet_grid(. ~ speaker_variety) + 
    geom_point() + 
    geom_smooth(method = "glm", method.args = list(family = "binomial"))

learners %>% 
  filter(rt_adj <= 5, is_correct == 1) %>% 
  ggplot(., aes(x = eq_score, y = rt_adj)) + 
    geom_point() + 
    geom_smooth(method = "lm")

learners %>% 
  filter(rt_adj <= 5, is_correct == 1) %>% 
  ggplot(., aes(x = eq_score, y = rt_adj)) + 
    facet_grid(. ~ speaker_variety) + 
    geom_point() + 
    geom_smooth(method = "lm")

learners %>% 
  filter(rt_adj <= 5, is_correct == 1) %>% 
  ggplot(., aes(x = eq_score, y = rt_adj, color = sentence_type)) + 
    geom_point() + 
    geom_smooth(method = "lm")

learners %>% 
  filter(rt_adj <= 5, is_correct == 1) %>% 
  ggplot(., aes(x = eq_score, y = rt_adj, color = sentence_type)) + 
    facet_grid(. ~ speaker_variety) + 
    geom_point() + 
    geom_smooth(method = "lm")


#
# Lextale
#


learners %>% 
  summarize(meanlextale = mean(lextale_avg), sdlextale = sd(lextale_avg), 
    minlextale = min(lextale_avg), maxlextale = max(lextale_avg))

learners %>% 
  summarize(meanlextale = mean(lextale_tra), sdlextale = sd(lextale_tra), 
    minlextale = min(lextale_tra), maxlextale = max(lextale_tra))


learners %>% 
  ggplot(., aes(x = lextale_avg)) + 
    geom_histogram(fill = "grey", color = "black", binwidth = 2)

learners %>% 
  ggplot(., aes(x = lextale_tra)) + 
    geom_histogram(fill = "grey", color = "black", binwidth = 2)

learners %>% 
  distinct(participant, lextale_avg, lextale_tra) %>% 
  pivot_longer(cols = -participant, names_to = "lextale", values_to = "score") %>% 
  group_by(lextale) %>% 
  mutate(score_std = scale(score)) %>% 
  ggplot(., aes(x = lextale, y = score)) + 
    geom_boxplot()

learners %>% 
  ggplot(., aes(x = lextale_avg, y = is_correct)) + 
    geom_point() + 
    geom_smooth(method = "glm", method.args = list(family = "binomial"))

learners %>% 
  ggplot(., aes(x = lextale_avg, y = is_correct)) + 
    facet_grid(. ~ speaker_variety) + 
    geom_point() + 
    geom_smooth(method = "glm", method.args = list(family = "binomial"))

learners %>% 
  ggplot(., aes(x = lextale_avg, y = is_correct, color = sentence_type)) + 
    geom_point() + 
    geom_smooth(method = "glm", method.args = list(family = "binomial"))

learners %>% 
  ggplot(., aes(x = lextale_avg, y = is_correct, color = sentence_type)) + 
    facet_grid(. ~ speaker_variety) + 
    geom_point() + 
    geom_smooth(method = "glm", method.args = list(family = "binomial"))


#
# Lextale and empathy
#

learners %>% 
  ggplot(., aes(x = eq_score, y = lextale_tra)) + 
    geom_point()

learners %>% 
  ggplot(., aes(x = eq_score, y = lextale_avg)) + 
    geom_point()






# Keep/reject?

p1_accuracy <- learners %>% 
  filter(participant == "5e85088b3f06400f3f75e21f") %>% 
  ggplot(., aes(x = participant, y = is_correct)) + 
    stat_summary(fun.data = mean_se, geom = "pointrange") + 
    coord_cartesian(ylim = c(0.25, 1))

p2_rts <- learners %>% 
  filter(participant == "5e85088b3f06400f3f75e21f") %>% 
  ggplot(., aes(x = participant, y = rt_adj)) + 
    geom_jitter(alpha = 0.5, width = 0.2, 
      aes(color = factor(is_correct))) + 
    geom_text(nudge_x = -0.35, 
      aes(label = ifelse(is_correct == 0, speaker_variety, ''))) + 
    stat_summary(fun.data = mean_se, geom = "pointrange") + 
    scale_color_brewer(name = NULL, palette = "Set1", 
      labels = c("incorrect", "correct")) + 
    geom_text(aes(x = 1.4, y = 0.1, label = spn_variety), alpha = 0.02)

p1_accuracy + p2_rts

learners %>% 
  select(participant, check_fails) %>% 
  filter(check_fails != 0) %>% 
  pull(participant) %>% unique()



# Rejected
# 5fc8232ac3b2ae180fa1a39b (took a few vacations)
# 5fc98a7db2180e4b23c6f5be (all 1's on 2afc)
# 5dcd91a0b7ebf606d16c971e (failed attention check, finished in 9min)
