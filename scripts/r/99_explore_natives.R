# Source libs, helpers, and data ----------------------------------------------

source(here::here("scripts", "r", "07_load_data.R"))

# -----------------------------------------------------------------------------

#
# descriptives
#

# n participants
n_participant <- natives$participant %>% unique() %>% length
n_trials <- natives %>% nrow()

# n participants by spanish variety
natives %>% 
  group_by(spn_variety) %>%
  summarize(., totals = n_distinct(participant)) %>% 
  add_row(spn_variety = "total", totals = sum(.$totals))

# Test randomization of stim
natives %>% 
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

# Test randomization of stim across varieties
natives %>% 
  group_by(participant, spn_variety, speaker_variety) %>% 
  summarize(
    total_trials = max(seq_along(speaker_variety)),  
    total_correct = sum(is_correct), 
    errors = total_trials - total_correct, .groups = "drop") %>% 
  ggplot(., aes(x = spn_variety, y = total_trials, color = speaker_variety)) + 
    geom_hline(yintercept = 8, size = 3, color = "white") + 
    stat_summary(fun.data = mean_sdl, fun.args = list(mult = 1), 
      geom = "pointrange", position = position_dodge(0.5)) + 
    coord_cartesian(ylim = c(0, 20))

# n participants removed
(id_remove$natives %>% nrow()) + (id_remove$learners %>% nrow())


#
# RTs 
#

# RT descriptives
nat_d <- natives %>% 
  summarize(
    mean_rt = mean(rt_adj), 
    median_rt = median(rt_adj), 
    mean_cor = mean(is_correct), 
    sd_cor = sd(is_correct)) %>% 
  pivot_longer(cols = everything(), names_to = "metric", values_to = "val") %>% 
  split(.$metric)

# Dist. of RTs
natives %>% 
  ggplot(., aes(x = rt_adj)) + 
    geom_histogram(fill = "grey40", color = "black") 

# How many RTs over X seconds?
sum(natives$rt_adj > 5); sum(natives$rt_adj > 10)

# How many RTs < 0?
sum(natives$rt_adj <= 0)
sum(natives$rt_adj <= 0) / nrow(natives)

# Accuracy when RT is <= 0
natives %>% 
  filter(rt_adj <= 0) %>% 
  summarize(mean_cor = mean(is_correct))

# adjusted RTs w/ mean/median
natives %>% 
  filter(rt_adj > 0, rt_adj <= 5) %>% 
  ggplot(., aes(x = rt_adj)) + 
    geom_histogram(bins = 50, fill = "grey40", color = "black") + 
    geom_vline(xintercept = c(nat_d$mean_rt$val, nat_d$median_rt$val), 
      color = c("red", "blue"))

# Transformed RTs
natives %>% 
  filter(rt_adj > 0) %>% 
  ggplot(., aes(x = log(rt_adj))) + 
    geom_histogram(bins = 40, fill = "grey40", color = "black")

# Adj RTs by speaker variety
natives %>% 
  filter(rt_adj > 0, rt_adj <= 5) %>% 
  ggplot(., aes(x = rt_adj, y = speaker_variety, 
    fill = 0.5 - abs(0.5 - stat(ecdf)))) +
    stat_density_ridges(geom = "density_ridges_gradient", calc_ecdf = TRUE) + 
    scale_fill_viridis_c(name = "Tail probability", direction = -1)

# Adj RTs by variety
natives %>% 
  ggplot(., aes(x = speaker_variety, y = rt_adj)) + 
    geom_jitter(alpha = 0.05, width = 0.2) + 
    stat_summary(fun.data = mean_sdl, geom = "pointrange", 
      fun.args = list(mult = 1), pch = 21, fill = "white") + 
    geom_hline(yintercept = nat_d$median_rt$val, lty = 3, color = "darkred") 

# Adj RTs with subset
natives %>% 
  filter(rt_adj <= 5 & rt_adj >= 0, is_correct == 1) %>% 
  ggplot(., aes(x = speaker_variety, y = rt_adj)) + 
    geom_violin() + 
    stat_summary(fun.data = mean_sdl, geom = "pointrange", 
      fun.args = list(mult = 1), pch = 21, fill = "white") + 
    geom_hline(yintercept = nat_d$median_rt$val, lty = 3, color = "darkred") + 
    coord_cartesian(ylim = c(-0.5, NA))

# Adj RTs over trials
natives %>% 
  filter(rt_adj <= 5 & rt_adj >= 0) %>% 
  ggplot(., aes(x = trial_n, y = rt_adj)) + 
    stat_summary(fun.data = mean_se, geom = "pointrange", 
      pch = 21, fill = "white")

# Adj RTs over trials
natives %>% 
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
natives %>% 
  group_by(participant) %>% 
  summarize(mean_cor = mean(is_correct)) %>% 
  arrange(desc(mean_cor)) %>% 
  ggplot(., aes(mean_cor)) + 
    geom_histogram(binwidth = 0.015, fill = "grey40", color = "black")

# 1 person got 100%
natives %>% 
  filter(participant == "5fff81cad30d320e706d5244") %>% 
  summarize(mean_rt = mean(rt_adj), sd_rt = sd(rt_adj), 
            min_rt = min(rt_adj), max_rt = max(rt_adj))

# % correct by speaker variety
natives %>% 
  filter(rt_adj <= 5 & rt_adj >= 0) %>% 
  ggplot(., aes(x = speaker_variety, y = is_correct)) + 
    geom_hline(yintercept = 0.5, size = 3, color = "white") + 
    geom_hline(yintercept = nat_d$mean_cor$val, lty = 3) + 
    stat_summary(fun.data = mean_cl_boot, geom = "pointrange") + 
    coord_cartesian(ylim = c(0.5, 1))

# % correct by trials
natives %>% 
  filter(rt_adj <= 5 & rt_adj >= 0) %>% 
  ggplot(., aes(x = trial_n, y = is_correct)) + 
    stat_summary(fun.data = mean_se, geom = "pointrange", 
      pch = 21, fill = "white")

# % correct by speaker variety over trials
natives %>% 
  filter(rt_adj <= 5 & rt_adj >= 0) %>% 
  ggplot(., aes(x = trial_n, y = is_correct)) + 
    stat_summary(fun = mean, geom = "line", aes(group = speaker_variety), 
      color = "grey50", alpha = 0.5) + 
    stat_summary(fun.data = mean_se, geom = "ribbon", fill = "darkred", alpha = 0.2) + 
    stat_summary(fun = mean, geom = "line", color = "darkred", size = 1.5) + 
    coord_cartesian(ylim = c(0.5, 1.1))

# % correct over trials by speaker variety
natives %>% 
  filter(rt_adj <= 5 & rt_adj >= 0) %>% 
  ggplot(., aes(x = trial_n, y = is_correct)) + 
    facet_grid(speaker_variety ~ .) + 
    stat_summary(fun = mean, geom = "line") + 
    coord_cartesian(ylim = c(0.2, 1.1))


#
# Error analysis
#

# Error descriptives
natives %>% 
  summarize(correct = sum(is_correct), 
            incorrect = sum(is_correct == 0), 
            avg = mean(is_correct), 
            check = correct / (correct + incorrect))

# N errors by speaker variety
natives %>% 
  filter(is_correct == 0) %>% 
  group_by(speaker_variety) %>% 
  summarize(
    errors = n()) %>% 
  mutate(speaker_variety = fct_reorder(speaker_variety, errors, max)) %>% 
  ggplot(., aes(x = speaker_variety, y = errors)) + 
    geom_bar(stat = "identity", fill = "grey40", color = "black")

# N errors by speaker variety
natives %>% 
  filter(is_correct == 0) %>% 
  group_by(spn_variety, speaker_variety) %>% 
  summarize(errors = n(), .groups = "drop") %>%
  mutate(speaker_variety = fct_reorder(speaker_variety, errors, max)) %>% 
  ggplot(., aes(x = speaker_variety, y = errors)) + 
    facet_grid(. ~ spn_variety) + 
    geom_bar(stat = "identity", color = "black") + 
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))

# Error rate by listener variety and speaker variety
natives %>% 
  group_by(participant, spn_variety, speaker_variety) %>% 
  summarize(
    total_trials = max(seq_along(speaker_variety)),  
    total_correct = sum(is_correct), 
    errors = total_trials - total_correct, .groups = "drop") %>% 
  mutate(speaker_variety = fct_reorder(speaker_variety, errors, max)) %>% 
  ggplot(., aes(x = spn_variety, y = errors, color = speaker_variety)) + 
    stat_summary(fun.data = mean_se, geom = "pointrange", 
      position = position_dodge(0.5)) + 
    scale_color_viridis_d(name = "Speaker variety") + 
    labs(x = "Listener variety", y = "Error rate")















# Keep/reject?

p1_accuracy <- natives %>% 
  filter(participant == "5edd2b336561d6085ed58841") %>% 
  ggplot(., aes(x = participant, y = is_correct)) + 
    stat_summary(fun.data = mean_se, geom = "pointrange") + 
    coord_cartesian(ylim = c(0.5, 1))

p2_rts <- natives %>% 
  filter(participant == "5edd2b336561d6085ed58841") %>% 
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

# Reject
# 5efcbbdd231bde0af2edb847 (accuracy)
# 5e8f67f9cc3d6c24f4b646c9 (no code)
# 5f3b1f2078bf3c22f24a262c (accuracy)
# 5e8ea704815afd16671d19e7 (no code)
# 5fbb46ab3109908f63f6f95b (accuracy)
