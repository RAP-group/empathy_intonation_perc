
# descriptives

# n participants
n_participant <- natives$participant %>% unique() %>% length
n_trials <- natives %>% nrow()

# n by spanish variety
natives %>% 
  group_by(sp_variety) %>%
  summarize(., totals = n_distinct(participant)) %>% 
  add_row(sp_variety = "total", totals = sum(.$totals))

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

natives %>% 
  group_by(participant, sp_variety, speaker_variety) %>% 
  summarize(
    total_trials = max(seq_along(speaker_variety)),  
    total_correct = sum(is_correct), 
    errors = total_trials - total_correct, .groups = "drop") %>% 
  ggplot(., aes(x = sp_variety, y = total_trials, color = speaker_variety)) + 
    geom_hline(yintercept = 8, size = 3, color = "white") + 
    stat_summary(fun.data = mean_sdl, fun.args = list(mult = 1), 
      geom = "pointrange", position = position_dodge(0.5)) + 
    coord_cartesian(ylim = c(0, 20))

# n removed
(id_remove$natives %>% nrow()) + (id_remove$learners %>% nrow())



# RTs and accuracy
nat_d <- natives %>% 
  summarize(
    mean_rt = mean(rt_adj), 
    median_rt = median(rt_adj), 
    mean_cor = mean(is_correct), 
    sd_cor = sd(is_correct)) %>% 
  pivot_longer(cols = everything(), names_to = "metric", values_to = "val") %>% 
  split(.$metric)

natives %>% 
  ggplot(., aes(x = rt_adj)) + 
    geom_histogram(fill = "grey40", color = "black") 

# How many RTs over X?
sum(natives$rt_adj > 5); sum(natives$rt_adj > 10)

# How many RTs < 0?
sum(natives$rt_adj < 0)
sum(natives$rt_adj < 0) / nrow(natives)


natives %>% 
  filter(rt_adj > 0, rt_adj <= 5) %>% 
  ggplot(., aes(x = rt_adj)) + 
    geom_histogram(bins = 50, fill = "grey40", color = "black") + 
    geom_vline(xintercept = c(nat_d$mean_rt$val, nat_d$median_rt$val), 
      color = c("red", "blue"))

natives %>% 
  filter(rt_adj > 0, rt_adj <= 5) %>% 
  ggplot(., aes(x = rt_adj)) + 
    facet_grid(speaker_variety ~ .) + 
    geom_histogram(bins = 50, fill = "grey40", color = "black")

natives %>% 
  filter(rt_adj > 0) %>% 
  ggplot(., aes(x = log(rt_adj))) + 
    geom_histogram(bins = 40, fill = "grey40", color = "black")

natives %>% 
  ggplot(., aes(x = speaker_variety, y = rt_adj)) + 
    geom_jitter(alpha = 0.05, width = 0.2) + 
    stat_summary(fun.data = mean_sdl, geom = "pointrange", 
      fun.args = list(mult = 1), pch = 21, fill = "white") + 
    geom_hline(yintercept = nat_d$median_rt$val, lty = 3, color = "darkred") 

natives %>% 
  filter(rt_adj <= 5 & rt_adj >= 0, is_correct == 1) %>% 
  ggplot(., aes(x = speaker_variety, y = rt_adj)) + 
    geom_violin() + 
    stat_summary(fun.data = mean_sdl, geom = "pointrange", 
      fun.args = list(mult = 1), pch = 21, fill = "white") + 
    geom_hline(yintercept = nat_d$median_rt$val, lty = 3, color = "darkred") + 
    coord_cartesian(ylim = c(-0.5, NA))

natives %>% 
  filter(rt_adj <= 5 & rt_adj >= 0) %>% 
  ggplot(., aes(x = speaker_variety, y = is_correct)) + 
    geom_hline(yintercept = 0.5, size = 3, color = "white") + 
    geom_hline(yintercept = nat_d$mean_cor$val) + 
    stat_summary(fun.data = mean_se, geom = "pointrange") + 
    coord_cartesian(ylim = c(0.4, 1))


# Error analysis

natives %>% 
  summarize(correct = sum(is_correct), 
            incorrect = sum(is_correct == 0), 
            avg = mean(is_correct), 
            check = correct / (correct + incorrect))

natives %>% 
  group_by(speaker_variety) %>% 
  summarize(
    total_trials = n_trials / 8, 
    total_correct = sum(is_correct), 
    errors = total_trials - total_correct) %>% 
  mutate(speaker_variety = fct_reorder(speaker_variety, errors, max)) %>% 
  ggplot(., aes(x = speaker_variety, y = errors)) + 
    geom_bar(stat = "identity", fill = "grey40", color = "black")

natives %>% 
  group_by(participant, sp_variety, speaker_variety) %>% 
  summarize(
    total_trials = max(seq_along(speaker_variety)),  
    total_correct = sum(is_correct), 
    errors = total_trials - total_correct, .groups = "drop") %>% 
  mutate(speaker_variety = fct_reorder(speaker_variety, errors, max)) %>% 
  ggplot(., aes(x = sp_variety, y = errors, color = speaker_variety)) + 
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
    geom_text(aes(x = 1.4, y = 0.1, label = sp_variety), alpha = 0.02)

p1_accuracy + p2_rts

# Reject
# 5efcbbdd231bde0af2edb847 (accuracy)
# 5e8f67f9cc3d6c24f4b646c9 (no code)
# 5f3b1f2078bf3c22f24a262c (accuracy)
# 5e8ea704815afd16671d19e7 (no code)
# 5fbb46ab3109908f63f6f95b (accuracy)
