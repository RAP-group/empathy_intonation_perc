# Source libs, helpers, and data ----------------------------------------------

source(here::here("scripts", "r", "07_load_data.R"))

# -----------------------------------------------------------------------------

#
# descriptives
#

# n participants
n_participant <- learners$participant %>% unique() %>% length
n_trials <- learners %>% nrow()

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

# n learners removed
id_remove$learners %>% nrow()

# n participants removed
(id_remove$learners %>% nrow()) + (id_remove$native %>% nrow())

# n incomplete
length(returned)


#
# RTs 
#

# RT descriptives
nat_d <- learners %>% 
  summarize(
    mean_rt = mean(rt_raw), 
    median_rt = median(rt_raw), 
    mean_cor = mean(is_correct), 
    sd_cor = sd(is_correct)) %>% 
  pivot_longer(cols = everything(), names_to = "metric", values_to = "val") %>% 
  split(.$metric)

# Dist. of RTs
learners %>% 
  ggplot(., aes(x = rt_raw)) + 
    geom_histogram(fill = "grey40", color = "black", binwidth = 0.15) 

# How many RTs over X seconds?
sum(learners$rt_raw > 5); sum(learners$rt_raw > 10)

# Accuracy when RT is <= 0
learners %>% 
  filter(rt_raw >= 15) %>% 
  summarize(mean_cor = mean(is_correct))


# How many RTs < 0?
sum(learners$rt_adj <= 0)
sum(learners$rt_adj <= 0) / nrow(learners)

# Accuracy when RT is <= 0
learners %>% 
  filter(rt_adj <= 0) %>% 
  summarize(mean_cor = mean(is_correct))

# adjusted RTs
learners %>% 
  filter(rt_adj >= -0.1, rt_adj <= 5) %>% 
  ggplot(., aes(x = rt_adj + abs(min(rt_adj)) + 0.2)) + 
    geom_histogram(bins = 50, fill = "grey40", color = "black") 

# Transformed RTs
learners %>% 
  filter(rt_adj >= -0.1) %>% 
  ggplot(., aes(x = log(rt_adj + abs(min(rt_adj)) + 0.2))) +
    geom_histogram(bins = 40, fill = "grey40", color = "black")

# Adj RTs by speaker variety
learners %>% 
  filter(rt_adj >= -0.1, rt_adj <= 5) %>% 
  ggplot(., aes(x = rt_adj + abs(min(rt_adj)) + 0.2, y = speaker_variety, 
    fill = 0.5 - abs(0.5 - stat(ecdf)))) +
    stat_density_ridges(geom = "density_ridges_gradient", calc_ecdf = TRUE) + 
    scale_fill_viridis_c(name = "Tail probability", direction = -1)

# Adj RTs by variety
learners %>% 
  filter(rt_adj >= -0.1, rt_adj <= 5) %>% 
  ggplot(., aes(x = speaker_variety, y = rt_adj)) + 
    geom_jitter(alpha = 0.05, width = 0.2) + 
    stat_summary(fun.data = mean_sdl, geom = "pointrange", 
      fun.args = list(mult = 1), pch = 21, fill = "white") + 
    geom_hline(yintercept = nat_d$median_rt$val, lty = 3, color = "darkred") 

# Adj RTs with subset
learners %>% 
  filter(rt_adj >= -0.1, rt_adj <= 5) %>% 
  ggplot(., aes(x = speaker_variety, y = rt_adj + abs(min(rt_adj)) + 0.2)) + 
    geom_violin() + 
    stat_summary(fun.data = mean_sdl, geom = "pointrange", 
      fun.args = list(mult = 1), pch = 21, fill = "white") + 
    coord_cartesian(ylim = c(-0.5, NA))

# Adj RTs over trials
learners %>% 
  filter(rt_adj >= -0.1, rt_adj <= 5) %>% 
  ggplot(., aes(x = trial_n, y = rt_adj)) + 
    stat_summary(fun.data = mean_se, geom = "pointrange", 
      pch = 21, fill = "white")

# Adj RTs over trials
learners %>% 
  filter(rt_adj >= -0.1, rt_adj <= 5) %>% 
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
    geom_histogram(binwidth = 0.035, fill = "grey40", color = "black")

# % correct by speaker variety
learners %>% 
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
    scale_fill_viridis_d(option = "viridis") + 
    coord_cartesian(ylim = c(0.5, 1)) + 
    labs(y = "Proportion correct", x = NULL, 
         title = "Proportion correct as a function of speaker variety", 
         caption = "Mean ± 95% CI") + 
    ds4ling::ds4ling_bw_theme(base_size = 13, base_family = "Times")

# % correct by trials
learners %>% 
  filter(rt_adj >= -0.1, rt_adj <= 5) %>% 
  ggplot(., aes(x = trial_n, y = is_correct)) + 
    stat_summary(fun.data = mean_se, geom = "pointrange", 
      pch = 21, fill = "white")

# % correct by speaker variety over trials
learners %>% 
  filter(rt_adj >= -0.1, rt_adj <= 5) %>% 
  ggplot(., aes(x = trial_n, y = is_correct)) + 
    stat_summary(fun = mean, geom = "line", aes(group = speaker_variety), 
      color = "grey50", alpha = 0.5) + 
    stat_summary(fun.data = mean_se, geom = "ribbon", fill = "darkred", alpha = 0.2) + 
    stat_summary(fun = mean, geom = "line", color = "darkred", size = 1.5) + 
    coord_cartesian(ylim = c(0.4, 1.1))


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

# Error rate by listener variety and speaker variety
learners %>% 
  group_by(participant, speaker_variety) %>% 
  summarize(
    total_trials = max(seq_along(speaker_variety)),  
    total_correct = sum(is_correct), 
    errors = total_trials - total_correct, .groups = "drop") %>% 
  mutate(speaker_variety = fct_reorder(speaker_variety, errors, max)) %>% 
  ggplot(., aes(x = speaker_variety, y = errors)) + 
    stat_summary(fun.data = mean_se, geom = "pointrange") + 
    scale_color_viridis_d(name = "Speaker variety") + 
    labs(x = "Speaker variety", y = "Error rate")


#
# Empathy
#

learners %>% 
  summarize(mean_eq = mean(eq_score), sd_eq = sd(eq_score), 
    min_eq = min(eq_score), max_eq = max(eq_score))

learners %>% 
  select(participant, eq_score) %>% 
  distinct() %>% 
  ggplot(., aes(x = eq_score)) + 
    geom_histogram(fill = "grey", color = "black", 
      binwidth = 3.54)

learners %>% 
  filter(rt_adj >= -0.1, rt_adj <= 5) %>% 
  ggplot(., aes(x = eq_score, y = is_correct)) + 
    geom_hline(yintercept = 0.5, size = 3, color = "white") + 
    geom_jitter(width = 0.3, height = 0.01, alpha = 0.05, pch = 21) + 
    geom_smooth(method = "glm", method.args = list(family = "binomial")) +
    scale_y_continuous(breaks = seq(0, 1, 0.1)) + 
    coord_cartesian(ylim = c(0.48, 1.0))

learners %>% 
  filter(rt_adj >= -0.1, rt_adj <= 5) %>% 
  ggplot(., aes(x = eq_score, y = is_correct, color = sentence_type)) + 
    geom_hline(yintercept = 0.5, size = 3, color = "white") + 
    geom_jitter(width = 0.3, height = 0.01, alpha = 0.05, pch = 21) + 
    geom_smooth(method = "glm", method.args = list(family = "binomial")) +
    scale_y_continuous(breaks = seq(0, 1, 0.1)) + 
    coord_cartesian(ylim = c(0.48, 1.0))


learners %>% 
  filter(rt_adj >= -0.1, rt_adj <= 5) %>% 
  ggplot(., aes(x = eq_score, y = is_correct, color = speaker_variety)) + 
    facet_grid(. ~ speaker_variety) + 
    geom_hline(yintercept = 0.5, size = 3, color = "white") + 
    geom_jitter(width = 0.3, height = 0.01, alpha = 0.05, pch = 21) + 
    geom_smooth(method = "glm", method.args = list(family = "binomial")) + 
    coord_cartesian(ylim = c(0.48, 1.0))

learners %>% 
  filter(rt_adj >= -0.1, rt_adj <= 5) %>% 
  ggplot(., aes(x = eq_score, y = is_correct, color = sentence_type)) + 
    facet_grid(. ~ speaker_variety) + 
    geom_point() + 
    geom_smooth(method = "glm", se = F, method.args = list(family = "binomial"))

#
# RTs
#

learners %>% 
  filter(rt_adj >= -0.1, rt_adj <= 5) %>% 
  ggplot(., aes(x = eq_score, y = rt_adj)) + 
    geom_point(alpha = 0.2, pch = 21) + 
    geom_smooth(method = "lm")

learners %>% 
  filter(rt_adj >= -0.1, rt_adj <= 5) %>% 
  ggplot(., aes(x = eq_score, y = rt_adj)) + 
    facet_grid(. ~ speaker_variety) + 
    geom_point(alpha = 0.2, pch = 21) + 
    geom_smooth(method = "lm")

learners %>% 
  filter(rt_adj >= -0.1, rt_adj <= 5) %>% 
  ggplot(., aes(x = eq_score, y = rt_adj, color = sentence_type)) + 
    geom_point(alpha = 0.2, pch = 21) + 
    geom_smooth(method = "lm")

learners %>% 
  filter(rt_adj >= -0.1, rt_adj <= 5) %>% 
  ggplot(., aes(x = eq_score, y = rt_adj)) + 
    facet_grid(. ~ speaker_variety) + 
    geom_point(alpha = 0.2, pch = 21) + 
    geom_smooth(method = "lm", aes(color = sentence_type))


#
# Lextale
#

learners %>% 
  distinct(participant, lextale_avg, lextale_tra) %>% 
  summarize(meanlextale = mean(lextale_avg), sdlextale = sd(lextale_avg), 
    minlextale = min(lextale_avg), maxlextale = max(lextale_avg))

learners %>% 
  distinct(participant, lextale_avg, lextale_tra) %>% 
  summarize(meanlextale = mean(lextale_tra), sdlextale = sd(lextale_tra), 
    minlextale = min(lextale_tra), maxlextale = max(lextale_tra))


learners %>% 
  distinct(participant, lextale_avg, lextale_tra) %>% 
  ggplot(., aes(x = lextale_avg)) + 
    geom_histogram(fill = "grey", color = "black", binwidth = 4.5)

learners %>% 
  distinct(participant, lextale_avg, lextale_tra) %>% 
  ggplot(., aes(x = lextale_tra)) + 
    geom_histogram(fill = "grey", color = "black", binwidth = 5)

learners %>% 
  distinct(participant, lextale_avg, lextale_tra) %>% 
  pivot_longer(cols = -participant, names_to = "lextale", values_to = "score") %>% 
  group_by(lextale) %>% 
  mutate(score_std = scale(score)) %>% 
  ggplot(., aes(x = lextale, y = score)) + 
    geom_boxplot()

learners %>% 
  ggplot(., aes(x = lextale_avg, y = is_correct)) + 
    geom_hline(yintercept = 0.5, size = 3, color = "white") + 
    geom_jitter(alpha = 0.05, height = 0.01, width = 0.5, pch = 21) + 
    geom_smooth(method = "glm", method.args = list(family = "binomial")) + 
    coord_cartesian(ylim = c(0.48, 1))

learners %>% 
  filter(rt_adj >= -0.1, rt_adj <= 5) %>% 
  group_by(participant, lextale_avg) %>% 
  summarize(rt_adj = mean(rt_adj)) %>% 
  ggplot(., aes(x = lextale_avg, y = rt_adj)) + 
    geom_point(alpha = 0.5, pch = 21) + 
    geom_smooth(method = "lm") 

learners %>% 
  ggplot(., aes(x = lextale_avg, y = is_correct)) + 
    facet_grid(. ~ speaker_variety) + 
    geom_jitter(alpha = 0.05, width = 0.4, height = 0.01, pch = 21) + 
    geom_smooth(method = "glm", method.args = list(family = "binomial")) 

learners %>% 
  filter(rt_adj >= -0.1, rt_adj <= 5) %>% 
  group_by(participant, speaker_variety, lextale_avg) %>% 
  summarize(rt_adj = mean(rt_adj)) %>% 
  ggplot(., aes(x = lextale_avg, y = rt_adj)) + 
    facet_grid(. ~ speaker_variety) + 
    geom_point(alpha = 0.5, pch = 21) + 
    geom_smooth(method = "lm") 

learners %>% 
  ggplot(., aes(x = lextale_avg, y = is_correct, color = sentence_type)) + 
    geom_jitter(alpha = 0.1, height = 0.01, width = 0.5, pch = 21) + 
    geom_smooth(method = "glm", method.args = list(family = "binomial"))

learners %>% 
  filter(rt_adj >= -0.1, rt_adj <= 5) %>% 
  group_by(participant, sentence_type, lextale_avg) %>% 
  summarize(rt_adj = mean(rt_adj)) %>% 
  ggplot(., aes(x = lextale_avg, y = rt_adj, color = sentence_type)) + 
    geom_point(alpha = 0.5, pch = 21) + 
    geom_smooth(method = "lm") 

learners %>% 
  ggplot(., aes(x = lextale_avg, y = is_correct, color = sentence_type)) + 
    facet_grid(. ~ speaker_variety) + 
    geom_point() + 
    geom_smooth(method = "glm", se = F, method.args = list(family = "binomial"))

learners %>% 
  filter(rt_adj >= -0.1, rt_adj <= 5) %>% 
  group_by(participant, sentence_type, speaker_variety, lextale_avg) %>% 
  summarize(rt_adj = mean(rt_adj)) %>% 
  ggplot(., aes(x = lextale_avg, y = rt_adj, color = sentence_type)) + 
    facet_grid(. ~ speaker_variety) + 
    geom_point(alpha = 0.5, pch = 21) + 
    geom_smooth(method = "lm") 

#
# Lextale and empathy
#

learners %>% 
  ggplot(., aes(x = eq_score, y = lextale_tra)) + 
    geom_point() + 
    geom_smooth(method = lm)

learners %>% 
  ggplot(., aes(x = eq_score, y = lextale_avg)) + 
    geom_point() + 
    geom_smooth(method = lm)


# Keep/reject?
learners %>% 
  select(participant, check_fails) %>% 
  filter(check_fails != 0) %>% 
  pull(participant) %>% unique()

check_participant(data = learners, id = "midd13")

# Rejected
# 5f4a7225cf944c08a81adca2 (failed attention check, 6min)
# 603454dbbea63e24a99aabd2 (majority negative RTs, 7 min)
# 5fb2caa34a6f4d94967b2748 (all negative RTs)
# 5d5af833f35ed70001e17a5c (all 1's on afc)
# 5dd55364dcec8750a2efc32b (no code)
# 5fc8232ac3b2ae180fa1a39b (took a few vacations)
# 5fc98a7db2180e4b23c6f5be (all 1's on 2afc)
# 5dcd91a0b7ebf606d16c971e (failed attention check, finished in 9min)


# Checked for AC, but ok
# "5ef411225a5e591d15fda2ec"
# "6037034f07abbf728431e247
# "603401117dd0c2000a1cb8e6"
# "6021f6c1f883e82b11739f31"
# "5fb9689421064d7ff5f5243e"
# "5f9710e93c0e4a07c48e98d9" 
# "5ff1e7e39f4b9e67a53f893a" 
# "6021d935ae778527962c9fd3"
