# Helpers ---------------------------------------------------------------------
#
#
#
#
#
# -----------------------------------------------------------------------------




# Source libs -----------------------------------------------------------------

source(here::here("scripts", "r", "00_libs.R"))

# -----------------------------------------------------------------------------




# Analytic functions ----------------------------------------------------------

# Score lextale task
# ((n_corr_real / n_real_words * 100) + (n_corr_nonse / n_nonse_words * 100)) / 2

score_lextale <- function(
  n_real = NULL, 
  n_nonse = NULL, 
  n_real_correct = NULL, 
  n_nonse_correct = NULL, 
  n_nonse_incorrect = NULL) {

  if (is.null(n_nonse_incorrect)) {
  avg_real <-  (n_real_correct / n_real * 100)
  avg_nonse <- (n_nonse_correct / n_nonse * 100)
  val <- (avg_real + avg_nonse) / 2
  } else {
  val <- n_real_correct - (2 * n_nonse_incorrect)
  }
  return(val)
}

# Calculate difference in days from origin
time_diff <- function(date = "1982-11-03", format = "%Y-%m-%d", 
                      origin = "1970-01-01", units = "days") {
  start_date <- as.Date(origin, format)
  end_date   <- as.Date(date, format)
  out <- as.numeric(difftime(end_date, start_date, units = units))
  return(out)
}

# Calculate binwidth for histograms
fd_bw <- function(x) {
  out <- 2 * IQR(x) / length(x)^(1/3)
  return(out)
}

# -----------------------------------------------------------------------------




# Plotting functions ----------------------------------------------------------

minimal_adj <- function(...) {
  list(
    theme_minimal(base_size = 12, base_family = "Times"), 
    theme(
      axis.title.y = element_text(size = rel(.9), hjust = 0.95), 
      axis.title.x = element_text(size = rel(.9), hjust = 0.95),
      panel.grid.major = element_line(colour = 'grey90', size = 0.15),
      panel.grid.minor = element_line(colour = 'grey90', size = 0.15))
  )
}

# Concert '-' to unicode minus
unicode_minus <- function(x) {
  sub('^-', '\U2212', format(x))
}

# Strip blank space
strip_blank <- function(x) {
  sub('[[:space:]]+', '', format(x))
}


# Participant check
check_participant <- function(data = "learners", id) {

  p1_accuracy <- data %>% 
    filter(participant == id) %>% 
    ggplot(., aes(x = participant, y = is_correct)) + 
      stat_summary(fun.data = mean_se, geom = "pointrange", 
        aes(color = sentence_type), position = position_dodge(0.25)
        ) + 
      geom_text(aes(label = lextale_tra, x = 1.4, y = 0.5)) + 
      geom_text(aes(label = eq_score, x = 0.6, y = 0.5)) + 
      coord_cartesian(ylim = c(0, 1))

  p2_rts <- data %>% 
    filter(participant == id) %>% 
    ggplot(., aes(x = participant, y = rt_adj)) + 
      geom_hline(yintercept = 0, size = 3, color = "white") + 
      geom_jitter(alpha = 0.5, width = 0.2, 
        aes(color = factor(is_correct))) + 
      geom_text(nudge_x = -0.35, 
        aes(label = ifelse(is_correct == 0, speaker_variety, ''))) + 
      stat_summary(fun.data = mean_se, geom = "pointrange") + 
      scale_color_brewer(name = NULL, palette = "Set1", 
        labels = c("incorrect", "correct")) + 
      geom_text(aes(x = 1.4, y = 0.1, label = spn_variety), alpha = 0.02)

  print(p1_accuracy + p2_rts)

}

# -----------------------------------------------------------------------------
















#forest_re <- function(mod, effect, quoted_var) {
  
#  ef   <- enquo(effect)
#  efn  <- enquo(quoted_var)

  # Get draws for each re
#  re_draws <- 
#  spread_draws(mod, !!!efn[!!effect,], b_Intercept) %>% 
#  mutate(b_Intercept = !!!efn + b_Intercept)

#  return(re_draws)
  # Get draws for pooled effect
  #re_pooled_effect_draws <- 
  #spread_draws(mod, b_Intercept) %>% 
  #mutate(effect = "Pooled Effect")

  # Combine it and clean up
  #re_forest_data <- 
  #bind_rows(re_draws, re_pooled_effect_draws) %>% 
  #ungroup() %>%
  #mutate(effect = reorder(effect, b_Intercept), 
  #       effect = relevel(effect, "Pooled Effect", after = Inf))

  # Calculate mean qi intervals for right margin text
  #re_forest_data_summary <- 
  #group_by(re_forest_data, effect) %>% 
  #mean_qi(b_Intercept, .width = 0.95) 

  # Calculate mean qi intervals for pooled effect
  #re_pooled_summary <- 
  #group_by(re_forest_data, effect) %>% 
  #mean_qi(b_Intercept, .width = c(0.5, 0.8, 0.95)) %>% 
  #filter(effect == "Pooled Effect")

  # Plot it all
  #p_post <- re_forest_data %>% 
  #ggplot() + 
  #aes(x = b_Intercept, y = effect) + 
  #geom_text(data = 
  #  mutate_if(re_forest_data_summary, is.numeric, round, 2) %>% 
  #  mutate_at(c("b_Intercept", ".lower", ".upper"), as.character) %>% 
  #  mutate_at(c("b_Intercept", ".lower", ".upper"), unicode_minus) %>% 
  #  mutate_at(c("b_Intercept", ".lower", ".upper"), strip_blank),
  #          aes(label = glue("{b_Intercept} [{.lower}, {.upper}]"), x = Inf), 
  #          hjust = "inward", family = "Times") + 
  #geom_tile(data = re_pooled_summary, aes(width = .lower - .upper),
  #  alpha = 0.2, height = Inf, fill = "#31688EFF") +
  #stat_pointinterval(point_fill = "white", shape = 21, .width = c(0.8, 0.95)) +
  #coord_cartesian(xlim = c(0, 6)) + 
  #labs(x = expression(italic("beta")), y = NULL) +
  #minimal_adj() + 
  #theme(axis.text.y = element_text(hjust = 0))

  #return(p_post)
#}

#forest_re(native_response_00, "speaker_variety", speaker_variety)


