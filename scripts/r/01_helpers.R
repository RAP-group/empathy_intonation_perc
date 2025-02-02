# Helpers ---------------------------------------------------------------------
#
# Author: Joseph V. Casillas
# LAST UPDATE: 10/12/2022
# - Helper function using for analyses, plotting and reporting results in
#   the project manuscript
# - This file is sourced automatically when the data are loaded via 
#   07_load_data.r
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

# Calculate dprime
dprime <- function(n_hit, n_fa, n_miss = NULL, n_cr = NULL, n_targets = NULL,
                   n_distractors = NULL, adjusted = TRUE) {

  # Get number of targets if not provided
  if (is.null(n_targets)) {
    n_targets <- n_hit + n_miss
  }

  # Get number of distractors is not provided
  if (is.null(n_distractors)) {
    n_distractors <- n_fa + n_cr
  }

  # Adjust hit rate and false alarm rate if there are extreme values
  # (Hautus, 1995)
  if (adjusted == TRUE) {
    if (is.null(n_miss) | is.null(n_cr)) {

      # Stop because of missing info
      warning("You need to include n_miss and n_cr to compute adjusted ratios.
               Computing indices anyway with non-adjusted ratios...")

      # Calculate non-adjusted hit rate and fa rate
      hit_rate_adjusted <- n_hit / n_targets
      fa_rate_adjusted  <- n_fa / n_distractors

    } else {

      # Calculate adjusted rate
      hit_rate_adjusted <- (n_hit + 0.5) / (n_hit + n_miss + 1)
      fa_rate_adjusted  <- (n_fa + 0.5) / (n_fa + n_cr + 1)

    }

    # Calculate adjusted dprime
    dprime <- qnorm(hit_rate_adjusted) - qnorm(fa_rate_adjusted)

  } else {

    # Calculate non-adjusted dprime
    hit_rate <- n_hit / n_targets
    fa_rate <- n_fa / n_distractors
    dprime <- qnorm(hit_rate) - qnorm(fa_rate)

  }

  # If there are 0 false alarms and correct rejections (distractors) then
  # it is impossible to calculate dprime
  if (any(n_distractors == 0)) {
    warning("There are observations with 0 distractors.
             Not possible to compute dprime :(")
    return(dprime)
  }

  return(dprime)

}

# DDM simulation function
sim_ddm <- function(q_type, eq, lt, drift_rate, boundary_separation, 
  bias, ndt, n_sims, seed = NULL) {
  set.seed(seed)
  simd = NULL
  for (sim in 1:n_sims){
    step = 1
    value = bias
    n = 1
    while (abs(value[n]) < boundary_separation) {
      n = n + 1
      value[n] = (value[n - 1] + rnorm(1, 0, abs(drift_rate)))
      step[n] = n
    }
    dd <- tibble(q_type = q_type, eq = eq, lt = lt, sim_n = sim, step, value) %>% 
    mutate(
      sim_n = as.factor(sim_n), 
      value = case_when(
        value > boundary_separation ~ boundary_separation, 
        value < -boundary_separation ~ -boundary_separation, 
        TRUE ~ .$value)
      )
    simd = rbind(simd, dd)
  }
  return(simd)
}

# Load all rds files in a directory
load_models <- function(path, obj_type) { 
  
  # Get object names
  obj_reg <- paste0("\\.", obj_type, "$")
  the_names <- list.files(path = path, pattern = obj_reg) %>% 
  str_remove(paste0(".", obj_type))

  # Get objects
  the_objs <- dir_ls(path = path, regexp = obj_reg) %>%
  map(readRDS)

  # Rename objects
  names(the_objs) <- the_names
  return(the_objs)
}

# simple scale
simple_scale <- function(x) {
  out <- (x - mean(x, na.rm = TRUE)) / sd(x, na.rm = TRUE)
  return(out)
}

# -----------------------------------------------------------------------------




# Plotting functions ----------------------------------------------------------

# Calculate binwidth for histograms
fd_bw <- function(x) {
  out <- 2 * IQR(x) / length(x)^(1/3)
  return(out)
}

minimal_adj <- function(...) {
  list(
    theme_minimal(base_size = 12, base_family = "Times"), 
    theme(
      axis.title.y = element_text(hjust = 0.95), 
      axis.title.x = element_text(hjust = 0.95),
      panel.grid.major = element_line(colour = 'grey90', size = 0.15),
      panel.grid.minor = element_line(colour = 'grey90', size = 0.15))
  )
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




# Printing functions ----------------------------------------------------------

# Convert '-' to unicode minus
unicode_minus <- function(x) {
  sub('^-', '\U2212', format(x))
}

# Strip blank space
strip_blank <- function(x) {
  sub('[[:space:]]+', '', format(x))
}

# Round and format numbers to exactly N digits
specify_decimal <- function(x, k) {
  out <- trimws(format(round(x, k), nsmall = k))
  return(out)
}


# Print values from tibble for article
pull_from_tib <- function(df, col, row, val) {
  col <- enquo(col)
  row <- enquo(row)
  val <- enquo(val)
  val <- filter(df, !!col == !!row) %>% pull(!!val)
  return(val)
}

# Report estimate from posterior distribution summary
report_posterior <- function(df, param, is_exp = TRUE, mod = NULL) {
  
  if (is_exp == TRUE) {
    
    # Extract wanted value from model output
    est  <- df[df$Parameter == param, "Median"]
    cis  <- df[df$Parameter == param, "HDI"]
    rope <- df[df$Parameter == param, "% in ROPE"]
    mpe  <- df[df$Parameter == param, "MPE"]

    capture.output(
      paste0("(&beta; = ", est, ", HDI = ", cis, ", ROPE = ", rope, 
             ", MPE = ", mpe, ")", "\n") %>% 
        cat()) %>% 
        paste()
  } else {
    # Extract wanted value from model output
    est  <- df[df$Parameter == param & df$Model == mod, "Median"]
    cis  <- df[df$Parameter == param & df$Model == mod, "HDI"]
    mpe  <- df[df$Parameter == param & df$Model == mod, "MPE"]

    capture.output(
      paste0("(&beta; = ", est, ", HDI = ", cis, ", MPE = ", mpe, ")", 
             "\n") %>% 
        cat()) %>% 
        paste()
  }
}

# Report manually calculated effects (takes a col of a df)
my_posterior_summary <- . %>% 
  pull() %>% 
  describe_posterior(., rope_range = c(-0.1, 0.1)) %>% 
  as_tibble() %>% 
  select(-c("CI", "ROPE_CI", "ROPE_low", "ROPE_high")) %>% 
  mutate(across(-c("Parameter"), specify_decimal, k = 2)) %>% 
  mutate(across(-Parameter, printy::fmt_minus_sign)) %>% 
  mutate(HDI = glue("[{CI_low}, {CI_high}]")) %>% 
  select(Parameter, Median, HDI, `% in ROPE` = ROPE_Percentage, MPE = pd) %>% 
  report_posterior(., param = "Posterior")

# -----------------------------------------------------------------------------
