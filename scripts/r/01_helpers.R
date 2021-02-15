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

# -----------------------------------------------------------------------------
