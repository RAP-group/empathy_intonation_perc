# Helpers ---------------------------------------------------------------------
#
#
#
#
#
# -----------------------------------------------------------------------------




# Source libs -----------------------------------------------------------------

source(here::here("scripts", "00_libs.R"))

# -----------------------------------------------------------------------------




# Analytic functions ----------------------------------------------------------

# Score lextale task
# ((n_corr_real / n_real_words * 100) + (n_corr_nonse / n_nonse_words * 100)) / 2

score_lextale <- function(n_real, n_nonse, n_real_correct, n_nonse_correct, 
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
