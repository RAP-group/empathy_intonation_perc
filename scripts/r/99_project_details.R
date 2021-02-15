# Project details -------------------------------------------------------------
#
#
#
#
# -----------------------------------------------------------------------------




# Source libraries ------------------------------------------------------------

source(here::here("scripts", "r","01_helpers.R"))

# -----------------------------------------------------------------------------



# Gantt char of project timeline ----------------------------------------------

# This is proof of concept, will change
time_line_df <- tribble(
 ~"wp",          ~"activity",             ~"start_date",  ~"end_date",
  "Preparation",  "Registered report",     "2020-09-16",   "2021-03-15",
  "Preparation",  "Materials prep.",       "2020-09-16",   "2021-02-14",
  "Preparation",  "Recruting",             "2021-02-17",   "2021-02-25",
  "Preparation",  "Analyses",              "2021-02-25",   "2021-03-01", 
  "Presentation", "Manuscript prep.",      "2021-03-15",   "2021-05-01", 
  "Presentation", "Manuscript submission", "2021-05-01",   "2021-05-10", 
  "Presentation", "Abstract submission",   "2021-04-01",   "2021-04-28"
)

# "Spots" can be used to mark when things actually occur on expected timeline
# Not used for now ("X" implies something is finished)
time_line_spots <- tribble(
  ~"activity",           ~"spot_type", ~"spot_date", 
   "Materials prep.",     "X",          "2021-02-13", 
   #"Writing",             "X",          "2020-10-31"
)

# Generate plot 
p_project_timeline <- ganttrify(
  project = time_line_df,
  spots = time_line_spots,
  by_date = TRUE,
  exact_date = TRUE,
  size_text_relative = 1.2,
  month_number = FALSE,
  font_family = "Times") + 
  geom_vline(xintercept = time_diff(date = Sys.Date()), lty = 3)

# -----------------------------------------------------------------------------




# Contributors plot -----------------------------------------------------------

# Create df 
contributor_list <- list(
  "JVC" = c(1), 
  "JGP" = c(1), 
  "NR"  = c(1), 
  "KP"  = c(1), 
  "LFA" = c(1), 
  "RE"  = c(1), 
  "IC"  = c(1), 
  "KG"  = c(1)
)

p_project_contributors <- contributor(contributor_list)

# -----------------------------------------------------------------------------




# Save plots ------------------------------------------------------------------

devices           <- c('pdf', 'png')
path_gantt        <- file.path(here("figs"), "project_timeline.")
path_contributors <- file.path(here("figs"), "project_contributors.")

walk(devices, ~ ggsave(filename = glue(path_gantt, .x), 
                       plot = p_project_timeline, device = .x, 
                       height = 5, width = 9, units = "in"))

walk(devices, ~ ggsave(filename = glue(path_contributors, .x), 
                       plot = p_project_contributors, device = .x, 
                       height = 5, width = 9, units = "in"))

# -----------------------------------------------------------------------------
