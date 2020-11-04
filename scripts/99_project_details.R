# Project details -------------------------------------------------------------
#
#
#
#
# -----------------------------------------------------------------------------




# Source libs and helpers -----------------------------------------------------

source(here::here("scripts", "01_helpers.R"))

# -----------------------------------------------------------------------------




# Gantt char of project timeline ----------------------------------------------

# This is proof of concept, will change
time_line_df <- tribble(
  ~'wp',                ~'activity',          ~'start_date',  ~'end_date',
  "Investigative team", "Registered report",  "2020-09-16",   "2020-12-15",
  "Investigative team", "Materials prep.",    "2020-09-16",   "2020-10-20",
  "Investigative team", "Recruting",          "2020-12-15",   "2021-02-15",
  "Investigative team", "Analyses",           "2021-06-16",   "2021-08-15",
  "Investigative team", "Registered report",  "2021-08-16",   "2021-09-15",
  "Data teams",         "Data processing",    "2021-02-15",   "2021-03-14",
  "Data teams",         "Reporting",          "2021-05-14",   "2021-06-16"
)

# "Spots" can be used to mark when things actually occur on expected timeline
# Not used for now ("X" implies something is finished)
time_line_spots <- tribble(
  ~'activity',           ~'spot_type', ~'spot_date', 
  "Data visualisation",  "X",         "2020-10-30", 
  "Writing",             "X",         "2020-10-31"
)

# Generate plot 
p_project_timeline <- ganttrify(
  project = time_line_df,
  #spots = time_line_spots,
  by_date = TRUE,
  exact_date = TRUE,
  size_text_relative = 1.2,
  month_number = FALSE,
  font_family = "Times"
)

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
