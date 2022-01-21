# Project details -------------------------------------------------------------
#
# Author: Joseph V. Casillas
# Last update: 20211221
#
# - This scripts provides a Gantt chart of the project timeline
#   and a CREDiT contributors plot
#
# -----------------------------------------------------------------------------




# Source libraries ------------------------------------------------------------

source(here::here("scripts", "r","01_helpers.R"))

# -----------------------------------------------------------------------------



# Gantt char of project timeline ----------------------------------------------

# This is proof of concept, will change
time_line_df <- tribble(
 ~"wp",          ~"activity",              ~"start_date",  ~"end_date", 
#  "Preparation",  "Winter break",          "2020-12-20",   "2021-01-08", 
#  "Preparation",  "Winter break",          "2021-12-20",   "2022-01-08", 
#  "Preparation",  "Summer break",          "2021-06-01",   "2021-10-01", 
  "Preparation",  "Pre-registration",      "2020-09-16",   "2021-03-15",
  "Preparation",  "Materials prep.",       "2020-09-16",   "2021-02-14",
  "Preparation",  "Recruting",             "2021-02-17",   "2021-02-25",
  "Preparation",  "Analyses",              "2021-12-12",   "2021-12-17", 
  "Presentation", "Manuscript prep.",      "2021-10-23",   "2021-12-01", 
  "Presentation", "Manuscript submission", "2021-12-21",   "2021-12-21", 
  "Presentation", "Deadline - HLS",        "2021-04-01",   "2021-04-28", 
  "Presentation", "Conference - HLS",      "2021-10-07",   "2021-10-07", 
  "Presentation", "Deadline - CASPSLaP",   "2021-10-30",   "2021-10-30", 
  "Presentation", "Conference - CASPSLaP", "2022-02-17",   "2022-02-19", 
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
 # spots = time_line_spots,
  by_date = TRUE,
  exact_date = TRUE,
  size_text_relative = 1,
  month_number = FALSE,
  font_family = "Times") + 
  geom_vline(xintercept = time_diff(date = Sys.Date()), lty = 3)

# -----------------------------------------------------------------------------




# Contributors plot -----------------------------------------------------------

# Create df 
contributor_list <- list(
  "JVC" = tibble(role = 1:14,                 weight = "high"),
  "JGP" = tibble(role = c(1, 2, 5, 6, 13:14), weight = "high"),
  "NR"  = tibble(role = c(1, 5, 6, 13:14),    weight = c(rep("high", 4), "low")),
  "KP"  = tibble(role = c(1, 5, 6, 13:14),    weight = "high"),
  "LFA" = tibble(role = c(1, 5, 6, 13:14),    weight = "high"),
  "RE"  = tibble(role = c(1, 5, 6),           weight = "high"),
  "IC"  = tibble(role = c(1, 5, 6, 13:14),    weight = "high"),
  "KG"  = tibble(role = c(1, 5, 6, 13),       weight = "high"),
  "GC"  = tibble(role = 13:14,                weight = "low"),
  "JS"  = tibble(role = 13:14,                weight = "low"),
  "IA"  = tibble(role = 13:14,                weight = "low"),
  "KT"  = tibble(role = 14,                   weight = "low")
)

p_project_contributors <- contributor(
  contributor_list, weight = T, begin = 0.2, end = 0.8)

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
