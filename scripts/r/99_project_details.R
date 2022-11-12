# Project details -------------------------------------------------------------
#
# Author: Joseph V. Casillas
# Last update: 20221111
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
 ~"wp",                ~"activity",             ~"start_date",  ~"end_date", 
  "Preparation phase",  "Pre-registration",      "2020-09-16",   "2021-03-15",
  "Preparation phase",  "Materials prep.",       "2020-09-16",   "2021-02-14",
  "Preparation phase",  "Recruting",             "2021-02-17",   "2021-02-25",
  "Preparation phase",  "Recruting",             "2022-10-17",   "2022-10-25",
  "Preparation phase",  "Analyses",              "2021-10-05",   "2021-12-17", 
  "Preparation phase",  "Analyses",              "2022-10-01",   "2022-11-11", 
  "Presentation phase", "HLS",                   "2021-04-01",   "2021-10-07", 
  "Presentation phase", "CASPSLaP",              "2021-10-30",   "2022-02-19", 
  "Manuscript phase",   "V1",                    "2021-10-23",   "2022-02-03", 
  "Manuscript phase",   "Review 1",              "2022-02-03",   "2022-07-13", 
  "Manuscript phase",   "V2",                    "2022-07-13",   "2022-11-11", 
)

# "Spots" can be used to mark when things actually occur on expected timeline
# Not used for now ("X" implies something is finished)
time_line_spots <- tribble(
  ~"activity",          ~"spot_type",    ~"spot_date", 
   "HLS",                "deadline",      "2021-04-28", 
   "HLS",                "presentation",  "2021-10-07", 
   "CASPSLaP",           "deadline",      "2021-10-30", 
   "CASPSLaP",           "presentation",  "2022-02-19",
   "V1",                 "submit",        "2022-02-03", 
   "V2",                 "start",       "2022-10-01", 
   "V2",                 "submit",        "2022-11-11", 
)

# Generate plot 
p_project_timeline <- ganttrify(
  project = time_line_df,
  spots = time_line_spots,
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
  "RE"  = tibble(role = c(1, 5, 6, 14),       weight = "high"),
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
                       height = 4, width = 7, units = "in"))

# -----------------------------------------------------------------------------
