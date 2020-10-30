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

# Vector of researchers
researchers <- c("JVC", "JGP", "NR", "KP", "LFA", "RE", "IC", "KG")

# Vector of contributor roles
contributor_roles <- c(
  "Conceptualization", "Methodology", "Software", "Validation", 
  "Formal analysis", "Investigation", "Resources", "Data curation", 
  "Writing - original draft preparation", "Writing - review and editing", 
  "Visualization", "Supervision", "Project Administration", 
  "Funding acquisition"
)

# Create df and tidy
contributor_df <- tribble(
~"Contributor role",                    ~"JVC", ~"JGP", ~"NR", ~"KP", ~"LFA", ~"RE", ~"IC", ~"KG", 
  "Conceptualization",                       1,      1,     1,     1,      1,     1,     1,     1,
  "Methodology",                             1,      1,     1,     1,      1,     1,     0,     0,
  "Software",                                1,      0,     0,     0,      0,     0,     0,     0,
  "Validation",                              1,      0,     0,     0,      0,     0,     0,     0,
  "Formal analysis",                         1,      1,     1,     1,      1,     1,     0,     0,
  "Investigation",                           0,      1,     1,     1,      1,     1,     0,     0,
  "Resources",                               1,      1,     1,     1,      1,     1,     1,     1,
  "Data curation",                           1,      0,     0,     1,      0,     0,     0,     0,
  "Writing - original draft preparation",    1,      1,     1,     1,      1,     0,     1,     1,
  "Writing - review and editing",            0,      1,     1,     1,      1,     1,     1,     1,
  "Visualization",                           1,      0,     0,     0,      0,     0,     0,     0,
  "Supervision",                             1,      0,     0,     0,      0,     0,     0,     0,
  "Project Administration",                  1,      0,     0,     0,      0,     0,     0,     0,
  "Funding acquisition",                     1,      0,     0,     0,      0,     0,     0,     0
  ) %>% 
  pivot_longer(cols = JVC:KG, names_to = "names", values_to = "vals") %>% 
  group_by(`Contributor role`) %>% 
  mutate(order_x = seq_along(names)) %>% 
  group_by(names) %>% 
  mutate(order_y = seq_along(`Contributor role`)) %>% 
  ungroup() %>% 
  mutate(
    `Contributor role` = fct_reorder2(`Contributor role`, order_y, order_y),
     names = fct_reorder(names, order_x, min))

p_project_contributors <- contributor_df%>% 
  ggplot(., aes(x = names, y = `Contributor role`)) + 
    geom_point(aes(alpha = if_else(vals == 1, 1, 0)), size = 3.5, pch = 18, 
      show.legend = F) + 
    scale_x_discrete(position = "top") + 
    labs(x = "Contributors", 
         caption = "Author contributions based on CRediT taxonomy") + 
    minimal_adj()

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
