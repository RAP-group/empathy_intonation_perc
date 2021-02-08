# -

# Source libs  and helpers ----------------------------------------------------

source(here::here("scripts", "r","01_helpers.R"))

# -----------------------------------------------------------------------------


# Load Speech rate data -------------------------------------------------------

sr <- read_csv(here("data", "raw", "speech_rate", "speech_rate_raw.csv")) %>% 
  separate(
    col = soundname, 
    into = c("variety", "condition", "sentence_type", "item"), sep = "_"
    ) %>%
  rename(
    speech_rate = `speechrate (nsyll/dur)`, 
    articulation_rate = `articulation rate (nsyll / phonationtime)`, 
    avg_syll_dur = `ASD (speakingtime/nsyll)`
    ) %>% 
  mutate(variety = recode(variety, "puertorican" = "Puerto Rican")) %>%  
  mutate(variety = tools::toTitleCase(variety)) 

sr_desc <- sr %>% 
  pivot_longer(cols = c("speech_rate", "articulation_rate", "avg_syll_dur"), 
  names_to = "metric", values_to = "val") %>% 
  group_by(variety, metric) %>% 
  summarize(avg_val = mean(val), med_val = median(val), .groups = "drop") %>% 
  pivot_longer(cols = c("avg_val", "med_val"), names_to = "measure", 
    values_to = "val")
  #printy::super_split(variety, metric)

sr %>% 
  ggplot() + 
  aes(x = speech_rate) + 
  facet_wrap(~ variety, nrow = 2) + 
  geom_histogram(fill = "lightblue", color = "black", binwidth = 0.55, 
    aes(y = ..density..)) + 
  geom_density(size = 1, color = "darkred") + 
  geom_vline(data = filter(sr_desc, metric == "speech_rate"), 
    aes(xintercept = val, color = measure)) + 
  scale_color_viridis_d(name = NULL, option = "D", end = 0.8, 
                        labels = c("Mean", "Median")) + 
  labs(x = "Speech rate", y = "Density") + 
  ds4ling::ds4ling_bw_theme(base_family = "Times", base_size = 16)

sr %>% 
  pivot_longer(cols = c("speech_rate", "articulation_rate", "avg_syll_dur"), 
    names_to = "metric", values_to = "val") %>% 
  group_by(metric) %>% 
  mutate(val_std = (val - mean(val)) / sd(val), 
         variety = fct_relevel(variety, rev)) %>% 
  filter(metric == "speech_rate") %>% 
  ggplot() + 
  aes(x = val_std, y = variety, color = variety) + 
  geom_vline(xintercept = 0, lty = 3) + 
  geom_beeswarm(alpha = 0.2, groupOnX = F, show.legend = F) + 
  stat_summary(fun.data = mean_cl_normal, geom = "pointrange", show.legend = F, 
    pch = 21, color = "black", size = 1.1, aes(fill = variety)) + 
  scale_color_viridis_d(option = "D", end = 0.9) + 
  scale_fill_viridis_d(option = "D", end = 0.9) + 
  coord_cartesian(xlim = c(-2.5, 2.5)) + 
  labs(x = "Speech rate (std)", y = "Spanish variety") + 
  ds4ling::ds4ling_bw_theme(base_family = "Times", base_size = 16)


