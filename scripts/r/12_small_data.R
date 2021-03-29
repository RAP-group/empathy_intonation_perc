



sr %>% 
  pivot_longer(cols = c("speech_rate", "articulation_rate", "avg_syll_dur"), 
  names_to = "metric", values_to = "val") %>% 
  group_by(speaker_variety, metric) %>% 
  summarize(avg_val = mean(val), med_val = median(val), .groups = "drop") %>% 
  pivot_longer(cols = c("avg_val", "med_val"), names_to = "measure", 
    values_to = "val") %>% 
  printy::super_split(speaker_variety, metric)




time <- read_csv(here("data", "raw", "prolific_export_602ab2f3ccc37d12d58ba79b.csv")) %>% 
  select(status, time_taken) %>% 
  filter(status == "APPROVED") %>% 
  mutate(min = time_taken / 60) 

t_mean   <- time$min %>% mean
t_median <- time$min %>% median


time %>% 
  ggplot() + 
    aes(x = min) + 
    geom_histogram(binwidth = 1, color = "black", fill = "grey90") + 
    geom_vline(xintercept = t_median, lty = 3)
