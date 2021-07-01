pacman::p_load(data.table, dplyr, ggplot2, tidyr, readr, plotly, sf, tmap, rgeos, RColorBrewer)


results = read_csv("scenarios/results_congestion.csv")

results_wide = results %>% pivot_wider(names_from = alt, values_from = c(distance_m, time_s))

results_wide %>%
  sample_frac(0.1) %>%
  ggplot(aes(x = time_s_base/3600, y = time_s_congested/3600)) +
  geom_point(alpha = .1) + 
  geom_abline(slope = 1, intercept = 0, color = "red") + 
  theme_bw() + 
  xlab("Time in free flow conditions (h)") + ylab("Time under congested conditions (h)")

results_wide %>% 
  sample_frac(0.1) %>%
  mutate(perc_delay = (time_s_congested - time_s_base)/time_s_base * 100) %>% 
  ggplot(aes(x = perc_delay)) + stat_ecdf() + coord_cartesian(xlim = c(0,100)) + 
  theme_bw() + 
  xlab("Delay (%)") + ylab("Frequency")

results_wide %>% 
  sample_frac(0.1) %>%
  mutate(perc_delay = (time_s_congested - time_s_base)/time_s_base * 100) %>% 
  ggplot(aes(x = time_s_base/3600, y = perc_delay)) +
  geom_point(alpha = 0.1) +
  theme_bw() + xlim(0,10) +
  xlab("Travel time in free flow conditions (h)") + ylab("Delay (%)") + ylim(0,100)

results_wide %>% 
  sample_frac(0.1) %>%
  mutate(delay = time_s_congested - time_s_base) %>% 
  ggplot(aes(x = delay/3600)) +
  stat_ecdf() + 
  coord_cartesian(xlim = c(0,2)) +
  theme_bw() + 
  xlab("Delay (h)") + ylab("Frequency") 
