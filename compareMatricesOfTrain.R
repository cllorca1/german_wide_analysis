pacman::p_load(readr, dplyr, tidyr, ggplot2)

data_analysis = read_csv("scenarios/results_scen_1.csv")
data_analysis = data_analysis %>% mutate(access_time_2 = access_time_2 - 8*3600)


#smaller dataset
sub_data_analysis = data_analysis[sample(nrow(data_analysis), 10000), ]

sub_data_analysis %>% 
ggplot(aes(x=egress_time_1, y = egress_time_2)) +
  geom_point(alpha = 0.01) + geom_abline(intercept = 0, slope = 1, color = "red") + 
  xlim(0,30000) + ylim(0,30000)


sub_data_analysis %>% 
  ggplot(aes(x=access_time_1, y = access_time_2 )) +
  geom_point(alpha = 0.01) + geom_abline(intercept = 0, slope = 1, color = "red") + 
  xlim(0,30000) + ylim(0,30000)


data_analysis_1 = data_analysis %>% select(o,d, t = time_1, a = access_time_1, e = egress_time_1) %>%
  mutate(case = "walk_opnv")
data_analysis_2 = data_analysis %>% select(o,d, t = time_2, a = access_time_2, e = egress_time_2) %>%
  mutate(case = "car")

data_analysis_long = data_analysis_1 %>% bind_rows(data_analysis_2) %>% pivot_longer(cols = c(t, a, e))


data_analysis_long %>% filter(value < 24*3600) %>%
  ggplot(aes(x = value/3600, color = case)) + geom_freqpoly() +
  facet_grid(name~., scales = "free") + theme_bw()

data_analysis_long %>% group_by(o, d, case) %>% summarise(value = sum(value)) %>% 
  filter(value < 24*3600) %>%
  ggplot(aes(x = value/3600,..density.., color = case)) + geom_freqpoly(size
                                                                        = 1) +
  theme_bw() + xlab("Travel time (h)") + ylab("Frequency")

ggsave(filename = "tmp/travel_time_scenario_1.png", device = png(),
       width = 15, height= 10, units = "cm", scale = 1.5)
