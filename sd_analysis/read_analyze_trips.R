pacman::p_load(readr, dplyr, tidyr, ggplot2)

trips_sd = read_csv("c:/models/mito/germany/scenOutput/testDCCalibr/2011/microData/trips.csv")

trips_sd %>% ggplot(aes(x = distance, color = purpose)) +
  stat_ecdf(size = 1) + scale_x_log10()


trips_sd %>% 
  mutate(distance_40 = if_else(distance < 40, distance, NULL)) %>%
  mutate(distance_greater = if_else(distance > 40, 1, 0)) %>%
  group_by(purpose) %>%
  summarize(mean(distance), mean(distance_40, na.rm = T), sum(distance_greater)/n())

summary = trips_sd %>% group_by(mode, purpose) %>% summarize(count = n(), mean(distance))


summary %>%
  ggplot(aes(x = purpose, fill  =mode, y = count)) + geom_bar(stat = "identity", position = "fill")


trips_sd %>% ggplot(aes(x = departure_time/60, color = purpose)) +
  geom_freqpoly(size = 1) + xlim(0,24)
  