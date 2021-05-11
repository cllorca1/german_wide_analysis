pacman::p_load(readr, dplyr, tidyr, ggplot2)

sensitivity_analysis = read_csv("scenarios/kamil_sa_v2.csv")

#remove duplicates

sensitivity_analysis = sensitivity_analysis %>% distinct(travel_time, transfer_saving, access_egress, .keep_all = T)

sensitivity_analysis = sensitivity_analysis %>% pivot_longer(cols = c(-Version, 
                                                                      -travel_time,
                                                                      -transfer_saving,
                                                                      -access_egress))
sensitivity_analysis = separate(sensitivity_analysis, col = name, into = c("type","purpose","mode","distance","variable"))

sensitivity_analysis = sensitivity_analysis %>%
  filter(variable == "trips") %>% 
  group_by(travel_time, transfer_saving, access_egress, type, purpose, mode) %>%
  summarize(trips = sum(value))

color_modes = c("air" = "gray10", "auto" = "gray50" , "bus" = "gray80", "rail" = "red")

## factor = travel_time

sensitivity_analysis %>%
  filter(transfer_saving == 0, access_egress == 100) %>% 
  ggplot(aes(x = travel_time, y = trips, fill = mode)) +
  geom_line(stat = "identity", position = "fill", color = "#B3B1B0") + 
  geom_bar(stat = "identity", position = "fill") + 
  facet_grid(type ~ purpose) + 
  xlab("Fraction of travel time (%)") + ylab("Modal share") + theme_bw() + 
  scale_y_continuous(expand = c(0,0)) + 
  scale_fill_manual(values = color_modes)

ggsave(filename = "tmp/shares_scenario_3_sa_time.png", device = png(),
       width = 15, height= 10, units = "cm", scale = 1.5)

sensitivity_analysis %>%
  filter(transfer_saving == 0, access_egress == 100, mode == "rail") %>% 
  ggplot(aes(x = travel_time, y = trips, fill = mode)) +
  geom_line(stat = "identity",color = "#B3B1B0") + 
  geom_bar(stat = "identity") + 
  facet_grid(type ~ purpose) + 
  xlab("Fraction of travel time (%)") + ylab("Number of trips") + theme_bw() + 
  scale_y_continuous(expand = c(0,0), lim = c(0,25000)) + 
  scale_fill_manual(values = color_modes)

ggsave(filename = "tmp/trips_scenario_3_sa_time.png", device = png(),
       width = 15, height= 10, units = "cm", scale = 1.5)

## factor = access_egress

sensitivity_analysis %>%
  filter(transfer_saving == 0, travel_time == 100) %>% 
  ggplot(aes(x = access_egress, y = trips, fill = mode)) +
  geom_bar(stat = "identity", position = "fill") + 
  facet_grid(type ~ purpose) + 
  xlab("Reduction of access/egress time (%)") + ylab("Share") + theme_bw() + 
  scale_y_continuous(expand = c(0,0)) + 
  scale_fill_manual(values = color_modes)

sensitivity_analysis %>%
  filter(transfer_saving == 0, travel_time == 100, mode == "rail") %>% 
  ggplot(aes(x = access_egress, y = trips, fill = mode)) +
  geom_bar(stat = "identity") + 
  facet_grid(type ~ purpose) + 
  xlab("Reduction of access/egress time (%)") + ylab("Trips") + theme_bw() + 
  scale_y_continuous(expand = c(0,0), lim = c(0,25000)) + 
  scale_fill_manual(values = color_modes)

## factor = transfer_saving

sensitivity_analysis %>%
  filter(access_egress == 100, travel_time == 100) %>%
  ggplot(aes(x = as.integer(transfer_saving), y = trips, fill = mode)) +
  geom_bar(stat = "identity", position = "fill") + 
  facet_grid(type ~ purpose) + 
  xlab("Time savings per transfer(min)") + ylab("Modal share") + theme_bw() + 
  scale_y_continuous(expand = c(0,0)) + 
  scale_fill_manual(values = color_modes)

ggsave(filename = "tmp/shares_scenario_3_sa_transfers.png", device = png(),
       width = 15, height= 10, units = "cm", scale = 1.5)

sensitivity_analysis %>%
  filter(access_egress == 100, travel_time == 100, mode == "rail") %>% 
  ggplot(aes(x = as.integer(transfer_saving), y = trips, fill = mode)) +
  geom_bar(stat = "identity") + 
  facet_grid(type ~ purpose) + 
  xlab("Time savings per transfer(min)") + ylab("Trips") + theme_bw() + 
  scale_y_continuous(expand = c(0,0), lim = c(0,25000)) + 
  scale_fill_manual(values = color_modes)

ggsave(filename = "tmp/trips_scenario_3_sa_transfers.png", device = png(),
       width = 15, height= 10, units = "cm", scale = 1.5)

