pacman::p_load(readr, dplyr, tidyr, ggplot2)

counts_folder = "C:/Users/carlloga/LRZ Sync+Share/bast_EntlastungBundesfernstraßen (Rolf Moeckel)/detectors/counts_bast_2011/"

counts_observed = read_csv(paste(counts_folder, "weekday_counts_by_station_hour.csv", sep  =""))


counts_observed_1_car = counts_observed %>% select(station = station_zst, 
                                             hour,
                                             vehicles = pkw_1,
                                             road_type,
                                             longitude, 
                                             latitude,
                                             osm = osm_link_1,
                                             direction = direction_1) %>%
  mutate(direction_index = 1, veh_type = "car")

counts_observed_1_truck = counts_observed %>% select(station = station_zst, 
                                                   hour,
                                                   vehicles = lkw_1,
                                                   road_type,
                                                   longitude, 
                                                   latitude,
                                                   osm = osm_link_1,
                                                   direction = direction_1) %>%
  mutate(direction_index = 1, veh_type = "truck")

counts_observed_2_car = counts_observed %>% select(station = station_zst, 
                                               hour,
                                               vehicles = pkw_2,
                                               road_type,
                                               longitude, 
                                               latitude,
                                               osm = osm_link_2,
                                               direction = direction_2) %>%
  mutate(direction_index = 2, veh_type = "car")

counts_observed_2_truck = counts_observed %>% select(station = station_zst, 
                                                   hour,
                                                   vehicles = lkw_2,
                                                   road_type,
                                                   longitude, 
                                                   latitude,
                                                   osm = osm_link_2,
                                                   direction = direction_2) %>%
  mutate(direction_index = 2, veh_type = "truck")

counts_observed_long = counts_observed_1_car %>%
  bind_rows(counts_observed_1_truck)%>%
  bind_rows(counts_observed_2_car)%>%
  bind_rows(counts_observed_2_truck)

rm(counts_observed, counts_observed_1_car, counts_observed_1_truck, counts_observed_2_car, counts_observed_2_truck)


#write_csv(counts_observed_long, paste(counts_folder, "weekday_counts_by_station_hour_long.csv", sep  =""))
names(counts_observed_long)

model_folder = "c:/models/germanymodel/matsim/"

scenarios = c("sd_passenger_1_20210504", "ld_passenger_1_20210505","ld_trucks")
veh_types = c("car_sd", "car_ld","truck")

#scenarios = c("combined_hermes_20210506","ld_trucks")
#veh_types = c("car","truck")

simulated_counts = data.frame()
for (i in 1:length(scenarios)){
  scenario = scenarios[i]
  this_simulated_counts = read_csv(paste(model_folder, "output/", scenario, "/counts.csv", sep  =""))
  simulated_counts = simulated_counts %>%
    bind_rows(this_simulated_counts %>%
                mutate(veh_type = veh_types[i], vehicles = car + truck) %>% select(link, hour, length, type, vehicles, veh_type))
  rm(this_simulated_counts)
}

#add every count into one day (hours will be repeated!)
simulated_counts = simulated_counts %>%
  mutate(hour = if_else(hour >= 48, hour - 48, if_else(hour >= 24, hour - 24, hour)))



##stations and links 
station_links_2011 = read_csv(paste(model_folder, "bast_station_links_network_2011.csv", sep =""))

simulated_counts = simulated_counts %>% left_join(station_links_2011, by = c("link" = "linkId"))


vehicle_colors = c("car" = "#5d86ec","car_ld" = "#415da5", "car_sd" = "#7d9eef" ,"truck" = "#a0bd7f")

scale = 100


#### observed vs simulated daily

sim = simulated_counts %>%
  mutate(is_truck = if_else(veh_type == "truck", "truck", "car")) %>%
  group_by(station = stationId, is_truck, direction ) %>%
  summarize(count = sum(vehicles) * scale) %>%
  mutate(source = "sim")
obs = counts_observed_long %>%
  mutate(is_truck = if_else(veh_type == "truck", "truck", "car")) %>%
  group_by(station, is_truck, direction = direction_index) %>%
  summarise(count = sum(vehicles)) %>%
  mutate(source = "obs")
both = sim %>% bind_rows(obs)


both %>% pivot_wider(values_from = count, names_from = source) %>% 
  ggplot(aes(x = obs, y = sim, color = is_truck)) + geom_point() + 
  geom_abline(intercept = 0, slope = 1, color = "black", size = 1) + 
  theme_bw() +
  scale_color_manual(values = vehicle_colors) + 
  scale_x_log10() + scale_y_log10() + 
  xlab("Observed traffic (veh/day/direction) - log10") +
  ylab("Simulated traffic (veh/day/direction) - log10")


both %>% pivot_wider(values_from = count, names_from = source) %>% 
  ggplot(aes(x = obs, y = sim, color = is_truck)) + geom_point() + 
  geom_abline(intercept = 0, slope = 1, color = "black", size = 1) + 
  geom_abline(intercept = 0, slope = 0.75, color = "black", size = 1) + 
  geom_abline(intercept = 0, slope = 1.25, color = "black", size = 1) + 
  theme_bw() +
  scale_color_manual(values = vehicle_colors) + 
  xlab("Observed traffic (veh/day/direction)") +
  ylab("Simulated traffic (veh/day/direction)")
  

#### observed vs simulated hourly

sim = simulated_counts %>%
  mutate(is_truck = if_else(veh_type == "truck", "truck", "car")) %>%
  group_by(station = stationId, is_truck, direction, hour ) %>%
  summarize(count = sum(vehicles) * scale) %>%
  mutate(source = "sim")
obs = counts_observed_long %>%
  mutate(is_truck = if_else(veh_type == "truck", "truck", "car")) %>%
  group_by(station, is_truck, direction = direction_index, hour) %>%
  summarise(count = sum(vehicles)) %>%
  mutate(source = "obs")
both = sim %>% bind_rows(obs)


both %>% pivot_wider(values_from = count, names_from = source) %>% 
  ggplot(aes(x = obs, y = sim, color = is_truck)) + geom_point() + 
  geom_abline(intercept = 0, slope = 1, color = "black", size = 1) + 
  geom_abline(intercept = 0, slope = 0.75, color = "black", size = 1) + 
  geom_abline(intercept = 0, slope = 1.25, color = "black", size = 1) + 
  theme_bw() +
  scale_color_manual(values = vehicle_colors) + 
  xlab("Observed traffic (veh/h/direction)") +
  ylab("Simulated traffic (veh/h/direction)")



#### totals per time of day

sim = simulated_counts %>%
  group_by(hour, veh_type) %>% 
  summarize(count = sum(vehicles) * scale) %>%
  mutate(source = "sim")
obs = counts_observed_long %>%
  group_by(hour, veh_type) %>%
  summarise(count = sum(vehicles)) %>%
  mutate(source = "obs")
both = sim %>% bind_rows(obs)


ggplot(both, aes(x = hour, y = count, fill = veh_type)) +
  geom_bar(stat = "summary", fun = "mean", position =  "stack") + 
  scale_fill_manual(values = vehicle_colors) + 
  theme_bw() + 
  xlab("Time of day (h)") + ylab("Average number of vehicles") + 
  facet_wrap(.~source)





##spatially

# sim = simulated_counts %>%
#   group_by(station = stationId, veh_type) %>%
#   summarize(count = sum(vehicles) * scale) %>%
#   mutate(source = "sim")
# obs = counts_observed_long %>%
#   group_by(station, latitude, longitude, veh_type) %>%
#   summarise(count = sum(vehicles)) %>%
#   mutate(source = "obs")
# both = obs %>% left_join(sim, by = c("station", "veh_type"), suffix = c("_obs", "_sim"))
# 
# both = both %>% filter(!is.na(count_sim))
# 
# write_csv(both, "comparison.csv")
