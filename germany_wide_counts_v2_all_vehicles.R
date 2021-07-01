pacman::p_load(readr, dplyr, tidyr, ggplot2)

save_plots = F


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

length(unique(counts_observed_long$station))

#write_csv(counts_observed_long, paste(counts_folder, "weekday_counts_by_station_hour_long.csv", sep  =""))
names(counts_observed_long)

model_folder = "F:/matsim_germany/"

scenarios = c("run_210625")
veh_types = c("car_sd", "car_ld","truck")

#scenarios = c("combined_hermes_20210506","ld_trucks")
#veh_types = c("car","truck")

simulated_counts = data.frame()
for (i in 1:length(scenarios)){
  scenario = scenarios[i]
  this_simulated_counts = read_csv(paste(model_folder, "output/", scenario, "/counts.csv", sep  =""))
  simulated_counts = simulated_counts %>%
    bind_rows(this_simulated_counts %>%
                mutate(veh_type = "car_sd", vehicles = car_sd) %>%
                select(link, hour, length, type, vehicles, veh_type))
  simulated_counts = simulated_counts %>%
    bind_rows(this_simulated_counts %>%
                mutate(veh_type = "car_ld", vehicles = car_ld) %>%
                select(link, hour, length, type, vehicles, veh_type))
  simulated_counts = simulated_counts %>%
    bind_rows(this_simulated_counts %>%
                mutate(veh_type = "truck", vehicles = truck) %>%
                select(link, hour, length, type, vehicles, veh_type))
  rm(this_simulated_counts)
}

#add every count into one day (hours will be repeated!)
simulated_counts = simulated_counts %>%
  mutate(hour = if_else(hour >= 48, hour - 48, if_else(hour >= 24, hour - 24, hour)))

length(unique(simulated_counts$link))



##stations and links 
model_folder = "C:/models/germanymodel/matsim/"
station_links_2011 = read_csv(paste(model_folder, "bast_station_links_network_2011.csv", sep =""))

length(unique(station_links_2011$stationId))
length(unique(station_links_2011$linkId))


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
  ggplot(aes(x = obs, y = sim, color = is_truck)) + geom_point(alpha = 0.2) + 
  geom_abline(intercept = 0, slope = 1, color = "black", size = 1) + 
  theme_bw() +
  scale_color_manual(values = vehicle_colors) + 
  xlab("Observed traffic (veh/day/direction)") +
  ylab("Simulated traffic (veh/day/direction)")

file_name = paste("tmp/", 
                  gsub(x = gsub(x = Sys.time(),pattern = ":", replacement = ""),pattern = " ", replacement = ""),
                  "_obs_sim_daily.jpg", sep = "" )
if (save_plots) {
  ggsave(filename = file_name, device = "jpeg", width = 15, height = 10, units = "cm", scale = 2)
}

both_wide = both %>% pivot_wider(id_cols = c(station, direction),
                                 names_from = c(source, is_truck),
                                 values_from = count,
                                 values_fill = NA)

##remove stations without observed traffic volume: 
both_wide = both_wide %>% filter(obs_car > 0, obs_truck > 0, sim_car > 0, sim_truck >0)

total_sim_car = sum(both_wide$sim_car) 
total_obs_car = sum(both_wide$obs_car)

total_sim_truck = sum(both_wide$sim_truck)
total_obs_truck = sum(both_wide$obs_truck)

apply_correction = T


if (apply_correction){
  re_scale_factor_car = total_obs_car/total_sim_car
  re_scale_factor_truck = total_obs_truck/total_sim_truck
} else {
  re_scale_factor_car = 1
  re_scale_factor_truck = 1
}

both_wide = both_wide %>%
  mutate(error_car = sim_car * re_scale_factor_car - obs_car) %>% 
  mutate(error_truck = sim_truck * re_scale_factor_truck - obs_truck) 


sqrt(sum(both_wide$error_car^2)/nrow(both_wide))
sqrt(sum(both_wide$error_car^2)/nrow(both_wide))/mean(both_wide$obs_car)


sqrt(sum(both_wide$error_truck^2)/nrow(both_wide))
sqrt(sum(both_wide$error_truck^2)/nrow(both_wide))/mean(both_wide$obs_truck)


ggplot(both_wide, aes(x = obs_car, y = sim_car * re_scale_factor_car)) +
  geom_point(color = vehicle_colors["car"])  + 
  geom_abline(intercept = 0, slope = 1, color = "black", size = 1) + 
  theme_bw() +
  scale_color_manual(values = vehicle_colors) + 
  xlab("Observed traffic (veh/day/direction)") +
  ylab("Simulated traffic (veh/day/direction)")

if(save_plots){
  file_name = paste("tmp/", 
                    gsub(x = gsub(x = Sys.time(),pattern = ":", replacement = ""),pattern = " ", replacement = ""),
                    "_obs_sim_daily_car_adjusted.jpg", sep = "" )
  ggsave(filename = file_name, device = "jpeg", width = 15, height = 10, units = "cm", scale = 2)
}


ggplot(both_wide, aes(x = obs_truck, y = sim_truck * re_scale_factor_truck)) +
  geom_point(color = vehicle_colors["truck"])  + 
  geom_abline(intercept = 0, slope = 1, color = "black", size = 1) + 
  theme_bw() +
  scale_color_manual(values = vehicle_colors) + 
  xlab("Observed traffic (veh/day/direction)") +
  ylab("Simulated traffic (veh/day/direction)")

if(save_plots){

file_name = paste("tmp/", 
                  gsub(x = gsub(x = Sys.time(),pattern = ":", replacement = ""),pattern = " ", replacement = ""),
                  "_obs_sim_daily_truck_adjusted.jpg", sep = "" )
ggsave(filename = file_name, device = "jpeg", width = 15, height = 10, units = "cm", scale = 2)

}


#### observed vs simulated hourly 

sim = simulated_counts %>%
  mutate(is_truck = if_else(veh_type == "truck", "truck", "car")) %>%
  group_by(station = stationId, is_truck, veh_type, direction, hour ) %>%
  summarize(count = sum(vehicles) * scale) %>%
  mutate(source = "sim")
obs = counts_observed_long %>%
  mutate(is_truck = if_else(veh_type == "truck", "truck", "car")) %>%
  group_by(station, is_truck, veh_type, direction = direction_index, hour) %>%
  summarise(count = sum(vehicles)) %>%
  mutate(source = "obs")
both = sim %>% bind_rows(obs)
# 
# 
# both %>% pivot_wider(values_from = count, names_from = source) %>% 
#   ggplot(aes(x = obs, y = sim, color = is_truck)) + geom_point() + 
#   geom_abline(intercept = 0, slope = 1, color = "black", size = 1) + 
#   geom_abline(intercept = 0, slope = 0.75, color = "black", size = 1) + 
#   geom_abline(intercept = 0, slope = 1.25, color = "black", size = 1) + 
#   theme_bw() +
#   scale_color_manual(values = vehicle_colors) + 
#   xlab("Observed traffic (veh/h/direction)") +
#   ylab("Simulated traffic (veh/h/direction)")



#### totals per time of day

both = both %>%
  group_by(station, veh_type, source) %>% 
  mutate(total_station = sum(count))

empty_stations = unique(both %>% group_by(station) %>% filter(total_station == 0) %>% select(station))

both = both %>% filter(!(station %in% empty_stations$station))

both = both %>% group_by(hour, veh_type, source) %>% summarise(count = sum(count))
# 
# sim = simulated_counts %>%
#   group_by(hour, veh_type) %>% 
#   summarize(count = sum(vehicles) * scale) %>%
#   mutate(source = "sim")
# obs = counts_observed_long %>%
#   group_by(hour, veh_type) %>%
#   summarise(count = sum(vehicles)) %>%
#   mutate(source = "obs")
# both = sim %>% bind_rows(obs)


ggplot(both, aes(x = hour, y = count, fill = veh_type)) +
  geom_bar(stat = "summary", fun = "mean", position =  "stack") + 
  scale_fill_manual(values = vehicle_colors) + 
  theme_bw() + 
  xlab("Time of day (h)") + ylab("Sum of vehicles") + 
  facet_wrap(.~source) + 
  geom_vline(xintercept = 24, color = "red", size = 1, linetype = "dashed")

if (save_plots){
  file_name = paste("tmp/", 
                    gsub(x = gsub(x = Sys.time(),pattern = ":", replacement = ""),pattern = " ", replacement = ""),
                    "_all_by_tod.jpg", sep = "" )
  ggsave(filename = file_name, device = "jpeg", width = 15, height = 10, units = "cm", scale = 2)
}


ggplot(both, aes(x = hour, y = count, fill = veh_type)) +
  geom_bar(stat = "summary", fun = "mean", position =  "stack") + 
  scale_fill_manual(values = vehicle_colors) + 
  theme_bw() + 
  xlab("Time of day (h)") + ylab("Sum of vehicles") + 
  facet_grid(veh_type~source) + 
  geom_vline(xintercept = 24, color = "red", size = 1, linetype = "dashed")

if (save_plots){
  file_name = paste("tmp/", 
                    gsub(x = gsub(x = Sys.time(),pattern = ":", replacement = ""),pattern = " ", replacement = ""),
                    "_all_by_tod_veh.jpg", sep = "" )
  ggsave(filename = file_name, device = "jpeg", width = 15, height = 10, units = "cm", scale = 2)
}


both_aggregated  = both %>% group_by(hour, source) %>% summarize(count = sum(count))
##seems that obs_hour = sim_hour + 1
both_aggregated = both_aggregated %>% mutate(hour = if_else(source == "obs", hour - 1, hour))

both_aggregated = both_aggregated %>% pivot_wider(names_from = source, values_from = count)
both_aggregated = both_aggregated %>% mutate(difference = obs - sim)

ggplot(both_aggregated, aes(x = hour, y = difference, fill = difference)) + geom_bar(stat = "identity") + theme_bw() + 
  scale_fill_gradient2() + ylab("observed - simulated flow (sum all stations)")


if (save_plots){
  file_name = paste("tmp/", 
                    gsub(x = gsub(x = Sys.time(),pattern = ":", replacement = ""),pattern = " ", replacement = ""),
                    "_diff_tod.jpg", sep = "" )
  ggsave(filename = file_name, device = "jpeg", width = 15, height = 10, units = "cm", scale = 2)
}



##spatially

sim = simulated_counts %>%
  mutate(veh_type = if_else(veh_type == "truck", "truck", "car")) %>%
  group_by(station = stationId, veh_type, direction) %>%
  summarize(count = sum(vehicles) * scale)
obs = counts_observed_long %>%
  group_by(station, latitude, longitude, veh_type, direction_index, direction_city = direction) %>%
  summarise(count = sum(vehicles))
both = obs %>% left_join(sim, by = c("station", "veh_type", "direction_index" = "direction" ), suffix = c("_obs", "_sim"))

both = both %>% filter(!is.na(count_sim))

write_csv(both, "tmp/comparison_all.csv")




##output file to find missing stations in the matsim network
sim = simulated_counts %>%
  group_by(station = stationId, direction) %>%
  summarize(count_sim = sum(vehicles) * scale) 
obs = counts_observed_long %>%
  group_by(station, direction = direction_index) %>%
  summarise(count_obs = sum(vehicles))
both = sim %>% full_join(obs)

missing_stations_id = both %>% filter(is.na(count_sim))

missing_stations = counts_observed_long %>% filter(station %in% missing_stations_id$station) %>%
  group_by(station, road_type, longitude, latitude, osm, direction, direction_index) %>% summarize(n())


write_csv(missing_stations, "tmp/missing_stations.csv")
