pacman::p_load(here, readr, dplyr, tidyr, ggplot2)

source(paste(here(), "/counts/data_reader_for_v3.R", sep =""), encoding = "UTF-8")

save_plots = F
vehicle_colors = c("car" = "#838383","car_ld" = "#414141", "car_sd" = "#c1c1c1" ,"truck" = "#70ab91")
scale = 100
re_scale = F

#### observed vs simulated daily

sim = simulated_counts %>%
  mutate(is_truck = if_else(veh_type == "truck", "truck", "car")) %>%
  group_by(station = stationId, is_truck ) %>%
  summarize(count = sum(vehicles) * scale) %>%
  mutate(source = "sim")
obs = counts_observed_long %>%
  mutate(is_truck = if_else(veh_type == "truck", "truck", "car")) %>%
  group_by(station, is_truck) %>%
  summarise(count = sum(vehicles)) %>%
  mutate(source = "obs")
both = sim %>% bind_rows(obs)

if (re_scale){
  re_scaling_factors = both %>%
    group_by(source, is_truck) %>%
    summarize(n = sum(count)) %>%
    pivot_wider(names_from = source, values_from = n) %>% 
    mutate(re_scaling = obs/sim) %>% select(-obs, -sim)
} else {
  re_scaling_factors = data.frame(is_truck = c("car", "truck"), re_scaling = c(1.,1.))
}




both = both %>% left_join(re_scaling_factors)
both = both %>% mutate(count = if_else(source == "sim", count * re_scaling, count))

both %>% pivot_wider(values_from = count, names_from = source) %>% 
  filter(obs > 0) %>%
  ggplot(aes(x = obs, y = sim, color = is_truck)) + geom_point() + 
  geom_abline(intercept = 0, slope = 1, color = "black", size = 1) + 
  theme_bw() +
  scale_color_manual(values = vehicle_colors, name = "Vehicle type") + 
  xlab("Observed traffic (veh/day)") +
  ylab("Simulated traffic (veh/day)")

file_name = paste("tmp/", 
                  gsub(x = gsub(x = Sys.time(),pattern = ":", replacement = ""),pattern = " ", replacement = ""),
                  "_obs_sim_daily.jpg", sep = "" )
if (save_plots) {
  ggsave(filename = file_name, device = "jpeg", width = 15, height = 10, units = "cm", scale = 2)
}


#### totals per time of day
sim = simulated_counts %>%
  mutate(is_truck = if_else(veh_type == "truck", "truck", "car")) %>%
  group_by(station = stationId, is_truck, veh_type, hour ) %>%
  summarize(count = sum(vehicles) * scale) %>%
  mutate(source = "sim")
obs = counts_observed_long %>%
  mutate(is_truck = if_else(veh_type == "truck", "truck", "car")) %>%
  group_by(station, is_truck, veh_type, hour) %>%
  summarise(count = sum(vehicles)) %>%
  mutate(source = "obs")
both = sim %>% bind_rows(obs)

both = both %>% left_join(re_scaling_factors)
both = both %>% mutate(count = if_else(source == "sim", count * re_scaling, count))

#### totals per time of day

both = both %>%
  group_by(station, veh_type, source) %>% 
  mutate(total_station = sum(count))

empty_stations = unique(both %>% group_by(station) %>% filter(total_station == 0) %>% select(station))

both = both %>% filter(!(station %in% empty_stations$station))

both = both %>% group_by(hour, veh_type, source) %>% summarise(count = sum(count))

ggplot(both, aes(x = hour, y = count, fill = veh_type)) +
  geom_bar(stat = "summary", fun = "mean", position =  "stack") + 
  scale_fill_manual(values = vehicle_colors, name = "Vehicle type and segment") + 
  theme_bw() + 
  theme(legend.position = "bottom") + 
  xlab("Time of day (h)") + ylab("Sum of vehicles") + 
  facet_wrap(.~source)

if (save_plots){
  file_name = paste("tmp/", 
                    gsub(x = gsub(x = Sys.time(),pattern = ":", replacement = ""),pattern = " ", replacement = ""),
                    "_all_by_tod.jpg", sep = "" )
  ggsave(filename = file_name, device = "jpeg", width = 15, height = 10, units = "cm", scale = 2)
}


both %>% 
  filter(veh_type != "truck") %>% 
  mutate(hour = if_else(source == "obs", hour - 1, hour)) %>% 
  ggplot(aes(x = hour, y = count, fill = veh_type)) +
  geom_bar(stat = "summary", fun = "mean", position =  "stack") + 
  scale_fill_manual(values = vehicle_colors, name = "Vehicle type and segment") + 
  theme_bw() + 
  theme(legend.position = "bottom") + 
  xlab("Time of day (h)") + ylab("Sum of vehicles") + 
  facet_wrap(.~source)



both_aggregated  = both %>% group_by(hour, source) %>% summarize(count = sum(count))
##seems that obs_hour = sim_hour + 1
both_aggregated = both_aggregated %>% mutate(hour = if_else(source == "obs", hour - 1, hour))

both_aggregated = both_aggregated %>% pivot_wider(names_from = source, values_from = count)
both_aggregated = both_aggregated %>% mutate(difference = obs - sim)

ggplot(both_aggregated, aes(x = hour, y = difference, fill = difference)) + geom_bar(stat = "identity") + theme_bw() + 
  scale_fill_gradient2(name = "Difference") +
  ylab("observed - simulated flow (sum all stations)") + 
  theme(legend.position = "bottom")


if (save_plots){
  file_name = paste("tmp/", 
                    gsub(x = gsub(x = Sys.time(),pattern = ":", replacement = ""),pattern = " ", replacement = ""),
                    "_diff_tod.jpg", sep = "" )
  ggsave(filename = file_name, device = "jpeg", width = 15, height = 10, units = "cm", scale = 2)
}


##by OSM type

sim = simulated_counts %>%
  mutate(is_truck = if_else(veh_type == "truck", "truck", "car")) %>%
  group_by(station = stationId, type, is_truck, veh_type) %>%
  summarize(count = sum(vehicles) * scale) %>%
  mutate(source = "sim")

types = sim %>% ungroup() %>% select(station, type) %>% unique()

obs = counts_observed_long %>%
  mutate(is_truck = if_else(veh_type == "truck", "truck", "car")) %>%
  group_by(station, is_truck, veh_type) %>%
  summarise(count = sum(vehicles)) %>%
  mutate(source = "obs") %>% left_join(types)

sim = sim %>% filter(station %in% obs$station)
obs = obs %>% filter(station %in% sim$station)

both = sim %>% bind_rows(obs)

both = both %>% left_join(re_scaling_factors)
both = both %>% mutate(count = if_else(source == "sim", count * re_scaling, count))



both = both %>%
  group_by(station, veh_type, source) %>% 
  mutate(total_station = sum(count))

empty_stations = unique(both %>% group_by(station) %>% filter(total_station == 0) %>% select(station))

both = both %>% filter(!(station %in% empty_stations$station))

both_grouped = both %>% group_by(type, veh_type, source) %>% summarise(mean = mean(count), sum_volumes = sum(count))


both_grouped$type = factor(both_grouped$type, levels = c("motorway", "motorway_link", "trunk", "trunk_link", "primary", "secondary"))


ggplot(both_grouped, aes(x = source, y = mean, fill = veh_type)) +
  geom_bar(stat = "summary", fun = "mean", position =  "stack") + 
  scale_fill_manual(values = vehicle_colors, name = "Vehicle type and segment") + 
  theme_bw() + 
  theme(legend.position = "bottom") + 
  xlab("OSM type") + ylab("Mean of vehicles") + 
  facet_wrap(.~type, ncol = 7)

ggplot(both_grouped, aes(x = source, y = sum_volumes, fill = veh_type)) +
  geom_bar(stat = "summary", fun = "mean", position =  "stack") + 
  scale_fill_manual(values = vehicle_colors, name = "Vehicle type and segment") + 
  theme_bw() + 
  theme(legend.position = "bottom") + 
  xlab("OSM type") + ylab("Sum of vehicles") + 
  facet_wrap(.~type, ncol = 7)


##RMSE by road type (OSM)
sim = simulated_counts %>%
  mutate(is_truck = if_else(veh_type == "truck", "truck", "car")) %>%
  group_by(station = stationId, type, is_truck) %>%
  summarize(count = sum(vehicles) * scale) %>%
  mutate(source = "sim")

types = sim %>% ungroup() %>% select(station, type) %>% unique()

obs = counts_observed_long %>%
  mutate(is_truck = if_else(veh_type == "truck", "truck", "car")) %>%
  group_by(station, is_truck) %>%
  summarise(count = sum(vehicles)) %>%
  mutate(source = "obs") %>% left_join(types)


sim = sim %>% filter(station %in% obs$station)
obs = obs %>% filter(station %in% sim$station)



both = sim %>% bind_rows(obs)

both = both %>% left_join(re_scaling_factors)
both = both %>% mutate(count = if_else(source == "sim", count * re_scaling, count))

both_wide = both %>% pivot_wider(names_from = source, values_from = count, values_fill =0) %>% 
  mutate(error = sim - obs) %>% 
  mutate(sq_error= (sim - obs)^2) %>% 
  filter(obs > 0)

both_wide %>% 
  group_by(type, is_truck) %>% 
  summarise(obs_mean = mean(obs, na.rm = T),
            sim_mean = mean(sim, na.rm = T), 
            rmse = sqrt(mean(sq_error, na.rm = T)),
            p_rmse = rmse / obs_mean * 100)

both_wide %>% 
  group_by(is_truck) %>% 
  summarise(obs_mean = mean(obs, na.rm = T),
            sim_mean = mean(sim, na.rm = T), 
            rmse = sqrt(mean(sq_error, na.rm = T)),
            p_rmse = rmse / obs_mean * 100)

ggplot(both_wide, aes(x = error, fill = is_truck)) +
  geom_density(alpha = 0.2) + 
  scale_fill_manual(values = vehicle_colors) + 
  theme_bw() + 
  xlab("Absolute error") + ylab("Frequency")



## by road type (station type A or B)

types = counts_observed_long %>% ungroup() %>% select(station, road_type) %>% unique()

sim = simulated_counts %>%
  mutate(is_truck = if_else(veh_type == "truck", "truck", "car")) %>%
  group_by(station = stationId, is_truck, veh_type) %>%
  summarize(count = sum(vehicles) * scale) %>%
  mutate(source = "sim") %>% left_join(types)


obs = counts_observed_long %>%
  mutate(is_truck = if_else(veh_type == "truck", "truck", "car")) %>%
  group_by(station, is_truck, veh_type, road_type) %>%
  summarise(count = sum(vehicles)) %>%
  mutate(source = "obs")

sim = sim %>% filter(station %in% obs$station)
obs = obs %>% filter(station %in% sim$station)



both = sim %>% bind_rows(obs)

both = both %>% left_join(re_scaling_factors)
both = both %>% mutate(count = if_else(source == "sim", count * re_scaling, count))

both = both %>%
  group_by(station, veh_type, source) %>% 
  mutate(total_station = sum(count))

empty_stations = unique(both %>% group_by(station) %>% filter(total_station == 0) %>% select(station))

both = both %>% filter(!(station %in% empty_stations$station))

both = both %>% group_by(road_type, veh_type, source) %>% summarise(mean = mean(count), sum_volumes = sum(count))

ggplot(both, aes(x = source, y = mean, fill = veh_type)) +
  geom_bar(stat = "identity", position =  "stack") + 
  scale_fill_manual(values = vehicle_colors, name = "Vehicle type and segment") + 
  theme_bw() + 
  theme(legend.position = "bottom") + 
  xlab("A-B type") + ylab("Mean of vehicles") + 
  facet_wrap(.~road_type)

ggplot(both, aes(x = source, y = sum_volumes, fill = veh_type)) +
  geom_bar(stat = "identity", position =  "stack") + 
  scale_fill_manual(values = vehicle_colors, name = "Vehicle type and segment") + 
  theme_bw() + 
  theme(legend.position = "bottom") + 
  xlab("A-B type") + ylab("Sum of vehicles") + 
  facet_wrap(.~road_type)



#RMSE by A-B type

obs = counts_observed_long %>%
  mutate(is_truck = if_else(veh_type == "truck", "truck", "car")) %>%
  group_by(station, road_type, is_truck) %>%
  summarise(count = sum(vehicles)) %>%
  mutate(source = "obs") 

types = obs %>% ungroup() %>% select(station, road_type) %>% unique()

sim = simulated_counts %>%
  mutate(is_truck = if_else(veh_type == "truck", "truck", "car")) %>%
  group_by(station = stationId, is_truck) %>%
  summarize(count = sum(vehicles) * scale) %>%
  mutate(source = "sim") %>% left_join(types)

both = sim %>% filter(station %in% obs$station) %>% bind_rows(obs)

both = both %>% left_join(re_scaling_factors)
both = both %>% mutate(count = if_else(source == "sim", count * re_scaling, count))

both_wide = both %>% pivot_wider(names_from = source, values_from = count, values_fill =0) %>% 
  mutate(error = sim - obs) %>% 
  mutate(sq_error= (sim - obs)^2) %>% 
  filter(obs > 0)

both_wide %>% 
  group_by(road_type, is_truck) %>% 
  summarise(obs_mean = mean(obs, na.rm = T),
            sim_mean = mean(sim, na.rm = T), 
            rmse = sqrt(mean(sq_error, na.rm = T)),
            p_rmse = rmse / obs_mean * 100)
 