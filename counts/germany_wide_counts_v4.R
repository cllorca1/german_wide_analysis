pacman::p_load(here, readr, dplyr, tidyr, ggplot2)

source(paste(here(), "/counts/data_reader_for_v4.R", sep =""), encoding = "UTF-8")

save_plots = F
vehicle_colors = c("car" = "#838383","car_ld" = "#414141", "car_sd" = "#c1c1c1" ,"truck" = "#70ab91")
scale = 100


#### observed vs simulated daily

sim = simulated_counts %>%
  mutate(is_truck = if_else(veh_type == "truck", "truck", "car")) %>%
  group_by(station = stationId, is_truck ) %>%
  summarize(sim = sum(vehicles) * scale) 
obs = counts_observed_long %>%
  mutate(is_truck = if_else(veh_type == "truck", "truck", "car")) %>%
  select(station, is_truck, obs = vehicles, vehicles_sd)

both_wide = sim %>% left_join(obs)

both_wide %>% filter(obs > 0) %>%
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

both_wide = both_wide %>% 
  mutate(error = abs(sim - obs), error_in_sd = error/vehicles_sd)


both_wide %>%
  filter(obs > 0) %>%
  ggplot(aes(x = error_in_sd, fill = is_truck)) + geom_density()

both_wide %>%
  filter(obs > 0) %>%
  mutate(band = cut(error_in_sd, breaks = c(0,1,2,200))) %>% 
  ggplot(aes(x = obs, y = sim, color = band)) + geom_point() + 
           facet_wrap(.~is_truck, scales = "free")



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

both = sim %>% bind_rows(obs)

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

both = sim %>% bind_rows(obs)

both_wide = both %>% pivot_wider(names_from = source, values_from = count, values_fill =0) %>% 
  mutate(error = sim - obs) %>% 
  mutate(sq_error= (sim - obs)^2) %>% 
  filter(obs > 0, sim > 0)

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
  mutate(source = "obs") %>% left_join(types)

both = sim %>% bind_rows(obs)

both = both %>%
  group_by(station, veh_type, source) %>% 
  mutate(total_station = sum(count))

empty_stations = unique(both %>% group_by(station) %>% filter(total_station == 0) %>% select(station))

both = both %>% filter(!(station %in% empty_stations$station))

both = both %>% group_by(road_type, veh_type, source) %>% summarise(mean = mean(count), sum_volumes = sum(count))


ggplot(both, aes(x = source, y = mean, fill = veh_type)) +
  geom_bar(stat = "summary", fun = "mean", position =  "stack") + 
  scale_fill_manual(values = vehicle_colors, name = "Vehicle type and segment") + 
  theme_bw() + 
  theme(legend.position = "bottom") + 
  xlab("A-B type") + ylab("Mean of vehicles") + 
  facet_wrap(.~road_type)

ggplot(both, aes(x = source, y = sum_volumes, fill = veh_type)) +
  geom_bar(stat = "summary", fun = "mean", position =  "stack") + 
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

both = sim %>% bind_rows(obs)

both_wide = both %>% pivot_wider(names_from = source, values_from = count, values_fill =0) %>% 
  mutate(error = sim - obs) %>% 
  mutate(sq_error= (sim - obs)^2) %>% 
  filter(obs > 0, sim > 0)

both_wide %>% 
  group_by(road_type, is_truck) %>% 
  summarise(obs_mean = mean(obs, na.rm = T),
            sim_mean = mean(sim, na.rm = T), 
            rmse = sqrt(mean(sq_error, na.rm = T)),
            p_rmse = rmse / obs_mean * 100)
 
