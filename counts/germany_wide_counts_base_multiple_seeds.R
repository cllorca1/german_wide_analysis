pacman::p_load(here, readr, dplyr, tidyr, ggplot2)

model_folder = "F:/matsim_germany/"

scenarios = 1L:5L
veh_types = c("car_sd", "car_ld","truck")
vehicle_colors = c("car" = "#838383","car_ld" = "#414141", "car_sd" = "#c1c1c1" ,"truck" = "#70ab91")
#scenarios = c("combined_hermes_20210506","ld_trucks")
#veh_types = c("car","truck")


simulated_counts = data.frame()
for (i in 1:length(scenarios)){
  scenario = scenarios[i]
  this_simulated_counts = read_csv(paste(model_folder, "output/base_seed_", scenario, "/counts_toll.csv", sep  =""))
  simulated_counts = simulated_counts %>%
    bind_rows(this_simulated_counts %>%
                mutate(scenario = scenario))
  
  rm(this_simulated_counts)
}
#add every count into one day (hours will be repeated!)
simulated_counts = simulated_counts %>%
  mutate(hour = if_else(hour >= 48, hour - 48, if_else(hour >= 24, hour - 24, hour)))

veh_by_day_station = simulated_counts %>% group_by(vehicle_type, scenario,  link) %>% summarize(count = 100 * sum(count))


substet_stations = sample(x = unique(veh_by_day_station$link), size = 100, replace = F)

veh_by_day_station %>% group_by(link, vehicle_type) %>% mutate(mean = mean(count), sd = sd(count)) %>% 
  arrange(mean) %>% 
  filter(link %in% substet_stations) %>%
  ggplot(aes(x = mean, y = count, color = vehicle_type)) + 
  geom_point() + 
  geom_abline(slope = 1, intercept = 0, color = "red") + 
  theme_bw() + 
  scale_color_manual(values = vehicle_colors, name = "Vehicle type") + 
  xlab("Average number of vehicles/day") + ylab("Number of vehicles/day")


veh_by_day_station %>% group_by(link, vehicle_type) %>% mutate(mean = mean(count), sd = sd(count)) %>% 
  arrange(mean) %>% 
  filter(link %in% substet_stations) %>%
  ggplot(aes(x = mean, y = sd/mean, color = vehicle_type)) + 
  geom_point()+ 
  theme_bw() + 
  scale_color_manual(values = vehicle_colors, name = "Vehicle type") + 
  xlab("Average number of vehicles/day") + ylab("Coefficient of variation")
