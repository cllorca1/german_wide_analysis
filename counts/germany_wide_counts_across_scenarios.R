pacman::p_load(here, readr, dplyr, tidyr, ggplot2)

source(paste(here(), "/counts/data_reader_across_scenarios.R", sep =""), encoding = "UTF-8")

save_plots = F
vehicle_colors = c("car" = "#838383","car_ld" = "#414141", "car_sd" = "#c1c1c1" ,"truck" = "#70ab91")
scale = 100


#### observed vs simulated daily

simulated_counts %>%
  group_by(scenario, vehicle_type, type) %>%
  summarize(vehicles = sum(count)) %>% 
  ggplot(aes(x = scenario, y = vehicles, fill = vehicle_type)) + 
  geom_bar(stat = "identity", position = "stack") + 
  facet_wrap(.~type)


simulated_counts %>%
  group_by(scenario, driver_type, type) %>%
  summarize(vehicles = sum(count)) %>% 
  ggplot(aes(x = scenario, y = vehicles, fill = driver_type)) + 
  geom_bar(stat = "identity", position = "stack") + 
  facet_wrap(.~type)



simulated_counts %>%
  filter(vehicle_type == "car_ld") %>% 
  group_by(scenario, vehicle_type, type) %>%
  summarize(vehicles = sum(count)) %>% 
  ggplot(aes(x = scenario, y = vehicles, fill = vehicle_type)) + 
  geom_bar(stat = "identity", position = "stack") + 
  facet_wrap(.~type)

