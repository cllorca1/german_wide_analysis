pacman::p_load(data.table, dplyr, ggplot2, tidyr, readr, plotly, sf, tmap, rgeos, RColorBrewer)

#+ fig.width = 10, fig.height = 7, dev = 'svg', message = FALSE


out_folder= "Y:/projects/2019/BASt/data/input_files_LD/germanymodel/output/"
ld_mode_order = c("auto", "auto_toll", "bus", "rail", "air")
colors_ld_modes  =c("auto" = "#aaaaaa", "auto_toll" = "#2d2d2d", "rail"  ="#764c6e", 
                    "bus" = "#bdc9bb", "air" = "#83adb5")

scenarios = c("0", "0_2030")

scenario_names = scenarios
scenario_groups = substr(scenarios, 1,1)

trips = data.frame()

for (i in 1:length(scenarios)){
  scenario_folder = scenarios[i]
  this_trips = read_csv(paste(out_folder,scenario_folder, "/0_trips.csv", sep  =""))
  this_trips$scenario_name = scenario_names[i]
  this_trips$scenario_group = scenario_groups[i]
  trips = trips %>% bind_rows(this_trips)
  print(scenario_folder)
}

# 
# trips = trips %>% mutate(tripMode = if_else(scenario_group == "4", 
#                                             recode(tripMode, "auto" = "auto_toll", "auto_noToll" = "auto"),
#                                             tripMode))

#summary(as.factor(trips$tripMode))


##Destination choice
trips_by_distance = trips %>%
  filter(tripState != "away", tripDestType != "EXTOVERSEAS") %>%
  mutate(dist_bin = cut(travelDistanceByCar_km, breaks = seq(0,2000,20))) %>%
  group_by(tripPurpose, tripState, scenario_name, scenario_group) %>%
  mutate(total = n()) %>% group_by(tripPurpose, tripState, scenario_name, dist_bin, total) %>%
  summarise(n = n())

trips_by_distance %>%
  ggplot(aes(x = as.numeric(dist_bin)*20, y = n/total * 100, color = scenario_name)) +
  geom_line(stat = "identity", size = 1) +
  facet_grid(tripState~tripPurpose) + xlab("Log(distance) (km)") + ylab("Frequency (%)") + 
  scale_x_log10() + theme_bw() + 
  scale_color_brewer(palette = "Set2")
  

##Mode choice
trip_count = trips %>%
  filter(travelDistanceByCar_km < 10000 ) %>% 
  filter(tripState != "away", tripDestType != "EXTOVERSEAS") %>%
  group_by(tripMode, scenario_name, scenario_group) %>%
  summarize(trips = n(), distance = sum(travelDistanceByCar_km))

trip_count = trip_count %>% mutate(tripMode = factor(tripMode, levels = ld_mode_order))


trip_count %>%
  group_by(scenario_name) %>% mutate(total = sum(trips)) %>% 
  ggplot(aes(x = scenario_name, y = trips, label = sprintf("%.0f", trips), fill = tripMode)) +
  geom_bar(position  = "stack", stat = "identity") +
  scale_fill_manual(values = colors_ld_modes, name = "Mode") + 
  theme(axis.text.x = element_text(angle = 0), legend.position = "bottom") + 
  xlab("Scenario") + ylab("Trips") + 
  geom_text(position = position_stack(vjust = 0.5))

trip_count %>%
  group_by(scenario_name) %>% mutate(total = sum(trips)) %>% 
  ggplot(aes(x = scenario_name, y = trips/total, label = sprintf("%.3f", trips/total), fill = tripMode)) +
  geom_bar(position  = "stack", stat = "identity") +
  scale_fill_manual(values = colors_ld_modes, name = "Mode") + 
  theme(axis.text.x = element_text(angle = 0), legend.position = "bottom") + 
  xlab("Scenario") + ylab("Modal share") + 
  geom_text(position = position_stack(vjust = 0.5))



##By area type
zonal_data = read_csv("scenarios/zoneSystemDE_2col.csv")
zonal_data$BBSR_type = recode(as.character(zonal_data$BBSR_type), 
                              "10" = "10: core",
                              "20" = "20: medium_city",
                              "30" = "30:town",
                              "40" = "40: rural")


trips = trips %>% left_join(zonal_data, by = c("tripOriginZone" = "Zone")) %>%
  mutate(type_o = BBSR_type) %>% select(-BBSR_type)

trips = trips %>% left_join(zonal_data, by = c("tripDestZone" = "Zone")) %>%
  mutate(type_d = BBSR_type) %>% select(-BBSR_type)


trip_count = trips %>%
  filter(travelDistanceByCar_km < 10000 ) %>% 
  filter(tripState != "away", tripDestType != "EXTOVERSEAS") %>%
  group_by(tripMode, scenario_name, scenario_group, type_o) %>%
  summarize(trips = n(), distance = sum(travelDistanceByCar_km))

trip_count = trip_count %>% mutate(tripMode = factor(tripMode, levels = ld_mode_order))

trip_count %>%
  ggplot(aes(x = scenario_name, y = trips, fill = tripMode)) +
  geom_bar(position  = "fill", stat = "identity") +
  scale_fill_manual(values = colors_ld_modes, name = "Mode") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1), legend.position = "bottom") + 
  xlab("Scenario") + ylab("Modal share")  + 
  facet_grid(. ~ type_o)


trip_count %>%
  ggplot(aes(x = scenario_name, y = trips*20, fill = tripMode)) +
  geom_bar(position  = "stack", stat = "identity") +
  scale_fill_manual(values = colors_ld_modes, name = "Mode") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1), legend.position = "bottom") + 
  xlab("Scenario") + ylab("Modal share")  + 
  facet_grid(. ~ type_o)


