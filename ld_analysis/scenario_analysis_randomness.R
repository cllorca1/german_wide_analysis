pacman::p_load(data.table, dplyr, ggplot2, tidyr, readr, plotly, sf, tmap, rgeos, RColorBrewer)

out_folder= "Y:/projects/2019/BASt/data/input_files_LD/germanymodel/output/"
ld_mode_order = c("auto", "auto_toll", "bus", "rail", "air")
colors_ld_modes  =c("auto" = "#aaaaaa", "auto_toll" = "#2d2d2d", "rail"  ="#764c6e", 
                    "bus" = "#bdc9bb", "air" = "#83adb5")

scenarios = c("7_A_2", 
              "7_A_2_0",
              "7_A_2_1",
              "7_A_2_2",
              "7_A_2_3",
              "7_A_2_4")

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

trips_by_distance %>% ggplot(aes(x = as.numeric(dist_bin)*20, y = n/total * 100, color = scenario_name)) +
  geom_line(stat = "identity", size = 1) + facet_grid(tripState~tripPurpose)




##Mode choice
trip_count = trips %>%
  filter(travelDistanceByCar_km < 10000 ) %>% 
  filter(tripState != "away", tripDestType != "EXTOVERSEAS") %>%
  group_by(tripMode, scenario_name, scenario_group) %>%
  summarize(trips = n(), distance = sum(travelDistanceByCar_km))

trip_count = trip_count %>% mutate(tripMode = factor(tripMode, levels = ld_mode_order))

trip_count = trip_count %>% left_join(scenario_parameters, by = c("scenario_name" = "scenario"))


trip_count %>%
  group_by(scenario_name) %>% mutate(total = sum(trips)) %>% 
  ggplot(aes(x = scenario_name, y = trips/total, label = paste(sprintf("%.3f", trips/total * 100), "%", sep  =""), fill = tripMode)) +
  geom_bar(position  = "fill", stat = "identity") +
  scale_fill_manual(values = colors_ld_modes, name = "Mode") + 
  theme(axis.text.x = element_text(angle = 0), legend.position = "bottom") + 
  xlab("Scenario") + ylab("Modal share") + 
  geom_text(position = position_stack(vjust = 0.5))


##Mode choice bz purpose and trip-state
trip_count = trips %>%
  filter(travelDistanceByCar_km < 10000 ) %>% 
  filter(tripState != "away", tripDestType != "EXTOVERSEAS") %>%
  group_by(tripMode, scenario_name, scenario_group, tripPurpose, tripState) %>%
  summarize(trips = n(), distance = sum(travelDistanceByCar_km))

trip_count = trip_count %>% mutate(tripMode = factor(tripMode, levels = ld_mode_order))

trip_count = trip_count %>% left_join(scenario_parameters, by = c("scenario_name" = "scenario"))


trip_count %>%
  group_by(scenario_name, tripPurpose, tripState) %>% mutate(total = sum(trips)) %>% 
  ggplot(aes(x = scenario_name, y = trips/total, label = paste(sprintf("%.3f", trips/total * 100), "%", sep  =""), fill = tripMode)) +
  geom_bar(position  = "fill", stat = "identity") +
  scale_fill_manual(values = colors_ld_modes, name = "Mode") + 
  theme(axis.text.x = element_text(angle = 0, hjust = 1), legend.position = "bottom") + 
  xlab("Scenario") + ylab("Modal share") + 
  geom_text(position = position_stack(vjust = 0.5)) + 
  facet_grid(tripPurpose ~ tripState)


trip_count = trips %>%
  mutate(dist_bin = cut(travelDistanceByCar_km, breaks = seq(0,2000,200), include.lowest = T)) %>%
  filter(tripState != "away", tripDestType != "EXTOVERSEAS", travelDistanceByCar_km < 2000) %>%
  group_by(tripMode, scenario_name,dist_bin) %>%
  summarize(trips = n(), distance = sum(travelDistanceByCar_km))

trip_count = trip_count %>% mutate(tripMode = factor(tripMode, levels = ld_mode_order))

trip_count %>% 
  ggplot(aes(x = scenario_name, y = trips, fill = tripMode, width=1)) + 
  geom_bar(position  = position_fill(), stat = "identity", color = "gray20") +
  scale_fill_manual(values = colors_ld_modes, name = "Mode") + 
  theme(axis.text.x = element_text(angle = 90), legend.position = "bottom") + 
  xlab("Distance (km)") + ylab("Share") + facet_grid(.~dist_bin)






summary = trips %>%
  filter(tripState != "away", tripDestType != "EXTOVERSEAS") %>%
  group_by(tripMode, scenario_name) %>%
  summarize(n = n())

write.table(summary, "clipboard", row.names = T, sep = "\t")


summary = trips %>%
  filter(tripState != "away", tripDestType != "EXTOVERSEAS") %>%
  filter(tripOriginZone == 7698) %>%
group_by(tripMode, scenario_name) %>%
  summarize(n = n())

write.table(summary, "clipboard", row.names = T, sep = "\t")

