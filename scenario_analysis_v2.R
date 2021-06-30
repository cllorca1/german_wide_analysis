pacman::p_load(data.table, dplyr, ggplot2, tidyr, readr, plotly, sf, tmap, rgeos, RColorBrewer)

out_folder= "Y:/projects/2019/BASt/data/input_files_LD/germanymodel/output/"
ld_mode_order = c("auto", "auto_toll", "bus", "rail", "air")
colors_ld_modes  =c("auto" = "#aaaaaa", "auto_toll" = "#2d2d2d", "rail"  ="#764c6e", 
                    "bus" = "#bdc9bb", "air" = "#83adb5")

save_plots = F

scenarios = c("0", 
              "1_A_0",
              "1_A_1",
              "1_A_2",
              "1_B_1",
              "1_B_2",
              #"1_C_1",
              "1_C_2",
              "1_C_3",
              "1_C_4",
              "2_A_1",
              "2_A_2",
              "2_A_3",
              "3_A_1",
              "3_A_2",
              "3_B_1",
              "3_C_1",
              "4_A_0_0.25",
              "4_A_1",
              "4_A_2", 
              "7_A_1",
              "7_A_2",
              "7_A_3")

scenario_names = scenarios
scenario_groups = substr(scenarios, 1,1)

scenario_parameters = read_csv("scenarios/scenario_labels.csv")
scenario_parameters = scenario_parameters %>%
  mutate(label = if_else(!is.na(var2), 
                         paste(scenario, "\n(", var1, "=", val1, ", ",  var2, "=", val2, ")", sep  ="") , 
                         paste(scenario, "\n(", var1, "=", val1, ")", sep  =""))) %>%
  mutate(label2 = if_else(!is.na(var2), 
                          paste(scenario, "\n", var1, "=", val1, "\n",  var2, "=", val2, "", sep  =""),
                          paste(scenario, "\n", var1, "=", val1, sep  ="")))

#manually correct the label of base scenario
scenario_parameters$label[1] = "0"
scenario_parameters$label2[1] = "0"

scenario_parameters = scenario_parameters %>% select(scenario, label, label2)


trips = data.frame()

for (i in 1:length(scenarios)){
  scenario_folder = scenarios[i]
  this_trips = read_csv(paste(out_folder,scenario_folder, "/0_trips.csv", sep  =""))
  this_trips$scenario_name = scenario_names[i]
  this_trips$scenario_group = scenario_groups[i]
  trips = trips %>% bind_rows(this_trips)
  print(scenario_folder)
}


trips = trips %>% mutate(tripMode = if_else(scenario_group == "4", 
                                            recode(tripMode, "auto" = "auto_toll", "auto_noToll" = "auto"),
                                            tripMode))

trip_sumamry = trips %>%
  filter(travelDistanceByCar_km < 10000 ) %>% 
  filter(tripState != "away", tripDestType != "EXTOVERSEAS") %>%
  group_by(tripMode, scenario_name, scenario_group, tripState, tripPurpose) %>%
  summarize(trips = n(), distance = sum(travelDistanceByCar_km))

#####Mode choice with all scenarios######
trip_count = trips %>%
  filter(travelDistanceByCar_km < 10000 ) %>% 
  filter(tripState != "away", tripDestType != "EXTOVERSEAS") %>%
  group_by(tripMode, scenario_name, scenario_group) %>%
  summarize(trips = n(), distance = sum(travelDistanceByCar_km))

trip_count = trip_count %>% mutate(tripMode = factor(tripMode, levels = ld_mode_order))
trip_count = trip_count %>% left_join(scenario_parameters, by = c("scenario_name" = "scenario"))

trip_count %>% write_csv(paste(out_folder, "summary_1.csv", sep = ""))

########################################
##start from here if already written!
#########################################
trip_count = read_csv(paste(out_folder, "summary_1.csv", sep = ""))
trip_count = trip_count %>% mutate(tripMode = factor(tripMode, levels = ld_mode_order))


trip_count %>%
  ggplot(aes(x = label, y = trips, fill = tripMode)) +
  geom_bar(position  = "fill", stat = "identity") +
  scale_fill_manual(values = colors_ld_modes, name = "Mode") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1), legend.position = "bottom") + 
  xlab("Scenario") + ylab("Modal share") 

if (save_plots){
  file_name = paste("tmp/",
                    gsub(x = gsub(x = Sys.time(),pattern = ":", replacement = ""),pattern = " ", replacement = ""),
                    "_modal_shares_fill.jpg", sep = "" )
  ggsave(filename = file_name, device = "jpeg", width = 15, height = 10, units = "cm", scale = 2)
}


trip_count %>%
  ggplot(aes(x = label, y = distance, fill = tripMode)) +
  geom_bar(position  = "fill", stat = "identity") +
  scale_fill_manual(values = colors_ld_modes, name = "Mode") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1), legend.position = "bottom") + 
  xlab("Scenario") + ylab("Modal share based on travelled distance (by car)") 

## plot only the relative differences of the car modal share: 

trip_count_relative = trip_count %>%
  mutate(mode = if_else(tripMode == "auto_toll", "auto", as.character(tripMode))) %>%
  group_by(scenario_name) %>% 
  mutate(total = sum(trips))

trip_count_relative_base = trip_count_relative %>%
  filter(scenario_group == "0") %>% 
  mutate(trips_base = trips, distance_base = distance) %>%
  ungroup() %>%
  select(mode, distance_base, trips_base, total_base = total)

trip_count_relative = trip_count_relative %>%
  filter(scenario_group != "0") %>%
  left_join(trip_count_relative_base, by = c("mode")) %>% 
  group_by(scenario_name, scenario_group, label, label2, mode, total, trips_base, total_base, distance_base) %>% 
  summarize(trips = sum(trips), distance = sum(distance))
 

trip_count_relative %>%
  filter(mode == "auto") %>% 
  ggplot(aes(x = label, y = (trips/total - trips_base/total_base)*100, group = mode, color = mode)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  scale_color_manual(values = colors_ld_modes, name = "Mode") + 
  theme_bw() + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1), legend.position = "none") + 
  xlab("Scenario") + ylab("Difference in auto modal share (p.p.)") + 
  geom_hline(yintercept = 0, color = "black")


##Mode choice by zone type #####
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
  group_by(tripMode, scenario_name, scenario_group, type_o, type_d) %>%
  summarize(trips = n(), distance = sum(travelDistanceByCar_km))

trip_count = trip_count %>% left_join(scenario_parameters, by = c("scenario_name" = "scenario"))

trip_count %>% write_csv(paste(out_folder, "summary_2.csv", sep = ""))
########################################
##start from here if already written!
#########################################
trip_count = read_csv(paste(out_folder, "summary_2.csv", sep = ""))


trip_count = trip_count %>% mutate(tripMode = factor(tripMode, levels = ld_mode_order))

trip_count %>%
  filter(!is.na(type_d)) %>%
  ggplot(aes(x = scenario_name, y = trips, fill = tripMode)) +
  geom_bar(position  = "fill", stat = "identity") +
  scale_fill_manual(values = colors_ld_modes, name = "Mode") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1), legend.position = "bottom") + 
  xlab("Scenario") + ylab("Modal share")  + 
  facet_grid(type_o ~ type_d)


## and in relative terms, for auto only, for origin only:

trip_count_relative = trip_count %>%
  mutate(mode = if_else(tripMode == "auto_toll", "auto", as.character(tripMode))) %>%
  group_by(scenario_name, type_o, type_d) %>% 
  mutate(total = sum(trips))

trip_count_relative_base = trip_count_relative %>%
  filter(scenario_group == "0") %>% 
  mutate(trips_base = trips, distance_base = distance) %>%
  ungroup() %>%
  select(mode, distance_base, trips_base, total_base = total, type_o, type_d)

trip_count_relative = trip_count_relative %>%
  filter(scenario_group != "0") %>%
  left_join(trip_count_relative_base, by = c("mode", "type_o", "type_d")) %>% 
  group_by(scenario_name, scenario_group, type_d, type_o, label, label2, mode, total, trips_base, total_base, distance_base) %>% 
  summarize(trips = sum(trips), distance = sum(distance))


trip_count_relative %>%
  filter(mode == "auto") %>% 
  filter(!is.na(type_d)) %>%
  ggplot(aes(x = scenario_name, y = (trips/total - trips_base/total_base)*100, group = paste(type_o, mode), color = type_o)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  theme_bw() + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1), legend.position = "bottom") + 
  xlab("Scenario") + ylab("Difference in auto modal share (p.p.)") + 
  geom_hline(yintercept = 0, color = "black") + 
  facet_grid(~type_d)



##Mode choice by trip purpose and type #####
trip_count = trips %>%
  filter(travelDistanceByCar_km < 10000 ) %>% 
  filter(tripState != "away", tripDestType != "EXTOVERSEAS") %>%
  group_by(tripMode, scenario_name, scenario_group, tripPurpose, tripState) %>%
  summarize(trips = n(), distance = sum(travelDistanceByCar_km))


trip_count = trip_count %>% left_join(scenario_parameters, by = c("scenario_name" = "scenario"))
trip_count %>% write_csv(paste(out_folder, "summary_3.csv", sep = ""))
########################################
##start from here if already written!
#########################################
trip_count = read_csv(paste(out_folder, "summary_3.csv", sep = ""))

trip_count = trip_count %>% mutate(tripMode = factor(tripMode, levels = ld_mode_order))

trip_count %>%
  ggplot(aes(x = scenario_name, y = trips, fill = tripMode)) +
  geom_bar(position  = "fill", stat = "identity") +
  scale_fill_manual(values = colors_ld_modes, name = "Mode") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1), legend.position = "bottom") + 
  xlab("Scenario") + ylab("Modal share")  + 
  facet_grid(tripState ~ tripPurpose)

## and in relative terms, for auto only, for origin only:
trip_count_relative = trip_count %>%
  mutate(mode = if_else(tripMode == "auto_toll", "auto", as.character(tripMode))) %>%
  group_by(scenario_name, tripPurpose, tripState) %>% 
  mutate(total = sum(trips))

trip_count_relative_base = trip_count_relative %>%
  filter(scenario_group == "0") %>% 
  mutate(trips_base = trips, distance_base = distance) %>%
  ungroup() %>%
  select(mode, distance_base, trips_base, total_base = total, tripPurpose, tripState)

trip_count_relative = trip_count_relative %>%
  filter(scenario_group != "0") %>%
  left_join(trip_count_relative_base, by = c("mode", "tripPurpose", "tripState")) %>% 
  group_by(scenario_name, scenario_group, tripState, tripPurpose, label, label2, mode, total, trips_base, total_base, distance_base) %>% 
  summarize(trips = sum(trips), distance = sum(distance))


trip_count_relative %>%
  filter(mode == "auto") %>% 
  ggplot(aes(x = scenario_name, y = (trips/total - trips_base/total_base)*100, group = paste(tripState, mode), color = tripState)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  theme_bw() + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1), legend.position = "bottom") + 
  xlab("Scenario") + ylab("Difference in auto modal share (p.p.)") + 
  geom_hline(yintercept = 0, color = "black") + 
  facet_grid(~tripPurpose)




##by scenario groups

groups = c("1", "2", "3", "4")
mode_of_interest = c("1" = "rail", "2" = "bus", "3" = "rail", "4" = "auto")
                     
for (i in 1:4){
  group = groups[i]
  trip_count %>%
    filter(scenario_group == 0 | scenario_group == group) %>%
    ggplot(aes(x = label2, y = trips, fill = tripMode)) +
    geom_bar(position  = "fill", stat = "identity") +
    scale_fill_manual(values = colors_ld_modes, name = "Mode") + 
    theme(axis.text.x = element_text(angle = 0), legend.position = "bottom") + 
    xlab("Scenario") + ylab("Modal share")

  if (save_plots){
    file_name = paste("tmp/",
                      gsub(x = gsub(x = Sys.time(),pattern = ":", replacement = ""),pattern = " ", replacement = ""),
                      "_modal_shares_fill_",group, ".jpg", sep = "" )
    ggsave(filename = file_name, device = "jpeg", width = 15, height = 10, units = "cm", scale = 2)
  }
}

#only for a particulat analysis of the random noise
i=3
trip_count %>%
  filter(scenario_group == 0 | scenario_group == group) %>%
  group_by(scenario_name) %>%
  mutate(total = sum(trips)) %>% 
  ggplot(aes(x = label2, y = trips/total, fill = tripMode, label = paste(sprintf("%.3f", trips/total * 100), "%", sep  =""))) +
  geom_bar(position  = "stack", stat = "identity") +
  scale_fill_manual(values = colors_ld_modes, name = "Mode") + 
  theme(axis.text.x = element_text(angle = 0), legend.position = "bottom") + 
  xlab("Scenario") + ylab("Modal share") + geom_text(position  = position_stack(vjust = 0.5))


re_scale_factor = 1/0.05

for (i in 1:3){
  group = groups[i]
  trips_scenario = trip_count %>%
    filter(scenario_group == group, tripMode == mode_of_interest[i])
  
  trips_base = trip_count %>% ungroup() %>% 
    filter(scenario_group == "0", tripMode == mode_of_interest[i]) %>% summarise(sum(trips))
  
  trips_scenario %>%
    ggplot(aes(x = label2,
               y = (trips - as.numeric(trips_base))/as.numeric(trips_base)*100, fill = tripMode,
               color = tripMode)) +
    geom_bar(position  = "dodge", stat = "identity") +
    scale_fill_manual(values = colors_ld_modes, name = "Mode") + 
    scale_color_manual(values = colors_ld_modes, name = "Mode") + 
    theme(axis.text.x = element_text(angle = 0), legend.position = "none") + 
    xlab("Scenario") + ylab("Difference with respect of the base scenario (%)") + 
    geom_hline(yintercept = 0, color = "black")
  
  if (save_plots){
    file_name = paste("tmp/",
                      gsub(x = gsub(x = Sys.time(),pattern = ":", replacement = ""),pattern = " ", replacement = ""),
                      "_modal_shares_parallel_",group, ".jpg", sep = "" )
    ggsave(filename = file_name, device = "jpeg", width = 15, height = 10, units = "cm", scale = 2)
  }
}

#toll scenarios are different, since there are two interesting modes
i = 4
group = groups[i]
trips_scenario = trip_count %>%
  filter(scenario_group == group, tripMode %in% c("auto", "auto_toll")) %>%
  group_by(scenario_name, label2) %>% 
  summarize(trips = sum(trips)) %>% mutate(tripMode = "auto")

trips_base = trip_count %>% ungroup() %>% 
  filter(scenario_group == "0", tripMode == mode_of_interest[i]) %>% summarise(sum(trips))

trips_scenario %>%
  ggplot(aes(x = label2,
             y = (trips - as.numeric(trips_base))/as.numeric(trips_base) * 100, fill = tripMode,
             color = tripMode)) +
  geom_bar(position  = "stack", stat = "identity") +
  scale_fill_manual(values = colors_ld_modes, name = "Mode") + 
  scale_color_manual(values = colors_ld_modes, name = "Mode") + 
  theme(axis.text.x = element_text(angle = 0), legend.position = "none") + 
  xlab("Scenario") + ylab("Difference with respect of the base scenario (%)") + 
  geom_hline(yintercept = 0, color = "black")

if (save_plots){
  file_name = paste("tmp/",
                    gsub(x = gsub(x = Sys.time(),pattern = ":", replacement = ""),pattern = " ", replacement = ""),
                    "_modal_shares_parallel_",group, ".jpg", sep = "" )
  ggsave(filename = file_name, device = "jpeg", width = 15, height = 10, units = "cm", scale = 2)
  
}



#first approach to toll payments
trips %>%
  filter(scenario_group == "4", international == FALSE) %>%
  filter(tripMode == "auto" | tripMode == "auto_toll") %>%
  mutate(factor = if_else(tripState == "daytrip", 2, if_else(tripState == "away", 0, 1))) %>% 
  group_by(scenario_name, tripMode) %>%
  summarize(n = n(), d = sum(as.numeric(distance_auto) * factor, na.rm = T), 
            toll_d =  sum(as.numeric(tollDistance_auto)* factor, na.rm= T),
            toll_d_no_toll =  sum(as.numeric(tollDistance_auto_noToll)* factor, na.rm= T)) %>% 
  write.table("clipboard", sep = "\t", row.names = F)



# trip_count %>% ggplot(aes(x = scenario_name, y = trips, fill = tripMode)) +
#   geom_bar(position  = "stack", stat = "identity") +
#   scale_fill_manual(values = colors_ld_modes) + 
#   theme(axis.text.x = element_text(angle = 90), legend.position = "bottom")
# 
# file_name = paste("tmp/",
#                   gsub(x = gsub(x = Sys.time(),pattern = ":", replacement = ""),pattern = " ", replacement = ""),
#                   "_modal_shares_stack.jpg", sep = "" )
# ggsave(filename = file_name, device = "jpeg", width = 15, height = 10, units = "cm", scale = 2)

re_scale_factor = 20

if (save_plots){
  trip_count %>%
    select(scenario = scenario_name, scenario_group, mode = tripMode, trips, distance) %>%
    mutate(trips = trips * re_scale_factor) %>%
    group_by(scenario) %>%
    mutate(total = sum(trips)) %>% mutate(share = trips / total * 100) %>% 
    pivot_wider(names_from = mode, values_from = c(trips, share,distance)) %>% 
    write_csv("tmp/modal_shares_by_scenario.csv")
}


  

##trips by length and mode

trip_count = trips %>%
  mutate(dist_bin = cut(travelDistanceByCar_km, breaks = seq(0,2000,20))) %>%
  filter(tripState != "away", tripDestType != "EXTOVERSEAS") %>%
  group_by(tripMode, scenario_name,dist_bin) %>%
  summarize(trips = n(), distance = sum(travelDistanceByCar_km))

trip_count = trip_count %>% mutate(tripMode = factor(tripMode, levels = ld_mode_order))

trip_count %>% filter(scenario_name == scenario_names[1]) %>% 
  ggplot(aes(x = as.numeric(dist_bin)*20, y = trips, fill = tripMode, color = tripMode)) + 
  geom_bar(position  = position_fill(reverse  =T), stat = "identity") +
  scale_fill_manual(values = colors_ld_modes, name = "Mode") + 
  scale_color_manual(values = colors_ld_modes, name = "Mode") + 
  theme(axis.text.x = element_text(angle = 90), legend.position = "bottom") + 
  xlab("Distance (km)") + ylab("Share")

if (save_plots){
  file_name = paste("tmp/",
                    gsub(x = gsub(x = Sys.time(),pattern = ":", replacement = ""),pattern = " ", replacement = ""),
                    "_modal_shares_by_dis_base.jpg", sep = "" )
  ggsave(filename = file_name, device = "jpeg", width = 15, height = 10, units = "cm", scale = 2)
}


##using shares instead of counts
# 
# trips2 = trips %>% filter(scenario_group == "3" | scenario_group == "0") %>% 
#   select(tripId, international, tripPurpose, 
#          tripState, travelDistanceByCar_km, 
#          utility_air, utility_auto,
#          utility_auto_noToll, utility_rail,
#          utility_bus, tripMode,
#          scenario_name, scenario_group, 
#          tripDestType) %>%
#   mutate(utility_auto = as.numeric(utility_auto)) %>% 
#   mutate(utility_auto_noToll = as.numeric(utility_auto_noToll)) %>% 
#   mutate(utility_air = as.numeric(utility_air)) %>% 
#   mutate(utility_rail = as.numeric(utility_rail)) %>% 
#   mutate(utility_bus = as.numeric(utility_bus))
#   
#   
#   
# trips2[is.na(trips2)] = 0
#   
# trips2 = trips2 %>%
#   mutate(sum = utility_auto + utility_auto_noToll + utility_air + 
#            utility_rail + utility_bus)
# 
# trips2 = trips2 %>%
#   mutate(utility_auto = if_else(sum == 0, 1,utility_auto))
#  
# 
# 
# trip_shares = trips2 %>%
#   filter(travelDistanceByCar_km < 10000 ) %>% 
#   filter(tripState != "away", tripDestType != "EXTOVERSEAS") %>%
#   group_by(scenario_name, scenario_group) %>%
#   summarize(trips = n(), auto = mean(utility_auto), auto_noToll = mean(utility_auto_noToll), 
#             rail = mean(utility_rail) , bus = mean(utility_bus),
#             air = mean(utility_air))
# 
# trip_shares = trip_shares %>% left_join(scenario_parameters, by = c("scenario_name" = "scenario"))
# 
# trip_shares = trip_shares %>% pivot_longer(cols = c(auto, auto_noToll,air, rail, bus))
# 
# trip_shares = trip_shares %>% mutate(name = factor(name, levels = ld_mode_order))
# 
# 
# trip_shares %>% 
#   filter(name != "auto_toll") %>% 
#   ggplot(aes(x = label2, y = value, fill = name, label = paste(sprintf("%.3f", value * 100), "%", sep  =""))) +
#   geom_bar(position  = "stack", stat = "identity") +
#   scale_fill_manual(values = colors_ld_modes, name = "Mode") + 
#   theme(axis.text.x = element_text(angle = 0), legend.position = "bottom") + 
#   xlab("Scenario") + ylab("Modal share") + 
#   geom_text(position = position_stack(vjust = 0.5))







