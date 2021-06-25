pacman::p_load(data.table, dplyr, ggplot2, tidyr, readr, plotly, sf, tmap, rgeos, RColorBrewer)

out_folder= "Y:/projects/2019/BASt/data/input_files_LD/germanymodel/output/"
ld_mode_order = c("auto", "auto_noToll", "bus", "rail", "air")
colors_ld_modes  =c("auto" = "#B0B0B0", "auto_noToll" = "#6D6D6D", "rail"  ="#5e3c58", 
                    "bus" = "#c7bbc9", "air" = "#83adb5")


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
              "4_A_2")

scenario_names = scenarios
scenario_groups = substr(scenarios, 1,1)

scenario_parameters = read_csv("tmp/scenario_labels.csv")
scenario_parameters = scenario_parameters %>%
  mutate(label = paste(scenario, "\n(", var1, "=", val1, ", ",  var2, "=", val2, ")", sep  ="")) %>%
  mutate(label2 = paste(scenario, "\n", var1, "=", val1, "\n",  var2, "=", val2, "", sep  =""))

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

#summary(as.factor(trips$tripMode))


##Destination choice
# trips_by_distance = trips %>%
#   filter(tripState != "away", tripDestType != "EXTOVERSEAS") %>%
#   mutate(dist_bin = cut(travelDistanceByCar_km, breaks = seq(0,2000,20))) %>%
#   group_by(tripPurpose, tripDestType, scenario_name, scenario_group) %>%
#   mutate(total = n()) %>% group_by(tripPurpose, tripDestType, scenario_name, dist_bin, total) %>%
#   summarise(n = n())
# 
# trips_by_distance %>% ggplot(aes(x = as.numeric(dist_bin)*20, y = n/total * 100, color = scenario_name)) +
#   geom_line(stat = "identity", size = 1) + facet_wrap(tripDestType~tripPurpose, scales = "free_y", ncol = 3) + 
#   geom_vline(xintercept = 50, color = "gray70") + theme_bw()
# 
# 
# trips_by_distance %>% ggplot(aes(x = as.numeric(dist_bin)*20, y = n/total * 100, color = scenario_name)) +
#   geom_line(stat = "identity", size = 1) + facet_wrap(tripDestType~tripPurpose, scales = "free_y", ncol = 3) + 
#   geom_vline(xintercept = 50, color = "gray70") + theme_bw() + scale_x_log10()
# 
# 
# 
# file_name = paste("tmp/", gsub(x = gsub(x = Sys.time(),pattern = ":", replacement = ""),pattern = " ", replacement = ""),"_dist_distr.jpg", sep = "" )
# ggsave(filename = file_name, device = "jpeg", width = 15, height = 10, units = "cm", scale = 2)


##Mode choice
trip_count = trips %>%
  filter(travelDistanceByCar_km < 10000 ) %>% 
  filter(tripState != "away", tripDestType != "EXTOVERSEAS") %>%
  group_by(tripMode, scenario_name, scenario_group) %>%
  summarize(trips = n(), distance = sum(travelDistanceByCar_km))

trip_count = trip_count %>% mutate(tripMode = factor(tripMode, levels = ld_mode_order))

trip_count = trip_count %>% left_join(scenario_parameters, by = c("scenario_name" = "scenario"))


trip_count %>%
  ggplot(aes(x = label, y = trips, fill = tripMode)) +
  geom_bar(position  = "fill", stat = "identity") +
  scale_fill_manual(values = colors_ld_modes, name = "Mode") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1), legend.position = "bottom") + 
  xlab("Scenario") + ylab("Modal share")


trip_count %>%
  ggplot(aes(x = label, y = trips, fill = tripMode)) +
  geom_bar(position  = "fill", stat = "identity") +
  scale_fill_manual(values = colors_ld_modes, name = "Mode") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1), legend.position = "bottom") + 
  xlab("Scenario") + ylab("Modal share") + 
  geom_hline(yintercept = .196, linetype = "dashed")


file_name = paste("tmp/",
                  gsub(x = gsub(x = Sys.time(),pattern = ":", replacement = ""),pattern = " ", replacement = ""),
                  "_modal_shares_fill.jpg", sep = "" )
ggsave(filename = file_name, device = "jpeg", width = 15, height = 10, units = "cm", scale = 2)


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
  
  file_name = paste("tmp/",
                    gsub(x = gsub(x = Sys.time(),pattern = ":", replacement = ""),pattern = " ", replacement = ""),
                    "_modal_shares_fill_",group, ".jpg", sep = "" )
  ggsave(filename = file_name, device = "jpeg", width = 15, height = 10, units = "cm", scale = 2)
  
 
}

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
  
  file_name = paste("tmp/",
                    gsub(x = gsub(x = Sys.time(),pattern = ":", replacement = ""),pattern = " ", replacement = ""),
                    "_modal_shares_parallel_",group, ".jpg", sep = "" )
  ggsave(filename = file_name, device = "jpeg", width = 15, height = 10, units = "cm", scale = 2)
}

#toll scenarios are different, since there are two interesting modes
i = 4
group = groups[i]
trips_scenario = trip_count %>%
  filter(scenario_group == group, tripMode %in% c("auto", "auto_noToll")) %>%
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

file_name = paste("tmp/",
                  gsub(x = gsub(x = Sys.time(),pattern = ":", replacement = ""),pattern = " ", replacement = ""),
                  "_modal_shares_parallel_",group, ".jpg", sep = "" )
ggsave(filename = file_name, device = "jpeg", width = 15, height = 10, units = "cm", scale = 2)





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

trip_count %>%
  select(scenario = scenario_name, scenario_group, mode = tripMode, trips, distance) %>%
  mutate(trips = trips * re_scale_factor) %>%
  group_by(scenario) %>%
  mutate(total = sum(trips)) %>% mutate(share = trips / total * 100) %>% 
  pivot_wider(names_from = mode, values_from = c(trips, share,distance)) %>% 
  write_csv("tmp/modal_shares_by_scenario.csv")
  

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

file_name = paste("tmp/",
                  gsub(x = gsub(x = Sys.time(),pattern = ":", replacement = ""),pattern = " ", replacement = ""),
                  "_modal_shares_by_dis_base.jpg", sep = "" )
ggsave(filename = file_name, device = "jpeg", width = 15, height = 10, units = "cm", scale = 2)



# 
# trip_count %>% 
#   ggplot(aes(x = as.numeric(dist_bin)*20, y = trips, fill = tripMode, color = tripMode)) + 
#   geom_bar(position  = position_fill(reverse  =T), stat = "identity") +
#   scale_fill_manual(values = colors_ld_modes) + 
#   scale_color_manual(values = colors_ld_modes) + 
#   theme(axis.text.x = element_text(angle = 90), legend.position = "bottom") + 
#   facet_wrap(.~scenario_name)
# 
# file_name = paste("tmp/",
#                   gsub(x = gsub(x = Sys.time(),pattern = ":", replacement = ""),pattern = " ", replacement = ""),
#                   "_modal_shares_by_dis_all.jpg", sep = "" )
# ggsave(filename = file_name, device = "jpeg", width = 15, height = 10, units = "cm", scale = 2)


# trips_by_zone = trips %>% filter(tripState != "away", tripDestType != "EXTOVERSEAS") %>%
#   filter(scenario_name == "0:base") %>% 
#   group_by(tripOriginZone, tripMode) %>%
#   summarise(trips = n())
# 
# trips_by_zone = trips_by_zone %>% ungroup() %>% group_by(tripOriginZone) %>% mutate(total = sum(trips))
# 
# zone_shp = st_read("C:/models/germanymodel/input/trafficAssignment/zonesShapefile/zonesNew.shp")
# 
# 
# p =  tm_basemap(leaflet::providers$CartoDB)
# 
# #for (mode in ld_mode_order){
# mode = "auto"
# trips_by_zone_this_mode = trips_by_zone %>% filter(tripMode == mode) %>%
#   ungroup()
# this_shp = zone_shp %>%
#   select(TAZ_id) %>%
#   left_join(trips_by_zone_this_mode , by = c("TAZ_id" = "tripOriginZone")) 
# this_shp$share = this_shp$trips / this_shp$total
# p = p + tm_shape(shp = this_shp, name = "mode") +
#   tm_fill(col = "share", alpha = 0.7, border.alpha = 0.1, convert2density = F, style = "quantile")
# #}
# 
# tmap_leaflet(p)


# trip_count_2 = trips %>%
#   mutate(dist_bin = cut(travelDistanceByCar_km, breaks = seq(0,2000,20))) %>%
#   filter(tripState != "away", tripDestType != "EXTOVERSEAS") %>%
#   group_by(tripMode, tripDestType, scenario_name,dist_bin) %>%
#   summarize(trips = n(), distance = sum(travelDistanceByCar_km))
# 
# trip_count_2 = trip_count_2 %>% mutate(tripMode = factor(tripMode, levels = ld_mode_order))
# 
#  
# trip_count_2 %>% filter(scenario_name == scenario_names[1]) %>% 
#   ggplot(aes(x = as.numeric(dist_bin)*20, y = trips, fill = tripMode, color = tripMode)) + 
#   geom_bar(position  = position_fill(reverse  =T), stat = "identity") +
#   scale_fill_manual(values = colors_ld_modes) + 
#   scale_color_manual(values = colors_ld_modes) + 
#   facet_wrap(.~tripDestType) + 
#   theme(axis.text.x = element_text(angle = 90), legend.position = "bottom")
# 
# file_name = paste("tmp/",
#                   gsub(x = gsub(x = Sys.time(),pattern = ":", replacement = ""),pattern = " ", replacement = ""),
#                   "_modal_shares_by_dis_base_by_segment.jpg", sep = "" )
# ggsave(filename = file_name, device = "jpeg", width = 15, height = 10, units = "cm", scale = 2)
# 
# trip_count_2 %>% filter(scenario_name == scenario_names[1]) %>% 
#   ggplot(aes(x = as.numeric(dist_bin)*20, y = trips, fill = tripMode, color = tripMode)) + 
#   geom_bar(position  = position_stack(reverse  =T), stat = "identity") +
#   scale_fill_manual(values = colors_ld_modes) + 
#   scale_color_manual(values = colors_ld_modes) + 
#   facet_wrap(.~tripDestType, scales = "free_y") + 
#   theme(axis.text.x = element_text(angle = 90), legend.position = "bottom")
# 
