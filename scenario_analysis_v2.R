pacman::p_load(data.table, dplyr, ggplot2, tidyr, readr, plotly, sf, tmap, rgeos, RColorBrewer)

out_folder= "d:/simulations/ld/"
ld_mode_order = c("auto", "auto_noToll", "bus", "rail", "air")
colors_ld_modes  =c("auto" = "#B0B0B0", "auto_noToll" = "#6D6D6D", "rail"  ="#5e3c58", 
                    "bus" = "#c7bbc9", "air" = "#83adb5")


scenarios = c("5percent_weekday_calibrated_20210525", 
              "5percent_base_noHoliday_AE",
              "5percent_scenario1_0.12cost_noHoliday",
              "5percent_scenario2_noHoliday",
              "5percent_scenario3_low_noHoliday",
              "5percent_scenario3_intermediate_noHoliday",
              "5percent_scenario3_extreme_noHoliday",
              "5percent_weekday_sce4_0.000",
              "5percent_weekday_sce4_0.015",
              "5percent_weekday_sce4_0.050")

scenario_names = c("40:base_wei",
                   "0:base_alona",
                   "1: scenario1",
                   "2: scenario2",
                   "31: scen3 - low",
                   "32: scen3 - intermed.",
                   "33: scen3 - extreme",
                   "41: toll_0.0",
                   "42: toll_1.5",
                   "43: toll_5.0")

trips = data.frame()

for (i in 1:length(scenarios)){
  scenario_folder = scenarios[i]
  this_trips = read_csv(paste(out_folder,scenario_folder, "/trips.csv", sep  =""))
  this_trips$scenario_name = scenario_names[i]
  trips = trips %>% bind_rows(this_trips)
}

summary(as.factor(trips$tripMode))


##Destination choice
trips_by_distance = trips %>%
  filter(tripState != "away", tripDestType != "EXTOVERSEAS") %>%
  mutate(dist_bin = cut(travelDistanceByCar_km, breaks = seq(0,2000,20))) %>%
  group_by(tripPurpose, tripDestType, scenario_name) %>%
  mutate(total = n()) %>% group_by(tripPurpose, tripDestType, scenario_name, dist_bin, total) %>%
  summarise(n = n())

trips_by_distance %>% ggplot(aes(x = as.numeric(dist_bin)*20, y = n/total * 100, color = scenario_name)) +
  geom_line(stat = "identity", size = 1) + facet_wrap(tripDestType~tripPurpose, scales = "free_y", ncol = 3) + 
  geom_vline(xintercept = 50, color = "gray70") + theme_bw()


trips_by_distance %>% ggplot(aes(x = as.numeric(dist_bin)*20, y = n/total * 100, color = scenario_name)) +
  geom_line(stat = "identity", size = 1) + facet_wrap(tripDestType~tripPurpose, scales = "free_y", ncol = 3) + 
  geom_vline(xintercept = 50, color = "gray70") + theme_bw() + scale_x_log10()



file_name = paste("tmp/", gsub(x = gsub(x = Sys.time(),pattern = ":", replacement = ""),pattern = " ", replacement = ""),"_dist_distr.jpg", sep = "" )
ggsave(filename = file_name, device = "jpeg", width = 15, height = 10, units = "cm", scale = 2)


##Mode choice
trip_count = trips %>%
  filter(travelDistanceByCar_km < 10000 ) %>% 
  filter(tripState != "away", tripDestType != "EXTOVERSEAS") %>%
  group_by(tripMode, scenario_name) %>%
  summarize(trips = n(), distance = sum(travelDistanceByCar_km))

trip_count = trip_count %>% mutate(tripMode = factor(tripMode, levels = ld_mode_order))


trip_count %>% ggplot(aes(x = scenario_name, y = trips, fill = tripMode)) +
  geom_bar(position  = "fill", stat = "identity") +
  scale_fill_manual(values = colors_ld_modes) + 
  theme(axis.text.x = element_text(angle = 90), legend.position = "bottom") + 
  xlab("Scenario") + ylab("Modal share")

file_name = paste("tmp/",
                  gsub(x = gsub(x = Sys.time(),pattern = ":", replacement = ""),pattern = " ", replacement = ""),
                  "_modal_shares_fill.jpg", sep = "" )
ggsave(filename = file_name, device = "jpeg", width = 15, height = 10, units = "cm", scale = 2)



trip_count %>% ggplot(aes(x = scenario_name, y = trips, fill = tripMode)) +
  geom_bar(position  = "stack", stat = "identity") +
  scale_fill_manual(values = colors_ld_modes) + 
  theme(axis.text.x = element_text(angle = 90), legend.position = "bottom")

file_name = paste("tmp/",
                  gsub(x = gsub(x = Sys.time(),pattern = ":", replacement = ""),pattern = " ", replacement = ""),
                  "_modal_shares_stack.jpg", sep = "" )
ggsave(filename = file_name, device = "jpeg", width = 15, height = 10, units = "cm", scale = 2)


trip_count %>% ggplot(aes(x = scenario_name, y = distance, fill = tripMode)) +
  geom_bar(position  = "stack", stat = "identity") +
  scale_fill_manual(values = colors_ld_modes) + 
  theme(axis.text.x = element_text(angle = 90), legend.position = "bottom")




##trips by length and mode


trip_count = trips %>%
  mutate(dist_bin = cut(travelDistanceByCar_km, breaks = seq(0,2000,20))) %>%
  filter(tripState != "away", tripDestType != "EXTOVERSEAS") %>%
  group_by(tripMode, scenario_name,dist_bin) %>%
  summarize(trips = n(), distance = sum(travelDistanceByCar_km))

trip_count = trip_count %>% mutate(tripMode = factor(tripMode, levels = ld_mode_order))

trip_count %>% filter(scenario_name == "0:base_alona") %>% 
  ggplot(aes(x = as.numeric(dist_bin)*20, y = trips, fill = tripMode, color = tripMode)) + 
  geom_bar(position  = position_fill(reverse  =T), stat = "identity") +
  scale_fill_manual(values = colors_ld_modes) + 
  scale_color_manual(values = colors_ld_modes) + 
  theme(axis.text.x = element_text(angle = 90), legend.position = "bottom")

file_name = paste("tmp/",
                  gsub(x = gsub(x = Sys.time(),pattern = ":", replacement = ""),pattern = " ", replacement = ""),
                  "_modal_shares_by_dis_base.jpg", sep = "" )
ggsave(filename = file_name, device = "jpeg", width = 15, height = 10, units = "cm", scale = 2)



trip_count %>% filter(scenario_name == "0:base_alona") %>% 
  ggplot(aes(x = as.numeric(dist_bin)*20, y = trips, fill = tripMode, color = tripMode)) + 
  geom_bar(position  = position_stack(reverse  =T), stat = "identity") +
  scale_fill_manual(values = colors_ld_modes) + 
  scale_color_manual(values = colors_ld_modes) + 
  theme(axis.text.x = element_text(angle = 90), legend.position = "bottom")


trip_count %>% filter(scenario_name == "0:base_alona") %>% 
  ggplot(aes(x = as.numeric(dist_bin)*20, y = distance, fill = tripMode, color = tripMode)) + 
  geom_bar(position  = position_stack(reverse  =T), stat = "identity") +
  scale_fill_manual(values = colors_ld_modes) + 
  scale_color_manual(values = colors_ld_modes) + 
  theme(axis.text.x = element_text(angle = 90), legend.position = "bottom")


trip_count %>% 
  ggplot(aes(x = as.numeric(dist_bin)*20, y = trips, fill = tripMode, color = tripMode)) + 
  geom_bar(position  = position_fill(reverse  =T), stat = "identity") +
  scale_fill_manual(values = colors_ld_modes) + 
  scale_color_manual(values = colors_ld_modes) + 
  theme(axis.text.x = element_text(angle = 90), legend.position = "bottom") + 
  facet_wrap(.~scenario_name)

file_name = paste("tmp/",
                  gsub(x = gsub(x = Sys.time(),pattern = ":", replacement = ""),pattern = " ", replacement = ""),
                  "_modal_shares_by_dis_all.jpg", sep = "" )
ggsave(filename = file_name, device = "jpeg", width = 15, height = 10, units = "cm", scale = 2)


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


trip_count_2 = trips %>%
  mutate(dist_bin = cut(travelDistanceByCar_km, breaks = seq(0,2000,20))) %>%
  filter(tripState != "away", tripDestType != "EXTOVERSEAS") %>%
  group_by(tripMode, tripDestType, scenario_name,dist_bin) %>%
  summarize(trips = n(), distance = sum(travelDistanceByCar_km))

trip_count_2 = trip_count_2 %>% mutate(tripMode = factor(tripMode, levels = ld_mode_order))

trip_count_2 %>% filter(scenario_name == "0:base_alona") %>% 
  ggplot(aes(x = as.numeric(dist_bin)*20, y = trips, fill = tripMode, color = tripMode)) + 
  geom_bar(position  = position_fill(reverse  =T), stat = "identity") +
  scale_fill_manual(values = colors_ld_modes) + 
  scale_color_manual(values = colors_ld_modes) + 
  facet_wrap(.~tripDestType) + 
  theme(axis.text.x = element_text(angle = 90), legend.position = "bottom")

file_name = paste("tmp/",
                  gsub(x = gsub(x = Sys.time(),pattern = ":", replacement = ""),pattern = " ", replacement = ""),
                  "_modal_shares_by_dis_base_by_segment.jpg", sep = "" )
ggsave(filename = file_name, device = "jpeg", width = 15, height = 10, units = "cm", scale = 2)

trip_count_2 %>% filter(scenario_name == "0:base_alona") %>% 
  ggplot(aes(x = as.numeric(dist_bin)*20, y = trips, fill = tripMode, color = tripMode)) + 
  geom_bar(position  = position_stack(reverse  =T), stat = "identity") +
  scale_fill_manual(values = colors_ld_modes) + 
  scale_color_manual(values = colors_ld_modes) + 
  facet_wrap(.~tripDestType, scales = "free_y") + 
  theme(axis.text.x = element_text(angle = 90), legend.position = "bottom")

