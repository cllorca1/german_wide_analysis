pacman::p_load(dplyr, readr, ggplot2, tidyr)


trips = read_csv("c:/models/mito/germany/scenOutput/sd_1_percent_20210614/2011/microData/trips.csv")

trips %>% ggplot(aes(x = distance, color = purpose)) + stat_ecdf(size = 1) +
  theme_bw() + xlim(0,200)

file_name = paste("tmp/",
                  gsub(x = gsub(x = Sys.time(),pattern = ":", replacement = ""),pattern = " ", replacement = ""),
                  "_sd_dist_by_purpose.jpg", sep = "" )
ggsave(filename = file_name, device = "jpeg", width = 15, height = 10, units = "cm", scale = 2)



trips %>% ggplot(aes(x = distance, fill = purpose)) + geom_density() +
  theme_bw() + facet_wrap(.~purpose) + scale_x_log10()


trips %>%
  mutate(intrazonal = if_else(origin == destination, 0.3, 0.5)) %>% 
  ggplot(aes(x = distance, fill = purpose, group = as.factor(intrazonal), alpha = intrazonal)) + geom_density() +
  theme_bw() + facet_wrap(.~purpose) + scale_x_log10()


zonal_data = read_csv("c:/models/mito/germany/input/zoneSystemDE_2col.csv")


zonal_data$BBSR_type = recode(as.character(zonal_data$BBSR_type), 
                              "10" = "10: core",
                              "20" = "20: medium_city",
                              "30" = "30:town",
                              "40" = "40: rural")

trips = trips %>% left_join(zonal_data, by = (c("origin"="Zone")))


trips = trips %>% mutate(intrazonal = if_else(origin == destination, 1, 0))

trip_summary_2 = trips %>% group_by(purpose, BBSR_type) %>% summarize(n = n(), intrazonal = sum(intrazonal)) 

ggplot(trip_summary_2, aes(x = BBSR_type, y = intrazonal/n * 100)) +
  geom_bar(stat = "identity") +
  theme_bw() + 
  facet_wrap(.~purpose) + 
  theme(axis.text.x = element_text(angle  =90))




trips = trips %>% mutate(distance_bin = cut(distance, breaks  = c(0,5,10,15,20,25,30,35,40,45,50,1e6), include.lowest = T))

trip_summary = trips %>% group_by(purpose, mode, distance_bin) %>% summarize(n = n())

mode_colors = c("autoDriver" = "#878787",
                "autoPassenger" = "#a9a9a9",
                "train" = "#789ec9",
                "tramOrMetro" = "#5c768d",
                "bus" = "#50546d",
                "bicycle" = "#87c77d",
                "walk" = "#53815b")

mode_order = c("autoDriver","autoPassenger","train" ,"tramOrMetro" ,"bus","bicycle" ,"walk")

trip_summary$mode = factor(trip_summary$mode, levels = mode_order)

ggplot(trip_summary, aes(x = as.factor(distance_bin), y = n, fill = as.factor(mode), alpha  =log(n))) +
  geom_bar(stat = "identity", position = "fill") +
  scale_fill_manual(values = mode_colors) + 
  theme_bw() + 
  facet_wrap(.~purpose) + 
  theme(axis.text.x = element_text(angle  =90))

file_name = paste("tmp/",
                  gsub(x = gsub(x = Sys.time(),pattern = ":", replacement = ""),pattern = " ", replacement = ""),
                  "sd_modal_shares_by_dis.jpg", sep = "" )
ggsave(filename = file_name, device = "jpeg", width = 15, height = 10, units = "cm", scale = 2)

ggplot(trip_summary, aes(x = as.factor(distance_bin), y = n, fill = as.factor(mode))) +
  geom_bar(stat = "identity", position = "stack") +
  scale_fill_manual(values = mode_colors) + 
  theme_bw() + 
  facet_wrap(.~purpose) + 
  theme(axis.text.x = element_text(angle  =90))

file_name = paste("tmp/",
                  gsub(x = gsub(x = Sys.time(),pattern = ":", replacement = ""),pattern = " ", replacement = ""),
                  "sd_trips_by_dis.jpg", sep = "" )
ggsave(filename = file_name, device = "jpeg", width = 15, height = 10, units = "cm", scale = 2)


trip_summary_2 = trips %>% group_by(purpose, mode, BBSR_type) %>% summarize(n = n()) 

trip_summary_2$mode = factor(trip_summary_2$mode, levels = mode_order)

ggplot(trip_summary_2, aes(x = BBSR_type, y = n, fill = as.factor(mode))) +
  geom_bar(stat = "identity", position = "fill") +
  scale_fill_manual(values = mode_colors) + 
  theme_bw() + 
  facet_wrap(.~purpose) + 
  theme(axis.text.x = element_text(angle  =90))

file_name = paste("tmp/",
                  gsub(x = gsub(x = Sys.time(),pattern = ":", replacement = ""),pattern = " ", replacement = ""),
                  "sd_trips_by_area_type.jpg", sep = "" )
ggsave(filename = file_name, device = "jpeg", width = 15, height = 10, units = "cm", scale = 2)


trip_summary_3 = trip_summary_2 %>% group_by(purpose, mode) %>% summarize(n = sum(n)) 

ggplot(trip_summary_3, aes(x = purpose, y = n, fill = as.factor(mode))) +
  geom_bar(stat = "identity", position = "fill") +
  scale_fill_manual(values = mode_colors) + 
  theme_bw() + scale_y_continuous(expand = c(0,0))


