pacman::p_load(dplyr, readr, ggplot2, tidyr)

#+ fig.width = 10, fig.height = 7, dev = 'svg', message = FALSE

folder = "c:/models/mito/germany/scenOutput/"
file_name = "/2011/microData/trips.csv"

trips = data.frame()



path = paste(folder, "sd_1_percent_20210624", file_name, sep = "")
this_trips = read_csv(path) %>% mutate(year = 2011) %>% mutate(scale = 0.01)
trips = trips %>% bind_rows(this_trips)


path = paste(folder, "sd_5_percent_2030_20210701", file_name, sep = "")
##need to take a 20% to make uniform scales (1%)
this_trips = read_csv(path) %>% mutate(year = 2030) %>% mutate(scale = 0.05) %>% sample_frac(0.2)
trips = trips %>% bind_rows(this_trips)

trips %>%
  sample_frac(0.01) %>% 
  ggplot(aes(x = distance, color = as.factor(year), group = year)) + geom_density(alpha = 0.2) +
  theme_bw() + facet_wrap(.~purpose) + scale_x_log10()

mode_colors = c("autoDriver" = "#878787",
                "autoPassenger" = "#a9a9a9",
                "train" = "#789ec9",
                "tramOrMetro" = "#5c768d",
                "bus" = "#50546d",
                "bicycle" = "#87c77d",
                "walk" = "#53815b")

mode_order = c("autoDriver","autoPassenger","train" ,"tramOrMetro" ,"bus","bicycle" ,"walk")

trips$mode = factor(trips$mode, levels = mode_order)

##modal share 
trips %>% ggplot(aes(x = as.factor(year), fill = mode)) + geom_bar(stat = "count", position = "fill") +
  theme_bw() + scale_fill_manual(values = mode_colors)



##modal share by purpose

trips %>% ggplot(aes(x = as.factor(year), fill = mode)) + geom_bar(stat = "count", position = "fill") +
  theme_bw() + facet_grid(.~purpose) + scale_fill_manual(values = mode_colors)


trips %>% ggplot(aes(x = as.factor(year), fill = mode)) + geom_bar(stat = "count", position = "stack") +
  theme_bw() + facet_grid(.~purpose) + scale_fill_manual(values = mode_colors)

# ##by area type
# zonal_data = read_csv("scenarios/zoneSystemDE_2col.csv")
# zonal_data$BBSR_type = recode(as.character(zonal_data$BBSR_type), 
#                               "10" = "10: core",
#                               "20" = "20: medium_city",
#                               "30" = "30:town",
#                               "40" = "40: rural")
# 
# 
# trips = trips %>% left_join(zonal_data, by = c("origin" = "Zone")) %>%
#   mutate(type_o = BBSR_type) %>% select(-BBSR_type)
# 
# # trips = trips %>% left_join(zonal_data, by = c("destination" = "Zone")) %>%
# #   mutate(type_d = BBSR_type) %>% select(-BBSR_type)
# 
# 
# trips %>% ggplot(aes(x = as.factor(year), fill = mode)) + geom_bar(stat = "count", position = "fill") +
#   theme_bw() + facet_grid(.~type_o) + scale_fill_manual(values = mode_colors)





