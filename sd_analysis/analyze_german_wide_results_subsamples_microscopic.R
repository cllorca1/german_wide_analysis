pacman::p_load(dplyr, readr, ggplot2, tidyr)

folder = "c:/models/mito/germany/scenOutput/"
file_name = "/2011/microData/trips.csv"

trips = data.frame()

for (i in 0:0){
  path = paste(folder, "sd_5_percent_",i,file_name, sep = "")
  this_trips = read_csv(path) %>% mutate(subsample = i)
  trips = trips %>% bind_rows(this_trips)
}


##manually add the old 1 percent
path = paste(folder, "sd_1_percent_20210603",file_name, sep = "")
this_trips = read_csv(path) %>% mutate(subsample = 22)
trips = trips %>% bind_rows(this_trips)


path = paste(folder, "sd_1_percent_20210531",file_name, sep = "")
this_trips = read_csv(path) %>% mutate(subsample = 21)
trips = trips %>% bind_rows(this_trips)


trips %>% ggplot(aes(x = distance, fill = purpose, group = subsample)) + geom_density(alpha = 0.2) +
  theme_bw() + facet_wrap(.~purpose) + scale_x_log10()



trips %>% ggplot(aes(x = originX, fill = purpose, group = subsample)) + geom_density(alpha = 0.2) +
  theme_bw() + facet_wrap(.~purpose)

trips %>% ggplot(aes(x = destinationY, fill = purpose, group = subsample)) + geom_density(alpha = 0.2) +
  theme_bw() + facet_wrap(.~purpose)


mode_colors = c("autoDriver" = "#878787",
                "autoPassenger" = "#a9a9a9",
                "train" = "#789ec9",
                "tramOrMetro" = "#5c768d",
                "bus" = "#50546d",
                "bicycle" = "#87c77d",
                "walk" = "#53815b")

mode_order = c("autoDriver","autoPassenger","train" ,"tramOrMetro" ,"bus","bicycle" ,"walk")

trips$mode = factor(trips$mode, levels = mode_order)


trips %>% ggplot(aes(x = subsample, fill = mode)) + geom_bar(stat = "count", position = "fill") +
  theme_bw() + facet_grid(.~purpose) + scale_fill_manual(values = mode_colors)



trips %>% group_by(subsample) %>% summarise(n())
