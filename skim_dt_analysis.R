pacman::p_load(readr, dplyr, tidyr, ggplot2)

data_analysis = read_csv("c:/models/transit_germany/output/skims/ld_train_with_walk_2/deutschland_takt_summary_intermediate.csv")

data_analysis_long = data_analysis %>% pivot_longer(cols = 2:13)

unique(data_analysis_long$name)

data_analysis_long %>% filter(name == "time_before_s" | name == "time_after_s") %>%
  ggplot(aes(x=value/3600, color = name, fill = name)) + geom_density(alpha = 0.1, size = 2) + theme_bw() + 
  xlab("Total travel time (h)") + ylab("Frequency") + 
  scale_color_manual(values = c("time_after_s" = "#ae9dab", "time_before_s" = "#5e3c58")) + 
  scale_fill_manual(values = c("time_after_s" = "#ae9dab", "time_before_s" = "#5e3c58"))

ggsave(filename = "tmp/travel_time_scen_3_intermediate.jpg", device = "jpeg",
       width = 15, height= 10, units = "cm", scale = 1.5)

data_analysis_long %>% filter(name == "in_veh_before_s" | name == "in_veh_after") %>%
  ggplot(aes(x=value/3600, color = name)) + geom_density(alpha = 0.1)
