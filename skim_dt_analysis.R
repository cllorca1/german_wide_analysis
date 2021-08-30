pacman::p_load(readr, dplyr, tidyr, ggplot2)

alternatives = c("low", 
          "low_b", 
          "intermediate", 
          "extreme")

data_analysis_long = data.frame()

path = paste("c:/models/transit_germany/output/skims/ld_train_with_walk_2/deutschland_takt_summary_",
             alternatives[1],
             ".csv", sep = "")
this_data = read_csv(path)
this_data = this_data %>% mutate(alternative = "base")

fields_before = c("origin", "destination", "access_s", "egress_s", 
                  "distance_m", "transfers", "waiting_before_s", "in_veh_before_s", "time_before_s", 
                  "share_time_before")
fields_after = c("origin", "destination", "access_s", "egress_s", 
                  "distance_m", "transfers", "waiting_before_s", "in_veh_after", "time_after_s", 
                  "share_time_after")

this_data_long = this_data %>% pivot_longer(cols = 3:13) %>% filter(name %in% fields_before)

data_analysis_long = data_analysis_long %>% bind_rows(this_data_long)

for (alternative in alternatives){
  path = paste("c:/models/transit_germany/output/skims/ld_train_with_walk_2/deutschland_takt_summary_",
               alternative,
               ".csv", sep = "")
  this_data = read_csv(path)
  this_data = this_data %>% mutate(alternative = alternative)
  this_data_long = this_data %>% pivot_longer(cols = 3:13) %>% filter(name %in% fields_after)
  data_analysis_long = data_analysis_long %>% bind_rows(this_data_long)
}


order = c("base", "low", "low_b" , "intermediate" , "extreme" )
labels = c("Basis", "3_A_1", "3_A_2" , "3_B_1" , "3_C_1" )

data_analysis_long$alternative = factor(data_analysis_long$alternative, levels = order, labels = labels)
colors = c("Basis" ="#cec4cc",
           "3_A_1" = "#9e8a9a",
           "3_A_2" = "#6e4f68",
           "3_B_1" = "#54364f", 
           "3_C_1" = "#382434")

data_analysis_long %>% filter(name == "time_before_s" | name == "time_after_s") %>%
  ggplot(aes(x=value/3600, color = alternative, fill = alternative)) + geom_density(alpha = 0.1, size = 2) + theme_bw() + 
  xlab("Reisezeit (h)") + ylab("Häufigkeit") + 
  scale_color_manual(values = colors, name = "Szenario") + 
  scale_fill_manual(values = colors, name = "Szenario")

ggsave(filename = "tmp/travel_time_scen_3_distribution.jpg", device = "jpeg",
       width = 15, height= 10, units = "cm", scale = 1.5)

data_analysis_long %>% filter(name == "time_before_s" | name == "time_after_s") %>%
  ggplot(aes(x = alternative, y = value/3600, fill = alternative)) +
  geom_bar(stat = "summary", fun = "mean") +
  theme_bw() + 
  xlab("Szenario") + ylab("Reisezeit (Durchschnitt) (h)") + 
  scale_fill_manual(values = colors, name = "Szenario")

ggsave(filename = "tmp/travel_time_scen_3_average.jpg", device = "jpeg",
       width = 15, height= 10, units = "cm", scale = 1.5)
  







data_analysis_long %>% filter(name == "in_veh_before_s" | name == "in_veh_after") %>%
  ggplot(aes(x=value/3600, color = name)) + geom_density(alpha = 0.1)
