pacman::p_load(readr, dplyr, tidyr, ggplot2)

data_analysis = read_csv("scenarios/results_scenario_1.csv")

data_analysis = data_analysis %>% filter(time_1 < 24 * 3600, time_2 < 24* 3600)
# data_analysis %>% ggplot(aes(x = time_1)) + geom_histogram()
# data_analysis %>% ggplot(aes(x = time_2)) + geom_histogram()


#smaller dataset
# sub_data_analysis = data_analysis[sample(nrow(data_analysis), 10000), ]
# 
# sub_data_analysis %>% 
# ggplot(aes(x=egress_time_1, y = egress_time_2)) +
#   geom_point(alpha = 0.01) + geom_abline(intercept = 0, slope = 1, color = "red") + 
#   xlim(0,30000) + ylim(0,30000)
# 
# 
# sub_data_analysis %>% 
#   ggplot(aes(x=access_time_1, y = access_time_2 )) +
#   geom_point(alpha = 0.01) + geom_abline(intercept = 0, slope = 1, color = "red") + 
#   xlim(0,30000) + ylim(0,30000)


data_analysis_1 = data_analysis %>% select(o,d, t = time_1,
                                           a = access_time_1,  e = egress_time_1,
                                           s = share_1, dist = dist_1) %>%
  mutate(case = "walk_opnv", ad = a*  3 / 3.6, ed = e*  3 / 3.6)
data_analysis_2 = data_analysis %>% select(o,d, t = time_2,
                                           a = access_time_2,
                                           e = egress_time_2,
                                           s = share_2, dist = dist_2) %>%
  mutate(case = "car", ad = a* 30 / 3.6, ed = e*  30 / 3.6)

data_analysis_long = data_analysis_1 %>% bind_rows(data_analysis_2) %>% pivot_longer(cols = c(t, a, e, ad, ed))


# data_analysis_long %>% filter(value < 24*3600) %>%
#   ggplot(aes(x = value/3600, color = case)) + geom_freqpoly() +
#   facet_grid(name~., scales = "free") + theme_bw()

#ggsave(filename = "tmp/all_variables_scenario_1.png", device = png(),
#       width = 15, height= 10, units = "cm", scale = 1.5)

# data_analysis_long %>% filter(name!= "ad", name != "ed") %>%  group_by(o, d, case) %>% summarise(value = sum(value)) %>% 
#   filter(value < 24*3600) %>%
#   ggplot(aes(x = value/3600,..density.., color = case)) + geom_freqpoly(size = 1) +
#   theme_bw() + xlab("Travel time (h)") + ylab("Frequency")

#ggsave(filename = "tmp/travel_time_scenario_1.png", device = png(),
#      width = 15, height= 10, units = "cm", scale = 1.5)


zonal_data = read_csv("c:/models/mito/germany/input/zoneSystemDE_2col.csv")


zonal_data$BBSR_type = recode(as.character(zonal_data$BBSR_type), 
                              "10" = "10: core",
                              "20" = "20: medium_city",
                              "30" = "30:town",
                              "40" = "40: rural")

data_analysis_long_ = data_analysis_long %>% left_join(zonal_data, by = (c("o"="Zone")))
data_analysis_long_ = data_analysis_long_ %>% left_join(zonal_data, by = (c("d"="Zone")),suffix = c("", "_d"))

# data_analysis_long_ %>% filter(name!= "ad", name != "ed", s > 0) %>%
#   group_by(o, d, case, BBSR_type, BBSR_type_d) %>%
#   summarise(value = sum(value)) %>% 
#   filter(value < 24*3600) %>%
#   ggplot(aes(x = value/3600,..density.., color = case)) + geom_freqpoly(size = 1) +
#   theme_bw() + xlab("Travel time (h)") + ylab("Frequency") + 
#   facet_wrap(BBSR_type~BBSR_type_d)

summary = data_analysis_long_ %>% filter() %>%
  group_by(case,name, BBSR_type, BBSR_type_d) %>%
  summarise(value = mean(value)/3600, s = mean(s), dist = mean(dist)) 

summary %>% filter(name== "t" | name == "a"| name == "e") %>%
  filter(!is.na(BBSR_type), !is.na(BBSR_type_d)) %>%
  ggplot(aes(x=case, y = value, fill = name)) + geom_bar(color = "black", stat = "identity", position = "dodge") + 
  facet_grid(BBSR_type~BBSR_type_d)

summary %>% filter(name== "t") %>%
  filter(!is.na(BBSR_type), !is.na(BBSR_type_d)) %>%
  ggplot(aes(x=case, y = value, fill = case)) + geom_bar(color = "black", stat = "identity", position = "dodge") + 
  facet_grid(BBSR_type~BBSR_type_d)


summary %>% filter(name== "ad"| name == "ed") %>%
  filter(!is.na(BBSR_type), !is.na(BBSR_type_d)) %>%
  ggplot(aes(x=case, y = value*3.6, fill = name)) + geom_bar(color = "black", stat = "identity", position = "stack") + 
  facet_grid(BBSR_type~BBSR_type_d)

