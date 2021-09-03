pacman::p_load(readr, dplyr, tidyr, ggplot2)

scenarios = c("base_2030",
              "1_A_2",
              "1_C_2", 
              "2_A_3", 
              "3_A_1", 
              "3_C_1", 
             # "4_A_1_2030_congested",
              "4_B_1_2030_congested",
              "5_1",
              "6_2030_congested")

scenario_names = c("Basis",
                   "1_A_2",
                   "1_C_2", 
                   "2_A_3", 
                   "3_A_1", 
                   "3_C_1", 
                   #"4_A_1",
                   "4_B_1",
                   "5", 
                   "6")

#scenarios = c("base_2030","4_A_1_2011_freeflow_calibrated", "4_B_1_2011_freeflow_calibrated")
folder = "Z:/projects/2019/BASt/data/results/matsim/"

#scenarios = c("base_2030")

links = data.frame()

for (i in 1:length(scenarios)) {
  this_scenario = scenarios[i]
  this_scenario_name = scenario_names[i]
  path = paste(folder, this_scenario, "/ITERS/it.50/", this_scenario, ".50.linkstats.txt.gz", sep  ="")
  my_links = read_delim(path, delim = "\t") %>% mutate(scenario = this_scenario_name)
  links = links %>% bind_rows(my_links)
  print(this_scenario)  
}


link_types <- read_csv("transport_indicators/link_types.csv", col_names = c("LINK", "type"))

links %>% sample_frac(0.05) %>% 
  select(LENGTH, CAPACITY, volume = `HRS18-19avg`, scenario) %>%
  mutate(v_c = volume * 100/CAPACITY) %>% 
  ggplot(aes(x = v_c, group = scenario, color = scenario)) + stat_ecdf() + xlim(0,1)


links = links %>% left_join(link_types)


links %>% sample_frac(0.05) %>% 
  select(LENGTH, CAPACITY, volume = `HRS18-19avg`, scenario, type) %>%
  mutate(v_c = volume * 100/CAPACITY) %>%
  ggplot(aes(x = v_c, group = scenario, color = scenario)) + stat_ecdf() + xlim(0,1) + 
  facet_grid(.~type)


links %>% 
  select(LENGTH, CAPACITY, volume = `HRS18-19avg`, scenario, type) %>%
  mutate(v_c = volume * 100/CAPACITY) %>% 
  mutate(is_over_50 = if_else (v_c > 0.5, 1, 0)) %>% 
  mutate(is_over_75 = if_else (v_c > 0.75, 1, 0)) %>% 
  mutate(is_over_90 = if_else (v_c > 0.90, 1, 0)) %>% 
  group_by(scenario, type) %>% 
  summarize(l = sum(LENGTH), 
            s_over_50 = sum(LENGTH * is_over_50)/ l * 100, 
            s_over_75 = sum(LENGTH * is_over_75)/ l * 100, 
            s_over_90 = sum(LENGTH * is_over_90)/ l * 100) %>% 
  write.table("clipboard", row.names = F, sep  ="\t")


links %>% 
  select(LENGTH, CAPACITY, volume = `HRS18-19avg`, scenario, type) %>%
  mutate(v_c = volume * 100/CAPACITY) %>% 
  mutate(is_over_50 = if_else (v_c > 0.5, 1, 0)) %>% 
  mutate(is_over_75 = if_else (v_c > 0.75, 1, 0)) %>% 
  mutate(is_over_90 = if_else (v_c > 0.90, 1, 0)) %>% 
  group_by(scenario, type) %>% 
  summarize(l = sum(LENGTH), 
            s_over_50 = sum(LENGTH * is_over_50)/ l * 100, 
            s_over_75 = sum(LENGTH * is_over_75)/ l * 100, 
            s_over_90 = sum(LENGTH * is_over_90)/ l * 100) %>%
  mutate(scenario = factor(scenario, levels = scenario_names)) %>% 
  mutate(type = recode(type, 
                       "autobahn" = "Autobahn", 
                       "bundestrasse" = "Bundesstraße",
                       "other" = "Sonstige")) %>% 
  ggplot(aes(x = scenario, y = s_over_50, color = type, group = type)) + 
  geom_line(size = 2) + geom_point(size = 3) + 
  xlab("Szenario") + 
  ylab("Anteil mit Auslastungsgrad über 50% (%)") + 
  theme_bw() + 
  scale_color_manual(values = c("Autobahn" = "blue",
                                "Bundesstraße" = "orange", 
                                "Sonstige" = "gray80"), 
                     name = "Straßentyp")


ggsave(filename = "tmp/vc_over_50.jpg", device = "jpeg", width = 15, height = 10, units = "cm", scale = 2)




links %>% 
  select(LENGTH, CAPACITY, volume = `HRS18-19avg`, scenario, type) %>%
  mutate(v_c = volume * 100/CAPACITY) %>% 
  mutate(is_over_50 = if_else (v_c > 0.5, 1, 0)) %>% 
  mutate(is_over_75 = if_else (v_c > 0.75, 1, 0)) %>% 
  mutate(is_over_90 = if_else (v_c > 0.90, 1, 0)) %>% 
  group_by(scenario, type) %>% 
  summarize(l = sum(LENGTH), 
            s_over_50 = sum(LENGTH * is_over_50)/ l * 100, 
            s_over_75 = sum(LENGTH * is_over_75)/ l * 100, 
            s_over_90 = sum(LENGTH * is_over_90)/ l * 100) %>%
  mutate(scenario = factor(scenario, levels = scenario_names)) %>%
  mutate(type = recode(type, 
                       "autobahn" = "Autobahn", 
                       "bundestrasse" = "Bundesstraße",
                       "other" = "Sonstige")) %>%
  ggplot(aes(x = scenario, y = s_over_75, color = type, group = type)) + 
  geom_line(size = 2) + geom_point(size = 3) + 
  xlab("Szenario") + 
  ylab("Anteil mit Auslastungsgrad über 75% (%)") + 
  theme_bw() + 
  scale_color_manual(values = c("Autobahn" = "blue",
                                "Bundesstraße" = "orange", 
                                "Sonstige" = "gray80"), 
                     name = "Straßentyp")



ggsave(filename = "tmp/vc_over_75.jpg", device = "jpeg", width = 15, height = 10, units = "cm", scale = 2)



links %>% 
  select(LENGTH, CAPACITY, volume = `HRS18-19avg`, scenario, type) %>%
  mutate(v_c = volume * 100/CAPACITY) %>% 
  mutate(is_over_50 = if_else (v_c > 0.5, 1, 0)) %>% 
  mutate(is_over_75 = if_else (v_c > 0.75, 1, 0)) %>% 
  mutate(is_over_90 = if_else (v_c > 0.90, 1, 0)) %>% 
  group_by(scenario, type) %>% 
  summarize(l = sum(LENGTH), 
            s_over_50 = sum(LENGTH * is_over_50)/ l * 100, 
            s_over_75 = sum(LENGTH * is_over_75)/ l * 100, 
            s_over_90 = sum(LENGTH * is_over_90)/ l * 100) %>%
  mutate(scenario = factor(scenario, levels = scenario_names)) %>%
  mutate(type = recode(type, 
                       "autobahn" = "Autobahn", 
                       "bundestrasse" = "Bundesstraße",
                       "other" = "Sonstige")) %>%
  ggplot(aes(x = scenario, y = s_over_90, color = type, group = type)) + 
  geom_line(size = 2) + geom_point(size = 3) + 
  xlab("Szenario") + 
  ylab("Anteil mit Auslastungsgrad über 90% (%)") + 
  theme_bw() + 
  scale_color_manual(values = c("Autobahn" = "blue",
                                "Bundesstraße" = "orange", 
                                "Sonstige" = "gray80"), 
                     name = "Straßentyp")



ggsave(filename = "tmp/vc_over_90.jpg", device = "jpeg", width = 15, height = 10, units = "cm", scale = 2)

