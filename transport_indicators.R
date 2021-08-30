pacman::p_load(readr, dplyr, tidyr, ggplot2)

scenarios = c("base_2030","1_A_2","1_C_2", "2_A_3", "3_A_1", "3_C_1")

#scenarios = c("base_2030","4_A_1_2011_freeflow_calibrated", "4_B_1_2011_freeflow_calibrated")
folder = "Z:/projects/2019/BASt/data/results/matsim/"



indicators = data.frame()

for (this_scenario in scenarios){
  path = paste(folder, this_scenario, "/transport_indicators.csv", sep  ="")
  my_indicators = read_csv(path) %>% mutate(scen = this_scenario)
  indicators = indicators %>% bind_rows(my_indicators)
  
}

#re-order the factors
indicators = indicators %>% mutate(scen = factor(x = scen, 
                                                 levels = scenarios))
indicators = indicators %>% mutate(osm_type = factor(x = osm_type, 
                                                 levels = c("autobahn", "bundestrasse", "other"),
                                                 labels = c("Autobahn", "Bundestraße", "sonstige")))

indicators = indicators %>% mutate(vehicle_type = factor(x = vehicle_type,
                                                                 levels = c("all", "car_ld", "car_sd", "truck"),
                                                         labels = c("Alle", "Pkw-Fernverkehr",
                                                                    "Pkw-Nahverkehr", "Lkw")))




##add manually another scenario?
# this_scenario = "base_210712"
# path = paste("F:/matsim_germany/output/", this_scenario, "/transport_indicators.csv", sep  ="")
# my_indicators = read_csv(path) %>% mutate(scen = this_scenario)
# indicators = indicators %>% bind_rows(my_indicators)

vehicle_colors = c("Pkw" = "#838383",
                   "Pkw-Fernverkehr" = "#414141",
                   "Pkw-Nahverkehr" = "#c1c1c1" ,
                   "Lkw" = "#70ab91")

#osm_types = c("motorway", "trunk", "primary", "secondary", "tertiary")

indicators %>%
  filter(indicator == "vkt") %>%
  ggplot(aes(x = scen, y = value/1e3/1e6, group  = vehicle_type, fill = vehicle_type)) + 
  geom_bar(stat = "identity", position = "stack") + 
  ylab("Tagesfahrleistung (10e6 km Fz/Tag)") + xlab("Szenario") + 
  theme_bw() + 
  scale_fill_manual(values = vehicle_colors, name = "Segment der Nachfrage") + 
  facet_wrap(.~osm_type) + 
  theme(axis.text.x = element_text(angle = 90)) 

indicators %>%
  filter(indicator == "vkt") %>% filter(vehicle_type == "Pkw-Fernverkehr") %>% 
  ggplot(aes(x = scen, y = value/1e3,  group  = vehicle_type, fill = vehicle_type)) + 
  geom_bar(stat = "identity", position = "stack") + 
  ylab("Tagesfahrleistung (10e6 Fz/Tag)") + xlab("Szenario") + 
  theme_bw() + 
  scale_fill_manual(values = vehicle_colors, name = "Segment der Nachfrage") + 
  facet_grid(.~osm_type, scales = "free") + 
  theme(axis.text.x = element_text(angle = 90))


indicators %>%
  filter(indicator %in% c("delay", "vehicles")) %>%
  pivot_wider(names_from = indicator, values_from = value) %>% 
  ggplot(aes(x = scen, y = delay/vehicles/60, fill = vehicle_type)) + 
  geom_bar(stat = "identity", position = "dodge", color = "white") + 
  scale_fill_manual(values = vehicle_colors, name = "Segment der Nachfrage") +
  scale_alpha_continuous(name = "Seed", range = c(0.5, 0.8)) + 
  ylab("Average delay (min)") +
  theme_bw() + 
  facet_grid(osm_type~vehicle_type, scales = "free") + 
  theme(axis.text.x = element_text(angle = 90))



indicators %>%
  filter(indicator == "vkt") %>% write.table("clipboard", row.names = F, sep = "\t")

