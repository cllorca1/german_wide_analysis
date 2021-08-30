pacman::p_load(readr, dplyr, tidyr, ggplot2)


path  = "c:/projects/bast_entlastung/analysis/ld_model_analysis/scenarios/results_scenario_1_v3.csv"

matrixdata = read_csv(path)

matrixdata %>%
  filter(!is.infinite(distance_m)) %>% 
  filter(distance_m > 0) %>% 
  group_by(alt) %>%
  summarize(dist = mean(distance_m)/1000,
    access_dist = mean(access_distance_m)/1000,
    egress_dist = mean(egress_distance_m)/1000, 
    total_dist = dist + access_dist + egress_dist,
    time = mean(travel_time_s)/60)


zonal_data = read_csv("c:/models/mito/germany/input/zoneSystemDE_2col.csv")
zonal_data$BBSR_type = recode(as.character(zonal_data$BBSR_type), 
                              "10" = "10: core",
                              "20" = "20: medium_city",
                              "30" = "30:town",
                              "40" = "40: rural")

matrixdata = matrixdata %>% left_join(zonal_data, by = (c("o"="Zone")))
matrixdata = matrixdata %>% left_join(zonal_data, by = (c("d"="Zone")),suffix = c("", "_d"))



matrixdata %>%
  filter(!is.infinite(distance_m)) %>% 
  filter(distance_m > 0) %>% 
  group_by(alt, BBSR_type) %>%
  summarize(dist = mean(distance_m)/1000,
            access_dist = mean(access_distance_m)/1000,
            egress_dist = mean(egress_distance_m)/1000, 
            total_dist = dist + access_dist + egress_dist,
            time = mean(travel_time_s)/60, 
            access_time = mean(access_time_s)/60)


matrixdata %>% group_by(alt, BBSR_type) %>% 
  filter(alt!= "base") %>%
  ggplot(aes(x = access_distance_m/1000,..density.., color = BBSR_type)) + 
  geom_density(size = 1) + 
  facet_wrap(.~alt)


access_2 = matrixdata %>%
  filter(!is.infinite(distance_m), alt == "2") %>% 
  group_by(o) %>%
  summarize(dist = mean(distance_m)/1000,
            access_dist = mean(access_distance_m)/1000,
            egress_dist = mean(egress_distance_m)/1000, 
            total_dist = dist + access_dist + egress_dist,
            time = mean(travel_time_s)/60, 
            access_time = mean(access_time_s)/60)

write_csv(access_2, "tmp/access_s.csv")


names = c("zu Fuß", "mit dem Auto")


matrixdata %>% filter(alt != "1") %>% 
  mutate(alt = recode(alt, "base" = names[1] , "2" = names[2])) %>% 
  mutate(alt = factor(alt, levels = names)) %>%
  ggplot(aes(x = travel_time_s/3600, color = alt)) + 
  geom_density(size = 2) + 
  scale_color_manual(values = c("zu Fuß" = "#764c6e", "mit dem Auto" = "#c8b7c5"), name = "Zubringerverkehr") + 
  ylab("Häufigkeit") + xlab("Reisezeit (h)") + xlim(0,20) + 
  theme_bw()

ggsave(filename = "tmp/travel_time_scenario_1.jpg", device = "jpeg",
       width = 15, height= 10, units = "cm", scale = 1.5)  

my_summary = matrixdata %>% filter(alt != "1") %>% 
  mutate(alt = recode(alt, "base" = names[1] , "2" = names[2])) %>% 
  mutate(alt = factor(alt, levels = names)) %>%
  group_by(alt, BBSR_type, BBSR_type_d) %>% 
  filter(!is.na(train_distance_share), train_distance_share > 0, !is.infinite(train_distance_share)) %>%
  filter(!is.na(BBSR_type), !is.na(BBSR_type_d)) %>%
  summarize(time = mean(travel_time_s)/3600, 
            distance = mean(distance_m)/1000,
            access_dist = mean(access_distance_m)/1000,
            egress_dist = mean(egress_distance_m)/1000)

types_bbsr_german = c("Kern- und Großst.","Mittelstädte",  "Kleinstädte", "Ländliche Gem.")

my_summary = my_summary %>% 
  mutate(BBSR_type = recode(BBSR_type, "10: core" = types_bbsr_german[1],
                            "20: medium_city" = types_bbsr_german[2], 
                            "30:town" = types_bbsr_german[3], 
                            "40: rural" = types_bbsr_german[4])) %>% 
  mutate(BBSR_type = factor(BBSR_type, levels = types_bbsr_german)) %>% 
  mutate(BBSR_type_d = recode(BBSR_type_d, "10: core" = types_bbsr_german[1],
                            "20: medium_city" = types_bbsr_german[2], 
                            "30:town" = types_bbsr_german[3],  
                            "40: rural" = types_bbsr_german[4])) %>%
  mutate(BBSR_type_d = factor(BBSR_type_d, levels = types_bbsr_german)) 

my_summary %>%
  ggplot(aes(x = alt, y = time, fill = alt)) + 
  geom_bar(stat = "identity", position = "dodge", color = "gray50") +
  facet_grid(BBSR_type~BBSR_type_d) + 
  scale_fill_manual(values = c("zu Fuß" = "#764c6e", "mit dem Auto" = "#c8b7c5"), name = "Zubringerverkehr") + 
  ylab("Durchschnittliche Reisezeit (h)") + xlab("Zubringerverkehrsmittel")  + 
  theme_bw()


ggsave( filename = "tmp/avg_travel_time_scenario_1_area_type.jpg", device = "jpeg",
       width = 15, height= 10, units = "cm", scale = 1.5)

my_summary %>%
  pivot_longer(cols = c(distance, access_dist, egress_dist)) %>%
  mutate(aux_field = if_else(name == "distance", as.character(alt), 
                             if_else(name == "access_dist", 
                                     "Zubringerverkehr (Quelleort)", "Zubringerverkehr (Zielort)"))) %>%
  ggplot(aes(x = alt, y = value, fill = aux_field)) + 
  geom_bar(stat = "identity", position = "stack", color = "gray50") +
  facet_grid(BBSR_type~BBSR_type_d) + 
  scale_fill_manual(values = c("zu Fuß" = "#764c6e", "mit dem Auto" = "#c8b7c5",
                               "Zubringerverkehr (Quelleort)" = "gray60",
                               "Zubringerverkehr (Zielort)" = "gray20"),
                    name = "Zubringerverkehr") + 
  ylab("Durchschnittliche Distanz (km)") + xlab("Zubringerverkehrsmittel")  + 
  theme_bw() + guides(alpha = F)

ggsave(filename = "tmp/avg_travel_distance_scenario_1_area_type.jpeg", device = "jpeg",
       width = 15, height= 10, units = "cm", scale = 1.5)




base_case = matrixdata %>% filter(alt == "base")

base_case %>% sample_frac(0.1) %>%
  ggplot(aes(x = access_time_s, y = access_distance_m)) + 
  geom_point()

base_case %>% 
  filter(!is.infinite(access_distance_m), !is.infinite(access_time_s)) %>%
  lm(formula = access_distance_m ~ access_time_s) %>% 
  summary()



base_case = matrixdata %>% filter(alt == "1")

base_case %>% sample_frac(0.1) %>%
  filter(transfer_count == 0) %>%
  ggplot(aes(x = travel_time_s,
             y = access_time_s + egress_time_s + in_vehicle_time_s,
             color = transfer_count)) + 
  geom_point() + 
  geom_abline(intercept = 0, slope = 1, color = "red")
  

