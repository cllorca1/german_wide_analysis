pacman::p_load(readr, dplyr, tidyr, ggplot2)

path_s3  = "C:/models/transit_germany/output/skims/ld_train_with_walk_2/deutschland_takt_summary_low.csv"
matrixdata_3 = read_csv(path_s3)

path_s5  = "c:/projects/bast_entlastung/analysis/ld_model_analysis/scenarios/deutschland_takt_summary_low_with_auto.csv"

matrixdata_5 = read_csv(path_s5)


fields_before = c("origin", "destination", "access_s", "egress_s", 
                  "distance_m", "transfers", "waiting_before_s", "in_veh_before_s", "time_before_s", 
                  "share_time_before")
fields_after = c("origin", "destination", "access_s", "egress_s", 
                 "distance_m", "transfers", "waiting_before_s", "in_veh_after", "time_after_s", 
                 "share_time_after")

matrixdata_long= matrixdata_3 %>% pivot_longer(cols = 3:13) %>% mutate(access = "walk") %>% 
  bind_rows(matrixdata_5 %>% pivot_longer(cols = 3:13) %>% mutate(access = "auto"))



summary_total_time = matrixdata_long %>% filter(name %in% c("time_before_s", "time_after_s")) %>%
  filter(!is.infinite(value), value > 0) %>% 
  group_by(access, name) %>% summarise(time = mean(value, na.rm = T)/3600)


summary_total_time = summary_total_time %>% mutate(scenario = if_else(name == "time_before_s", 
                                                 if_else(access == "walk", "Basis", "1_A_2 (mit dem Shuttle zur Bahn)"),
                                                 if_else(access == "walk","3_A_1 (und 5, zu Fuß zur Bahn)", "5 (mit dem Shuttle zur Bahn)")))


summary_total_time =  summary_total_time %>% mutate(scenario = factor(scenario, 
                                                                      levels = c("Basis", 
                                                                                 "1_A_2 (mit dem Shuttle zur Bahn)",
                                                                                 "3_A_1 (und 5, zu Fuß zur Bahn)", 
                                                                                 "5 (mit dem Shuttle zur Bahn)"))) 



summary_total_time =  summary_total_time %>% mutate(access = factor(access, 
                                                                    levels = c("walk", "auto"), 
                                                                    labels = c("zu Fuß", "Shuttle"))) 



summary_total_time %>% 
  ggplot(aes(x = scenario, y = time, fill = access)) + 
  geom_bar(stat = "identity", position = "dodge") + 
  scale_fill_manual(values = c("zu Fuß" = "#764c6e", "Shuttle"= "#c8b7c5"), name = "Zubringerverkehr") + 
  theme_bw() + 
  xlab("Szenario") + 
  ylab("Reisezeit (Durchschnitt) (h)") + 
  theme(axis.text.x = element_text(angle = 90))

ggsave(filename = "tmp/travel_time_for_scenario_5.jpeg", device = "jpeg",
       width = 15, height= 10, units = "cm", scale = 1.5)





