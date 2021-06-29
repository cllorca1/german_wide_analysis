pacman::p_load(dplyr, readr, ggplot2, tidyr)

osm_links = read_csv("F:/germany_wide_network/road_germany_2030.csv")


osm_links = osm_links %>% mutate(type = if_else((highway == "motorway" | highway == "motorway_link"),
                                                "autobahn",
                                                if_else(grepl("B", ref, fixed = T) & 
                                                          (highway == "trunk" | highway == "trunk_link"|
                                                            highway == "primary" | highway == "primary_link"|
                                                             highway == "secondary" | highway == "secondary_link"),
                                                         "bundestrasse", "other"))) 

                     

summary = osm_links %>%
  group_by(type, highway) %>%
  summarize(n(), speed = mean(as.numeric(maxspeed), na.rm = T), lanes = mean(lanes, na.rm  =T))


osm_links %>% filter(type != "other") %>% write_csv("F:/germany_wide_network/road_germany_2030_ab.csv")
