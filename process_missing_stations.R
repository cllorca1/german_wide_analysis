#processing of count stations with missing information
pacman::p_load(readr, dplyr, tidyr, ggplot2)

folder = "C:/projects/bast_entlastung/analysis/counts_bast_2011/"

missing_stations = read_csv(folder, "missing_stations_Sasan.csv", sep = "")

missing_stations = missing_stations %>% select(detId, fromNode, toNode) %>% distinct() %>%
  group_by(detId) %>% mutate(n = n(), test = sum(fromNode) - sum(toNode))


missing_stations %>% filter(n == 2) %>% write_csv(paste(folder, "bast_stations_second.csv", sep = ""))



first = read_csv(paste(folder, "bast_station_links_network_2011.csv", sep = ""))
second = read_csv(paste(folder, "bast_stations_second_links.csv", sep = ""))

first %>% bind_rows(second) %>% write_csv(paste(folder, "bast_station_links_network_2011_2.csv", sep = ""))
