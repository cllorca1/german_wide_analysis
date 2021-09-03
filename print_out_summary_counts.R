pacman::p_load(readr, dplyr, tidyr, ggplot2)


counts_folder = "C:/Users/carlloga/LRZ Sync+Share/bast_EntlastungBundesfernstraßen (Rolf Moeckel)/detectors/counts_bast_2011/"

counts_observed = read_csv(paste(counts_folder, "weekday_counts_by_station_hour.csv", sep  =""))


counts_observed %>% group_by(station_zst, x = longitude, y = latitude, roadType) %>%
  summarize() %>% write_csv(paste(counts_folder, "stations_summary.csv", sep = ""))



counts_observed %>% filter(!is.na(matsim_link_1) | !is.na(matsim_link_2)) %>% group_by(station_zst, x = longitude, y = latitude, roadType) %>%
  summarize() 
