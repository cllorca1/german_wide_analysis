pacman::p_load(readr, dplyr, tidyr, ggplot2)


counts_folder = "C:/Users/carlloga/LRZ Sync+Share/bast_EntlastungBundesfernstraßen (Rolf Moeckel)/detectors/counts_bast_2011/"

counts_observed = read_csv(paste(counts_folder, "weekday_counts_by_station_hour.csv", sep  =""))


counts_observed_1_car = counts_observed %>% select(station = station_zst, 
                                                   hour,
                                                   pkw_1,
                                                   pkw_2,
                                                   road_type,
                                                   longitude, 
                                                   latitude,
                                                   osm_1 = osm_link_1,
                                                   osm_2 = osm_link_2) %>%
  mutate(veh_type = "car", vehicles = pkw_1 + pkw_2)

counts_observed_1_truck = counts_observed %>% select(station = station_zst, 
                                                     hour,
                                                     lkw_1,
                                                     lkw_2,
                                                     road_type,
                                                     longitude, 
                                                     latitude,
                                                     osm_1 = osm_link_1,
                                                     osm_2 = osm_link_2) %>%
  mutate(veh_type = "truck", vehicles = lkw_1 + lkw_2)



counts_observed_long = counts_observed_1_car %>%
  bind_rows(counts_observed_1_truck)

rm(counts_observed, counts_observed_1_car, counts_observed_1_truck)

length(unique(counts_observed_long$station))

#write_csv(counts_observed_long, paste(counts_folder, "weekday_counts_by_station_hour_long.csv", sep  =""))
names(counts_observed_long)

model_folder = "Z:/projects/2019/BASt/data/results/matsim/"

scenario = "base_2011"
veh_types = c("car_sd", "car_ld","truck")

#scenarios = c("combined_hermes_20210506","ld_trucks")
#veh_types = c("car","truck")


simulated_counts = read_csv(paste(model_folder, scenario, "/counts.csv", sep  =""))
simulated_counts = simulated_counts %>% filter(driver_type == "accept_toll")
simulated_counts = simulated_counts %>% select(link, hour, type, veh_type = vehicle_type, vehicles = count)
simulated_counts = simulated_counts %>%  mutate(is_truck = if_else(veh_type == "truck", 1, 0))
  
#add every count into one day (hours will be repeated!)
simulated_counts = simulated_counts %>%
  mutate(hour = if_else(hour >= 48, hour - 48, if_else(hour >= 24, hour - 24, hour)))

length(unique(simulated_counts$link))

###### quick fix to error in scale factor (only for 24.06.2021)

#simulated_counts$vehicles[simulated_counts$veh_type == "car_sd"] = simulated_counts$vehicles * 5



##stations and links 
model_folder = "C:/models/germanymodel/matsim/"
station_links_2011 = read_csv(paste(model_folder, "bast_station_links_network_2011_2.csv", sep =""))

length(unique(station_links_2011$stationId))
length(unique(station_links_2011$linkId))


simulated_counts = simulated_counts %>% left_join(station_links_2011, by = c("link" = "linkId"))


