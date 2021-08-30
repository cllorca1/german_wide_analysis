pacman::p_load(readr, dplyr, tidyr, ggplot2)



model_folder = "Z:/projects/2019/BASt/data/results/matsim/"

scenarios = c("base_2030","4_A_1_2011_freeflow_calibrated", "4_B_1_2011_freeflow_calibrated")

simulated_counts = data.frame()
for (i in 1:length(scenarios)){
  scenario = scenarios[i]
  this_simulated_counts = read_csv(paste(model_folder, scenario, "/counts.csv", sep  =""))
  simulated_counts = simulated_counts %>%
    bind_rows(this_simulated_counts %>%
                mutate(scenario = scenario))
            
  rm(this_simulated_counts)
}

#add every count into one day (hours will be repeated!)
simulated_counts = simulated_counts %>%
  mutate(hour = if_else(hour >= 48, hour - 48, if_else(hour >= 24, hour - 24, hour)))

length(unique(simulated_counts$link))


simulated_counts$scenario = factor(simulated_counts$scenario, levels = scenarios)

###### quick fix to error in scale factor (only for 24.06.2021)

#simulated_counts$vehicles[simulated_counts$veh_type == "car_sd"] = simulated_counts$vehicles * 5



##stations and links 
model_folder = "C:/models/germanymodel/matsim/"
station_links_2011 = read_csv(paste(model_folder, "bast_station_links_network_2011_2.csv", sep =""))

length(unique(station_links_2011$stationId))
length(unique(station_links_2011$linkId))


simulated_counts = simulated_counts %>% left_join(station_links_2011, by = c("link" = "linkId"))


