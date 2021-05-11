pacman::p_load(data.table, dplyr, ggplot2, tidyr, readr, plotly, sf, tmap, rgeos, RColorBrewer)

upper_folder= "c:/models/germanymodel/"

out_folder = paste(upper_folder, "output/5_percent/", sep = "")

scenarios = read_csv(paste(upper_folder, "input/scenarios_paper.csv", sep = ""))

trips = data.frame()

for (i in 1:nrow(scenarios)){
  scenario_folder = scenarios$scenario[i]
  this_trips = read_csv(paste(out_folder,scenario_folder, "_trips.csv", sep  =""))
  this_trips$seed = scenarios$seed[i]
  this_trips$cost = scenarios$cost[i]
  this_trips$distance = scenarios$distance[i]
  this_trips$limSpeed = scenarios$limSpeed[i]
  this_trips$scenario_name = scenarios$scenarioNoSeed[i]
  trips = trips %>% bind_rows(this_trips)
}


summary_distance = trips %>% filter(cost == 1, tripState!= "away", limSpeed == 0) %>%
  group_by(distance, tripMode, scenario_name) %>%
  summarize(count = n() / 8 * 20, co2 = sum(CO2emissions_kg)/8*20,
            ave_dist = mean(travelDistanceByCar_km), ave_dist_mode = mean(travelDistance_km))

ggplot(summary_distance, aes(x = distance, y = count)) + geom_line() + geom_point() + 
  facet_wrap(.~tripMode, scales = "free")

ggplot(summary_distance, aes(x = distance, y = co2)) + geom_line() + geom_point() + 
  facet_wrap(.~tripMode, scales = "free")

ggplot(summary_distance, aes(x = distance, y = ave_dist, color = tripMode)) +
  geom_line() + geom_point()

ggplot(summary_distance, aes(x = distance, y = ave_dist_mode, color = tripMode)) +
  geom_line() + geom_point()



ggplot(summary_distance, aes(x = as.factor(distance), y = count, fill = tripMode)) +
  geom_bar(stat = "identity", position = "fill")

ggplot(summary_distance, aes(x = as.factor(distance), y = co2, fill = tripMode)) +
  geom_bar(stat = "identity", position = "stack")


summary_cost = trips %>% filter(distance == 0, tripState!= "away", limSpeed == 0) %>% 
  group_by(cost, tripMode, scenario_name) %>%
  summarize(count = n() / 8 * 20, co2 = sum(CO2emissions_kg)/8*20)

ggplot(summary_cost, aes(x = cost, y = count)) + geom_line() + geom_point() + 
  facet_wrap(.~tripMode, scales = "free")

ggplot(summary_cost, aes(x = cost, y = co2)) + geom_line() + geom_point() + 
  facet_wrap(.~tripMode, scales = "free")


ggplot(summary_cost, aes(x = as.factor(cost), y = count, fill = tripMode)) +
  geom_bar(stat = "identity", position = "fill")

ggplot(summary_cost, aes(x = as.factor(cost), y = co2, fill = tripMode)) +
  geom_bar(stat = "identity", position = "stack")



airTrips = trips %>% filter(tripMode == "air", tripState!= "away", limSpeed == 0, cost == 1)

ggplot(airTrips, aes(x=travelDistanceByCar_km)) +
  geom_density() +
  facet_grid(.~scenario_name)

ggplot(airTrips, aes(x=travelDistance_km)) + stat_ecdf() + facet_grid(.~scenario_name) +
  xlim(0,2000)

ggplot(airTrips, aes(x=travelDistanceByCar_km)) + stat_ecdf() + facet_grid(.~scenario_name) +
  xlim(0,2000)

