pacman::p_load(data.table, dplyr, ggplot2, tidyr, readr, plotly, sf, tmap, rgeos, RColorBrewer)

out_folder= "Z:/projects/2019/BASt/data/results/mito_ld/"

scenarios = c("1_A_2_v2",
              "1_C_2_v2")


shuttle_trips = data.frame()

for (scenario in scenarios){
  this_trips = read_csv(paste(out_folder,scenario, "/shuttle_trips.csv", sep  =""))
  this_trips$scenario = scenario
  shuttle_trips = shuttle_trips %>% bind_rows(this_trips)
  print(scenario)
}

shuttle_trips = shuttle_trips %>% mutate(dx = abs(orig_x - dest_x),
                                         dy = abs(orig_y - dest_y),
                                         beeline_d = sqrt(dx^2 + dy^2) /1000)



shuttle_trips %>% ggplot(aes(x = beeline_d,..density.., color = scenario)) + geom_freqpoly()


shuttle_trips %>% group_by(scenario) %>% summarize(legs = n(), mean_d = mean(beeline_d), sum_d = sum(beeline_d))

trip_origins = st_as_sf(shuttle_trips %>% sample_frac(0.1), coords = c("orig_x", "orig_y"), crs = 31468)

p =  tm_basemap(leaflet::providers$CartoDB)

p = p + tm_shape(trip_origins, "Origin") + 
  tm_dots(col = "orig_act")

tmap_leaflet(p)
