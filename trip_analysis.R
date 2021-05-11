pacman::p_load(data.table, dplyr, ggplot2, tidyr, readr, plotly, sf, tmap, rgeos, RColorBrewer)

upper_folder= "c:/models/germanymodel/"

out_folder = paste(upper_folder, "output/5_percent_old/", sep = "")

scenario = "_d0_c1_trips.csv"

seeds = 1:8

trips = data.frame()

for (seed in seeds){
  this_trips = read_csv(paste(out_folder,seed,scenario, sep  =""))
  this_trips$seed = seed
  trips = trips %>% bind_rows(this_trips)
}

trips %>% group_by(tripMode) %>% summarise(n())


#travel distance distribution



#read zonal data

path_to_zone_system_shp = paste(upper_folder, "input/trafficAssignment/zonesShapefile/zonesNew.shp", sep = "")
zones = st_read(path_to_zone_system_shp)
zones = zones %>% select(zone_id = TAZ_id)


#zonal analyses
trips_by_origin_zone = trips %>% group_by(tripOriginZone) %>% summarise(trips_at_orig = n())

zones_data = zones %>% left_join(trips_by_origin_zone, by=c("zone_id" = "tripOriginZone"))



p =  tm_basemap(leaflet::providers$CartoDB)

p = p + tm_shape(zones_data, "origin") +
  tm_polygons(alpha = 0.6, "trips_at_orig", border.alpha = 0.0, convert2density = T, style = "quantile")

tmap_leaflet(p)


##modal share by zone

share_by_orig_zone = trips %>% group_by(tripOriginZone, tripMode) %>%
  summarise(trips = n()) %>% spread(tripMode, trips)

share_by_orig_zone[is.na(share_by_orig_zone)] = 0

share_by_orig_zone  =share_by_orig_zone %>% 
  mutate(share = air / (auto + air + rail + bus))


zones_data = zones %>% left_join(share_by_orig_zone, by=c("zone_id" = "tripOriginZone"))


p =  tm_basemap(leaflet::providers$CartoDB)

p = p + tm_shape(zones_data, "air_trips") +
  tm_polygons(alpha = 0.6, "air", border.alpha = 0.0, palette = "Reds")

tmap_leaflet(p)



