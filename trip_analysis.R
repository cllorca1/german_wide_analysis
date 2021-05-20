pacman::p_load(data.table, dplyr, ggplot2, tidyr, readr, plotly, sf, tmap, rgeos, RColorBrewer)

upper_folder= "c:/models/germanymodel/"

scens = c("walk_to_rail", "auto_to_rail")

all_trips = data.frame()

max_seed = 1

for (scen in scens){
  out_folder = paste(upper_folder, "output/",scen , "/", sep = "")
  filename = "trips.csv"
  seeds = 1:max_seed
  trips = data.frame()
  for (seed in seeds){
    this_trips = read_csv(paste(out_folder,filename, sep  =""))
    this_trips$seed = seed
    trips = trips %>% bind_rows(this_trips)
  }
  all_trips = all_trips %>% bind_rows(trips %>% mutate(scen = scen))
}


all_trips$dist_bin = cut(all_trips$travelDistanceByCar_km, breaks = seq(0,1000, 50))


summary_trips = all_trips %>% group_by(dist_bin,tripMode, scen) %>%
  summarise(n = n()/max_seed,
            utility_auto = mean(as.numeric(utility_auto), na.rm = T),
            utility_air =  mean(as.numeric(utility_air), na.rm = T),
            utility_rail = mean(as.numeric(utility_rail), na.rm = T),
            utility_bus = mean(as.numeric(utility_bus), na.rm = T))


ggplot(summary_trips, aes(x = as.numeric(dist_bin)*50, y = n, fill = tripMode)) +
  geom_bar(stat = "identity", position = "fill") + facet_grid(.~scen)

ggplot(summary_trips, aes(x = dist_bin, y = n, fill = tripMode)) + geom_bar(stat = "identity", position = "stack") + 
  facet_grid(.~scen)

summary_trips_2 = all_trips %>% mutate(is_na = if_else(is.na(as.numeric(cost_rail)), 1,0)) %>% group_by(dist_bin, scen) %>%
  summarise(n = n()/8,
            impedance_auto = mean(as.numeric(impedance_auto), na.rm = T),
            impedance_air =  mean(as.numeric(impedance_air), na.rm = T),
            impedance_rail = mean(as.numeric(impedance_rail), na.rm = T),
            impedance_bus = mean(as.numeric(impedance_bus), na.rm = T), is_na= sum(is_na))


ggplot(summary_trips_2, aes(x = dist_bin, y = impedance_rail, color = scen, group = scen)) +
  geom_line(stat = "identity")

ggplot(summary_trips_2, aes(x = dist_bin, y = is_na, color = scen, group = scen)) +
  geom_line(stat = "identity")


ggplot(summary_trips_2, aes(x = dist_bin, y = impedance_bus, color = scen, group = scen)) +
  geom_line(stat = "identity")

ggplot(summary_trips_2, aes(x = dist_bin, y = impedance_auto, color = scen, group = scen)) +
  geom_line(stat = "identity")

summary_trips_0 = all_trips %>% group_by(tripMode, scen) %>%
  summarise(n = n()/max_seed)
#write.table(summary_trips_0, "clipboard", sep = "\t", row.names = F)


ggplot(summary_trips_0, aes(x = tripMode, group = scen, y = n*5, fill = scen)) + geom_bar(stat = "identity", position = "dodge")

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



