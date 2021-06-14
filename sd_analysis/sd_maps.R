pacman::p_load(data.table, dplyr, ggplot2, tidyr, readr, plotly, sf, tmap, rgeos, RColorBrewer)



trips = read_csv("c:/models/mito/germany/scenOutput/sd_1_percent_20210531/2011/microData/trips.csv")


trips = trips %>% mutate(mode2 = mode) 
trips$mode2 = recode(trips$mode2, "autoDriver" = "auto",
                                             "autoPassenger" = "auto",
                                           "train" = "pt",
                                             "tramOrMetro" = "pt",
                                             "bus" = "pt",
                                             "bicycle" = "active",
                                             "walk" = "active")

trips_by_origin = trips %>%
  group_by(origin) %>%
  mutate(total = n()) %>%
  group_by(origin, mode2, total) %>%
  summarize(n = n()) 

mode_colors = c("autoDriver" = "#878787",
                        "autoPassenger" = "#a9a9a9",
                        "train" = "#789ec9",
                        "tramOrMetro" = "#5c768d",
                        "bus" = "#50546d",
                        "bicycle" = "#87c77d",
                        "walk" = "#53815b")

mode_order = c("autoDriver","autoPassenger","train" ,"tramOrMetro" ,"bus","bicycle" ,"walk")

zone_shp = st_read("C:/models/germanymodel/input/trafficAssignment/zonesShapefile/zonesNew.shp")


zone_shp$region = substr(zone_shp$AGS, 1,4)


p =  tm_basemap(leaflet::providers$CartoDB)

modes = c("auto", "pt", "active")
colors = c("Greys", "Blues", "Greens")
for (i in 1:3){
  selected_mode = modes[i]
  my_palette = colors[i]
  this_trips_by_origin = trips_by_origin %>% filter(mode2 == selected_mode) %>%
  ungroup()
  this_shp = zone_shp %>%
  select(TAZ_id,region) %>%
  left_join(this_trips_by_origin , by = c("TAZ_id" = "origin"))
  this_shp = this_shp %>% group_by(region) %>% summarize(n = sum(n, na.rm = T), total = sum(total, na.rm = T))
  
  #this_shp = st_simplify(this_shp, dTolerance = 1000)
  this_shp$share = this_shp$n / this_shp$total
  
  p = p + tm_shape(shp = this_shp, name = selected_mode) +
  tm_fill(col = "share",alpha = 0.4, border.alpha = 0, convert2density = F, style = "quantile", palette = my_palette)
}

tmap_leaflet(p)



