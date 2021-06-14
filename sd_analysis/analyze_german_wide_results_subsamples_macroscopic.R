pacman::p_load(dplyr, readr, ggplot2, tidyr)

folder = "c:/models/mito/germany/scenOutput/"
file_name = "/2011/resultFile.csv"

trips = data.frame()

for (i in 0:9){
  path = paste(folder, "sd_10_percent_",i,file_name, sep = "")
  this_trips = read_csv(path) %>% mutate(subsample = i)
  trips = trips %>% bind_rows(this_trips)
}


distance_distribution = trips %>%
  filter(grepl("Distance", Attribute)) %>%
  separate(col = Attribute, sep = "_", into = c("attribute", "distance")) %>% 
  pivot_longer(cols = c(HBW, HBE, HBR, HBO, HBS, NHBO, AIRPORT, NHBW), names_to = "purpose", values_to = "n") %>% 
  mutate(n = as.numeric(n))

distance_distribution %>% filter(!is.na(n)) %>% 
  ggplot(aes(x = as.numeric(distance), y = n, color = as.factor(subsample))) +
  geom_line() + 
  facet_wrap(.~purpose) + 
  scale_x_log10()

mode_shares = trips %>%
  filter(grepl("ModeShare", Attribute)) %>%
  separate(col = Attribute, sep = "_", into = c("attribute", "mode")) %>%
  pivot_longer(cols = c(HBW, HBE, HBR, HBO, HBS, NHBO, AIRPORT, NHBW), names_to = "purpose", values_to = "n") %>% 
  mutate(n = as.numeric(n))


mode_colors = c("autoDriver" = "#878787",
                "autoPassenger" = "#a9a9a9",
                "train" = "#789ec9",
                "tramOrMetro" = "#5c768d",
                "bus" = "#50546d",
                "bicycle" = "#87c77d",
                "walk" = "#53815b")

mode_order = c("autoDriver","autoPassenger","train" ,"tramOrMetro" ,"bus","bicycle" ,"walk")

mode_shares$mode = factor(mode_shares$mode, mode_order)

mode_shares %>% ggplot(aes(x = subsample, y = n, fill = mode)) + 
  geom_bar(stat = "identity") + 
  scale_fill_manual(values = mode_colors) + 
  facet_wrap(.~purpose)
