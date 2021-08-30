pacman::p_load(readr, dplyr, tidyr, ggplot2)


population_germany_1_percent = 807834
#population_munich_100_percent = 4424800

sd_model_path = "c:/models/mito/germany/"
sd_trips = read_csv(paste(sd_model_path,
                          "scenOutput/sd_1_percent_20210624/2011/microData/trips.csv", sep = ""))


ld_model_path = "Z:/projects/2019/BASt/data/input_files_LD/germanymodel/output/carlos_base_scenarios_20210708/"
ld_trips = read_csv(paste(ld_model_path, "0/0_trips.csv", sep  = "")) %>% sample_frac(0.2)




s1 = sd_trips %>%
  group_by(purpose, mode) %>%
  summarize(n = n(), d = sum(distance)) %>%
  mutate(year = 2011, segment = "Short-distance", type = "sd")


s3 = ld_trips %>%
  filter(international == F) %>% 
  mutate(purpose = paste(tripPurpose, tripState, sep = "-")) %>% 
  group_by(purpose, type = tripState, mode = tripMode) %>%
  summarize(n = n(), d = sum(travelDistanceByCar_km)) %>%
  mutate(year = 2011, segment = "Long-distance")



s = s1 %>%  bind_rows(s3)

purposes = c("HBW",      "HBE" ,     "HBS"  ,    "HBR",      "HBO"   ,   "NHBW" ,
             "NHBO" ,    "business-daytrip","leisure-daytrip" , "private-daytrip",
             "business-overnight","leisure-overnight" , "private-overnight")

purpose_labels = c("HBW",      "HBE" ,     "HBS"  ,    "HBR",      "HBO"   ,   "NHBW" ,
             "NHBO" ,    "BD","LD" , "PD",
             "BO","LO" , "PO")





two_leg_purposes = c("HBE","HBO" ,"HBR",  "HBS", "HBW")

s = s %>% mutate(leg_factor = 
                   if_else(purpose %in% two_leg_purposes, 2,1)) %>% 
  mutate(leg_factor =  if_else(type == "away", 0, if_else(type == "daytrip", 2, leg_factor)))

s = s %>% mutate(purpose = factor(purpose, levels = purposes, labels = purpose_labels))


purpose_colors = c("HBW" = "#F59F9F", "HBE" = "#D69494",  
                   "HBS" = "#B06363", "HBR" = "#914848",
                   "HBO" = "#8A3737",  "NHBW" = "#701C1C",
                   "NHBO" = "#7D0A0A" ,
                   "BD"= "#2050BF", 
                   "LD" = "#242087",
                   "PD" = "#192573",
                   "BO" = "#C6C9F2",
                   "LO" = "#9597B8" ,
                   "PO" =  "#9BA6BD") 



modes =  c("autoDriver","autoPassenger", "bicycle" ,"bus" ,"train" ,"tramOrMetro","walk", "auto", "air","rail")  
modes_simple =  c("car","car", "bicycle" ,"bus" ,"train" ,"tramOrMetro","walk", "car", "air","train")

s = s %>% mutate(mode_simple  = factor(mode, levels = modes, labels = modes_simple))

colors_modes  =c("car" = "#aaaaaa", "train"  ="#764c6e", "tramOrMetro" = "#8684b2", "bicycle" = "#87c77d",
                 "walk" = "#53815b",  
                    "bus" = "#bdc9bb", "air" = "#83adb5")


order_modes  =c("air", "car", "train", "tramOrMetro", "bus", "bicycle", "walk")

s = s %>% mutate(mode_simple  = factor(mode_simple, levels = order_modes))

s = s %>% filter(!is.na(purpose))

s %>% ggplot(aes(x = purpose, y = n * leg_factor*100/1e6, fill = purpose, group = purpose)) +
  geom_bar(stat = "identity", position = "stack") +
  scale_fill_manual(values = purpose_colors, name = "Purpose") +
  theme_bw() +
  xlab("Purpose") + ylab("Legs (Million)") + 
  theme(legend.position = "none") + 
  facet_wrap(.~segment, scales = "free") 

s %>% ggplot(aes(x = purpose, y = n * leg_factor*100/1e6, fill = mode_simple, group = mode_simple)) +
  geom_bar(stat = "identity", position = "fill") +
  theme_bw() +
  scale_fill_manual(values = colors_modes, name = "Mode") + 
  xlab("Purpose") + ylab("Share") + 
  theme(legend.position = "bottom") + 
  facet_wrap(.~segment, scales = "free")


s %>% ggplot(aes(x = purpose, y = n * leg_factor*100/1e6, fill = mode_simple, group = mode_simple)) +
  geom_bar(stat = "identity", position = "stack") +
  theme_bw() +
  scale_fill_manual(values = colors_modes, name = "Mode") + 
  xlab("Purpose") + ylab("Legs (Million)") + 
  theme(legend.position = "bottom") + 
  facet_wrap(.~segment, scales = "free")




