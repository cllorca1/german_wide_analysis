pacman::p_load(readr, dplyr, tidyr, ggplot2, shiny)

folder_10 = "Z:/projects/2018/radLast/Verflechtungsprognose2030/PVMatrix_BVWP15_A2010/"

matrix_10 = read_csv2(paste(folder_10, "PVMatrix_BVWP15_A2010_english.csv", sep  =""))

folder_30 = "Z:/projects/2018/radLast/Verflechtungsprognose2030/PVMatrix_BVWP15_P2030/"

matrix_30 = read_csv2(paste(folder_30, "PVMatrix_BVWP15_P2030_english.csv", sep  =""))

zones =  read_csv(paste(folder_10, "zones.csv", sep  =""), locale = locale(encoding = "latin1"))


colors_ld_modes  =c("MIV" = "#aaaaaa", "Bahn"  ="#764c6e", 
                    "OESPV" = "#50546d", "Luft" = "#83adb5", "Rad" = "#87c77d", "Fuss" = "#53815b")
purposes = c("Beruf","Ausbildung", "Einkauf", "Geschäft", "Urlaub", "Privat")



matrix_10_long = matrix_10 %>% pivot_longer(cols = 3:38) %>%
  select(quelle = `# Quelle`, ziel = Ziel, name = name, n = value) %>%
  separate(name, sep ="_", into = c("mode", "purpose"))
matrix_30_long = matrix_30 %>% pivot_longer(cols = 3:38) %>%
  select(quelle = `# Quelle`, ziel = Ziel, name = name, n = value)  %>%
  separate(name, sep ="_", into = c("mode", "purpose"))

##summary by mode

summary_10 = matrix_10_long %>%
  mutate(domestic = if_else(quelle < 17000 & ziel < 17000, "Inland", "International")) %>% #limits the analysis to domestic trips
  group_by(domestic, mode) %>% summarize(trips = sum(n) / 1e6 / 365) %>%
  mutate(year = 2010)
  

summary_30 = matrix_30_long %>%
  mutate(domestic = if_else(quelle < 17000 & ziel < 17000, "Inland", "International")) %>% 
  group_by(domestic, mode) %>% summarize(trips = sum(n) / 1e6 / 365) %>%
  mutate(year = 2030)
 

summary_10 %>% bind_rows(summary_30) %>% 
  ggplot(aes(y = trips, x = mode, group = year, fill = mode, color = mode, alpha = as.factor(year))) + 
  geom_bar(stat = "identity", position = "dodge") +
  scale_alpha_discrete(range = c(0.4, 1), name = "Jahr") + 
  scale_fill_manual(values = colors_ld_modes, name = "Verkehrsmittel") + 
  scale_color_manual(values = colors_ld_modes, name = "Verkehrsmittel") + 
  facet_wrap(.~domestic, scales = "free") + 
  xlab("Verkehrsmittel") + ylab("Anzahl Fahrten pro Tag in Mio.") + 
  theme_bw()

ggsave(filename = "tmp/bvwp_trips_by_mode.jpg", device = "jpeg", width = 15, height = 10, units = "cm", scale = 2)

## summary by purpose

summary_10_p = matrix_10_long %>%
  mutate(domestic = if_else(quelle < 17000 & ziel < 17000, "Inland", "International")) %>% #limits the analysis to domestic trips
  group_by(domestic, purpose) %>% summarize(trips = sum(n) / 1e6 / 365) %>%
  mutate(year = 2010)


summary_30_p = matrix_30_long %>%
  mutate(domestic = if_else(quelle < 17000 & ziel < 17000, "Inland", "International")) %>% 
  group_by(domestic, purpose) %>% summarize(trips = sum(n) / 1e6 / 365) %>%
  mutate(year = 2030)





summary_10_p %>% bind_rows(summary_30_p) %>% 
  mutate(purpose = recode(purpose, 
                          "Fz1" = purposes[1], 
                          "Fz2" = purposes[2],
                          "Fz3" = purposes[3],
                          "Fz4" = purposes[4],
                          "Fz5" = purposes[5],
                          "Fz6" = purposes[6])) %>% 
  ggplot(aes(y = trips, x = purpose, group = year, fill = purpose, color = purpose, alpha = as.factor(year))) + 
  geom_bar(stat = "identity", position = "dodge") +
  scale_alpha_discrete(range = c(0.4, 1), name = "Jahr") + 
  scale_fill_discrete(name = "Fahrtzweck") + 
  scale_color_discrete(name = "Fahrtzweck") + 
  facet_wrap(.~domestic, scales = "free") + 
  xlab("Fahrtzweck") + ylab("Anzahl Fahrten pro Tag in Mio.") + 
  theme_bw()

ggsave(filename = "tmp/bvwp_trips_by_purpose.jpg", device = "jpeg", width = 15, height = 10, units = "cm", scale = 2)


###get simulated data

purpose_order = c("HBW", "HBE", "HBS", "HBR", "HBO", "NHBW", "NHBO")

sd_model_path = "Z:/projects/2019/BASt/data/results/mito_sd/"
sd_trips = read_csv(paste(sd_model_path,
                          "sd_1_percent_2011_20210624/2011/microData/trips.csv", sep = ""))
sd_trips_2030 = 
  read_csv(paste(sd_model_path,
                 "sd_5_percent_2030_20210813/2030/microData/trips.csv", sep = "")) %>% 
  sample_frac(0.2)

ld_model_path = "Z:/projects/2019/BASt/data/results/mito_ld/"
ld_trips = read_csv(paste(ld_model_path, "base_2011_congested_calibrated/0_trips.csv", sep  = "")) %>% sample_frac(0.2)
ld_trips_2030 = read_csv(paste(ld_model_path, "base_2030_congested/0_trips.csv", sep  = "")) %>% sample_frac(0.2)

s1 = sd_trips %>%
  group_by(purpose, mode) %>%
  summarize(n = n(), d = sum(distance)) %>%
  mutate(year = 2011, segment = "sd", type = "sd", domestic = T) 
s2 = sd_trips_2030 %>%
  group_by(purpose, mode) %>%
  summarize(n = n(), d = sum(distance)) %>%
  mutate(year = 2030, segment = "sd", type = "sd", domestic = T)
s3 = ld_trips %>%
  group_by(purpose = tripPurpose, type = tripState, mode =  tripMode, domestic = !international ) %>%
  summarize(n = n(), d = sum(travelDistanceByCar_km)) %>%
  mutate(year = 2011, segment = "ld")
s4 = ld_trips_2030 %>%
  group_by(purpose = tripPurpose, type = tripState, mode = tripMode, domestic = !international ) %>%
  summarize(n = n(), d = sum(travelDistanceByCar_km)) %>%
  mutate(year = 2030, segment = "ld")


s = s1 %>% bind_rows(s2) %>% bind_rows(s3) %>% bind_rows(s4)

rm(s1, s2, s3, s4)

purposes = c("HBE",      "HBO" ,     "HBR"  ,    "HBS",      "HBW"   ,   "NHBO" ,
             "NHBW" ,    "business","leisure" , "private")

two_leg_purposes = c("HBE","HBO" ,"HBR",  "HBS", "HBW")

s = s %>% mutate(leg_factor = 
                   if_else(purpose %in% two_leg_purposes, 2,1)) %>% 
  mutate(leg_factor =  if_else(type == "away", 0, if_else(type == "daytrip", 2, leg_factor)))


purpose_colors = c("HBE" = "#F59F9F", "HBO" = "#D69494",  
                   "HBR" = "#B06363", "HBS" = "#914848",
                   "HBW" = "#8A3737",  "NHBO" = "#701C1C",
                   "NHBW" = "#7D0A0A" , "business" = "#5D7DC2",
                   "leisure" = "#545280" , "private" ="#140B7A") 
s = s %>% mutate(purpose = factor(purpose, levels = purposes))

simulation_summary = s %>% mutate(mode_bvwp = recode(mode,
                                                     "train"  = "Bahn",
                                                     "walk" = "Fuss", 
                                                     "bicycle" = "Rad", 
                                                     "air"  ="Luft", 
                                                     "autoDriver" = "MIV", 
                                                     "autoPassenger" = "MIV", 
                                                     "rail" = "Bahn", 
                                                     "auto" = "MIV", 
                                                     "bus" = "OESPV", 
                                                     "tramOrMetro" = "OESPV"))


t_1 = summary_10 %>% bind_rows(summary_30) %>% mutate(source = "Verkehrsprognose")

t_2 = simulation_summary %>% 
  group_by(year, mode = mode_bvwp, domestic) %>%
  summarise(trips = sum(n * leg_factor)/1e6 * 100) %>%
  mutate(domestic = if_else(domestic, "Inland", "International")) %>% mutate(source = "Simulation") %>% 
  mutate(year = if_else(year == 2011, 2010, year))

t_1 %>% bind_rows(t_2) %>% 
  ggplot(aes(x = mode, y = trips, fill = mode, group = source, alpha = source, color = mode)) + 
  geom_bar(stat = "identity", position = position_dodge(preserve = "single")) + 
  facet_grid(domestic~year, scales = "free") +
  scale_alpha_discrete(range = c(0.2, 1), name = "Quelle") + 
  scale_fill_manual(values = colors_ld_modes, name = "Verkehrsmittel") + 
  scale_color_manual(values = colors_ld_modes, name = "Verkehrsmittel") + 
  xlab("Verkehrsmittel") + ylab("Anzahl Fahrten pro Tag in Mio.") + 
  theme_bw()


ggsave(filename = "tmp/bvwp_vs_model_trips_by_mode.jpg", device = "jpeg", width = 15, height = 10, units = "cm", scale = 2)
