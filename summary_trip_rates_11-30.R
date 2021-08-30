pacman::p_load(readr, dplyr, tidyr, ggplot2)


population_germany_1_percent = 807834
#population_munich_100_percent = 4424800

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
  group_by(purpose) %>%
  summarize(n = n(), d = sum(distance)) %>%
  mutate(year = 2011, segment = "sd", type = "sd")
s2 = sd_trips_2030 %>%
  group_by(purpose) %>%
  summarize(n = n(), d = sum(distance)) %>%
  mutate(year = 2030, segment = "sd", type = "sd")

s3 = ld_trips %>%
  filter(international == F) %>% 
  group_by(purpose = tripPurpose, type = tripState ) %>%
  summarize(n = n(), d = sum(travelDistanceByCar_km)) %>%
  mutate(year = 2011, segment = "ld")

s4 = ld_trips_2030 %>%
  filter(international == F) %>% 
  group_by(purpose = tripPurpose, type = tripState ) %>%
  summarize(n = n(), d = sum(travelDistanceByCar_km)) %>%
  mutate(year = 2030, segment = "ld")


s = s1 %>% bind_rows(s2) %>% bind_rows(s3) %>% bind_rows(s4)

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




s %>% ggplot(aes(x = as.factor(year), y = n * leg_factor*100/1e6, fill = purpose, group = purpose)) +
  geom_bar(stat = "identity", position = "stack") +
  scale_fill_manual(values = purpose_colors, name = "Purpose") +
  theme_bw() +
  xlab("Year") + ylab("Legs (Million)") + 
  theme(legend.position = "bottom")


s %>%
  filter(segment == "ld", type != "away") %>% 
  ggplot(aes(x = as.factor(year), y = n * leg_factor*100/1e6, fill = purpose)) +
  geom_bar(stat = "identity", position = "stack") +
  facet_wrap(purpose~type, scales = "free", ncol = 2) +
  theme_bw() + 
  scale_fill_manual(values = purpose_colors, name = "Purpose") +
  xlab("Year") + ylab("Legs (Million)")  + 
  theme(legend.position = "bottom")


s %>%
  filter(segment == "sd") %>% 
  ggplot(aes(x = as.factor(year), y = n * leg_factor*100/1e6, fill = purpose)) +
  geom_bar(stat = "identity", position = "stack") +
  facet_wrap(purpose~., scales = "free", ncol = 4) +
  theme_bw() + 
  scale_fill_manual(values = purpose_colors, name = "Purpose") +
  xlab("Year") + ylab("Legs (Million)")  + 
  theme(legend.position = "bottom")



sd_coefficients = read_csv("C:/models/mito/germany/input/tripGeneration/CoefficientsNegBinCountPersonBased.csv")
sd_coefficients_zero = read_csv("C:/models/mito/germany/input/tripGeneration/CoefficientsZeroHurdlePersonBased.csv")
ld_coefficients = read_csv("C:/models/germanymodel/input/tripGeneration/domesticTripGenerationCoefficients_weekday.csv")


sd_coefficients %>%
  pivot_longer(cols = c(HBW, HBE, HBO, HBS, HBR, NHBO, NHBW)) %>% 
  filter(variable != "theta", variable != "(Intercept)") %>% 
  ggplot(aes(x = variable, y  = value, fill = name)) + geom_bar(stat = "identity") + coord_flip() + 
  theme_bw() + facet_grid(.~name)

sd_coefficients_zero %>%
  pivot_longer(cols = c(HBW, HBE, HBO, HBS, HBR, NHBO, NHBW)) %>% 
  filter(variable != "theta", variable != "(Intercept)") %>% 
  ggplot(aes(x = variable, y  = value, fill = name)) + geom_bar(stat = "identity") + coord_flip() + 
  theme_bw() + facet_grid(.~name)


ld_coefficients %>%
  pivot_longer(cols = c(daytrip.business, daytrip.leisure, daytrip.private,
                        overnight.business, overnight.leisure, overnight.private, 
                        away.business, away.private, away.leisure)) %>% 
  separate(col = name, into = c("type", "purpose"), remove = F) %>% 
  filter(factorName != "oldK", factorName != "k_calibration", factorName != "newK", factorName != "(intercept)") %>% 
  ggplot(aes(x = factorName, y  = value, fill = purpose)) + geom_bar(stat = "identity") + coord_flip() + 
  theme_bw() + facet_grid(type~purpose) + geom_hline(yintercept = 0, color = "black") + 
  xlab("Coefficient") + ylab("Variable") + 
  scale_fill_manual(values = purpose_colors, name = "Purpose")


