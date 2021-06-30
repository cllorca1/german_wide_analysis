pacman::p_load(readr, dplyr, tidyr, ggplot2)


population_germany_1_percent = 807834
population_munich_100_percent = 4424800

purpose_order = c("HBW", "HBE", "HBS", "HBR", "HBO", "NHBW", "NHBO")

sd_model_path = "c:/models/mito/germany/"
sd_trips = read_csv(paste(sd_model_path, "scenOutput/sd_1_percent_20210624/2011/microData/trips.csv", sep = ""))
all_sd_germany = sd_trips %>%
  group_by(purpose) %>%
  summarise(rate = n()/population_germany_1_percent, distance = mean(distance)) %>% 
  mutate(area = "germany")
car_sd_germany = sd_trips %>%
  filter(mode == "autoDriver") %>%
  group_by(purpose) %>%
  summarise(rate = n()/population_germany_1_percent, distance = mean(distance)) %>% 
  mutate(area = "germany")

sd_munich_model_path = "c:/models/mito/muc/mitoMunich/"
sd_trips_munich = read_csv(paste(sd_munich_model_path, "scenOutput/test_2/2011/microData/trips.csv", sep = ""))
all_sd_munich = sd_trips_munich %>%
  group_by(purpose) %>%
  summarise(rate = n()/population_munich_100_percent, distance = mean(distance)) %>% 
  mutate(area = "munich")
car_sd_munich = sd_trips_munich %>%
  filter(mode == "autoDriver") %>%
  group_by(purpose) %>%
  summarise(rate = n()/population_munich_100_percent, distance = mean(distance)) %>% 
  mutate(area = "munich")


all_sd_munich %>% bind_rows(all_sd_germany) %>%
  mutate(purpose = factor(purpose, levels = purpose_order)) %>% 
  ggplot(aes(x = purpose, y = rate, fill =area)) + geom_bar(stat = "identity", position = "dodge") + 
  ggtitle("Trips/person by purpose")

car_sd_munich %>% bind_rows(car_sd_germany) %>%
  mutate(purpose = factor(purpose, levels = purpose_order)) %>% 
  ggplot(aes(x = purpose, y = rate, fill =area)) + geom_bar(stat = "identity", position = "dodge") + 
  ggtitle("Trips/person by purpose (car)")

all_sd_munich %>% bind_rows(all_sd_germany) %>%
  mutate(purpose = factor(purpose, levels = purpose_order)) %>% 
  ggplot(aes(x = purpose, y = distance, color =area, group = area )) +
  geom_line(stat = "identity", size = 2) +
  geom_point(size = 4) + 
  ggtitle("Average distance by purpose")




ld_model_path = "d:/simulations/ld/"
ld_trips = read_csv(paste(ld_model_path, "0/0_trips.csv", sep  = ""))
ld_trips_germany = ld_trips %>%
  group_by(tripPurpose, tripState) %>%
  summarise(rate = n()/population_germany_1_percent / 5)
car_ld_trips_germany = ld_trips %>% filter(tripMode == "auto") %>%
  group_by(tripPurpose, tripState) %>%
  summarise(rate = n()/population_germany_1_percent / 5)

ld_trips_germany %>% 
  ggplot(aes(x = tripPurpose, y = rate, fill = tripState)) + geom_bar(stat = "identity", position = "stack") + 
  ggtitle("Trips/person by purpose and trip type")

car_ld_trips_germany %>% 
  ggplot(aes(x = tripPurpose, y = rate, fill = tripState)) + geom_bar(stat = "identity", position = "stack")  + 
  ggtitle("Trips/person by purpose and trip type")



##Read SP Germany and SP Munich

hh_germany = read_csv(paste(sd_model_path, "microData/hh_mito_1_percent_2011.csv", sep = ""))
pp_germany = read_csv(paste(sd_model_path, "microData/pp_mito_1_percent_2011.csv", sep = ""))

hh_munich = read_csv(paste(sd_munich_model_path, "microData/hh_2011.csv", sep = ""))
pp_munich = read_csv(paste(sd_munich_model_path, "microData/pp_2011.csv", sep = ""))

mean(pp_germany$income)
mean(pp_munich$income)/12

occupation_codes = c("1" = "worker", "2" = "unemployed", "3" = "student")

pp_by_occupation_de = pp_germany %>%
  group_by(occupation) %>%
  summarize(share = n()/nrow(pp_germany)) %>%
  mutate(area = "germany")
pp_by_occupation_muc = pp_munich %>%
  group_by(occupation) %>%
  summarize(share = n()/nrow(pp_munich)) %>%
  mutate(area = "muc")

pp_by_occupation_de %>% bind_rows(pp_by_occupation_muc) %>%
  mutate(occupation = recode(occupation, "1" = "worker", "2" = "unemployed", "3" = "student")) %>% 
  ggplot(aes(x = occupation, y = share, fill = area )) + 
  geom_bar(stat = "identity", position = "dodge")  + 
  ggtitle("Persons by occupation")


hh_by_size_germany = hh_germany %>%
  group_by(hhSize) %>%
  summarize(share = n()/nrow(hh_germany))  %>%
  mutate(area = "germany")
hh_by_size_munich = hh_munich %>%
  group_by(hhSize) %>% 
  summarize(share = n()/nrow(hh_munich)) %>%
  mutate(area = "muc")

hh_by_size_germany %>% bind_rows(hh_by_size_munich) %>%
  ggplot(aes(x = hhSize, y = share, fill = area )) + 
  geom_bar(stat = "identity", position = "dodge")  + 
  ggtitle("Household by size")

pp_germany %>% group_by(driversLicense) %>% summarize(n()/nrow(pp_germany))
pp_munich %>% group_by(driversLicense) %>% summarize(n()/nrow(pp_munich))


hh_by_autos_germany = hh_germany %>%
  group_by(autos) %>%
  summarize(share = n()/nrow(hh_germany))  %>%
  mutate(area = "germany")
hh_by_autos_munich = hh_munich %>%
  group_by(autos) %>% 
  summarize(share = n()/nrow(hh_munich)) %>%
  mutate(area = "muc")

hh_by_autos_germany %>% bind_rows(hh_by_autos_munich) %>%
  ggplot(aes(x = autos, y = share, fill = area )) + 
  geom_bar(stat = "identity", position = "dodge")  + 
  ggtitle("Househlolds by auto ownership level")


sum(hh_munich$autos)/nrow(hh_munich)
sum(hh_germany$autos)/nrow(hh_germany)

