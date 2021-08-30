pacman::p_load(readr, dplyr, tidyr, ggplot2)

#LD train

path  = "c:/projects/bast_entlastung/analysis/ld_model_analysis/scenarios/results_scenario_1_v3.csv"

matrixdata = read_csv(path)

matrixdata %>%
  filter(!is.infinite(distance_m)) %>% 
  filter(distance_m > 0) %>% 
  filter(alt == "base") %>%
  summarize(share = mean(train_distance_share))


matrixdata %>% sample_frac(0.01) %>%
  filter(alt == "base") %>%
  ggplot(aes(x=distance_m/1000, y = train_distance_share)) + geom_point(alpha = 0.05) +
  stat_smooth() + xlim(0,1500)

matrixdata %>%
  filter(!is.infinite(distance_m)) %>% 
  filter(distance_m > 0) %>% 
  filter(alt == "base") %>%
  mutate(bin = cut(distance_m/1000, breaks = c(0,100,200,300,400,500,600,700,10000))) %>%
  group_by(bin) %>%
  summarize(share = mean(train_distance_share), count_od_pairs = n())



#Ld bus

path  = "c:/projects/bast_entlastung/analysis/ld_model_analysis/scenarios/extendedBusNetworkCurrentSpeeds.csv"

matrixdata = read_csv(path)

matrixdata %>%
  filter(!is.infinite(dist)) %>% 
  filter(dist > 0) %>% 
  filter(mode == "bus") %>%
  summarize(share = mean(share_distance))

matrixdata %>% sample_frac(0.01) %>%
  filter(mode == "bus") %>%
  ggplot(aes(x=dist/1000, y = share_distance)) + geom_point(alpha = 0.05) +
  stat_smooth() + xlim(0,1500)

matrixdata %>%
  filter(!is.infinite(dist)) %>% 
  filter(dist > 0) %>% 
  filter(mode == "bus") %>%
  mutate(bin = cut(dist/1000, breaks = c(0,100,200,300,400,500,600,700,10000))) %>%
  group_by(bin) %>%
  summarize(share = mean(share_distance),count_od_pairs = n())



