pacman::p_load(readr, dplyr, ggplot2, tidyr, sf, tmap, leaflet, plotly)

mc_calibration = read_csv("C:/projects/bast_entlastung/analysis/mito_sd_germany/mode_choice_calibration.csv")

my_colors = c("autoDriver" = "#878787",
  "autoPassenger" = "#a9a9a9",
  "train" = "#789ec9",
  "tramOrMetro" = "#5c768d",
  "bus" = "#50546d",
  "bicycle" = "#87c77d",
  "walk" = "#53815b", 
  "taxi" = "#F9E892", "privateAV" = "red", "sharedAV" = "red", "pooledTaxi" = "red")

modes_ordered = c("autoDriver",
                  "autoPassenger",
                  "train",
                  "tramOrMetro",
                  "bus" ,
                  "bicycle",
                  "walk", 
                  "taxi", "privateAV" , "sharedAV" , "pooledTaxi" )


mc_calibration$mode = factor(mc_calibration$mode, levels = modes_ordered)

ggplot(mc_calibration, aes(x=iteration, y = k, color = mode)) + 
  geom_path(stat  ="identity", size = 1) + facet_grid(region~purpose)  + 
  scale_color_manual(values = my_colors) + 
  theme_bw() + theme(axis.text.x = element_text(angle = 90))

goal_shares = mc_calibration

final_it = max(mc_calibration$iteration) 

goal_shares = goal_shares %>%
  mutate(iteration = iteration + final_it + 1) %>% 
  mutate(trips = trips/sim_share * observed_share) %>%
  mutate(sim_share = observed_share)


mc_calibration$case =  "simulation"
goal_shares$case = "observation"

mc_calibration = mc_calibration %>% bind_rows(goal_shares)

#for visualization clarity and quick plotting, the iterations > 100 are "the reality according to mid"

ggplot(mc_calibration, aes(x=iteration, y = sim_share, fill = mode)) + 
  geom_bar(stat  ="identity", position = "stack", width = 1.1) +
  facet_grid(region~purpose)  + 
  scale_fill_manual(values = my_colors) + 
  scale_color_manual(values = my_colors) + 
  scale_alpha_manual(values = c(1, 0.75)) + theme_bw() + theme(axis.text.x = element_text(angle = 90)) + 
  scale_y_continuous(expand = c(0,0)) + 
  geom_vline(xintercept = 199, color = "black", linetype = "dashed") + 
  theme(legend.position = "bottom")
  




