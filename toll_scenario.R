pacman::p_load(readr, dplyr, tidyr, ggplot2)

source("C:/code/omx/api/r/omx2.R")

folder = "C:/models/transit_germany/output/skims/"

scenarios = c("With toll", "Without toll", "With toll", "Without toll")
groups = c("B", "B", "A", "A")
folders = c("carWithToll_ab", "carWithoutToll_ab", "carWithToll", "carWithoutToll")
matrix_names = c("/car_matrix_toll.omx", "/car_matrix_no_toll.omx", "/car_matrix_toll.omx", "/car_matrix_no_toll.omx")

zones = 1:11879
subsample = sample(zones,300)

analysis = data.frame()

for (i in 1:length(scenarios)){
  scenario = scenarios[i]
  group = groups[i]
  this_folder = folders[i]
  matrix_name = matrix_names[i]
  path = paste(folder, this_folder, matrix_name, sep = "")
  distance = readMatrixOMX(path, "distance_m")
  toll_distance  = readMatrixOMX(path, "tollDistance_m")
  time = readMatrixOMX(path, "time_s")
  print ("Read omx")
  for (o in subsample){
    for (d in subsample){
      this_d = distance[o,d]
      this_toll_d = toll_distance[o,d]
      this_t = time[o,d]
      row = data.frame(scenario, o,d,
                       group,
                       distance = this_d,
                       time = this_t, 
                       toll_distance = this_toll_d)
      analysis = analysis %>% bind_rows(row)
      
    }
  }
  
}

analysis = analysis %>% mutate(scenario = recode(scenario,
                                                 "With toll" = "With toll",
                                                 "Without toll" = "Avoid toll"))


analysis %>% filter(distance < 1000e3, distance > 0) %>% 
  ggplot(aes(x=distance/time * 3.6,..density.., color = scenario)) + 
  geom_freqpoly(size = 1.5, binwidth = 1) +
  scale_color_manual(values = c("Avoid toll" = "#aaaaaa", "With toll" = "#2d2d2d"), name = "Scenario") + 
  xlab("Average travel speed (km/)") + ylab("Frequency") + theme_bw() + 
  facet_wrap(.~group)

ggsave(filename = "tmp/speed_scenario_4.jpg", device = "jpeg",
       width = 15, height= 10, units = "cm", scale = 1.5)


analysis %>% filter(distance < 1000e3, distance > 0) %>% 
  ggplot(aes(x=toll_distance/distance * 100, color = scenario)) +
  geom_freqpoly(size = 1.5, binwidth = 1) +
  scale_color_manual(values = c("Avoid toll" = "#aaaaaa", "With toll" = "#2d2d2d"), name = "Scenario") + 
  xlim(1,99) + xlab("Share on toll highway (%)") + ylab("Frequency") + theme_bw() + 
  facet_wrap(.~group)

analysis %>% filter(distance < 1000e3, distance > 0) %>% 
  ggplot(aes(x= distance/1000, color = scenario)) +
  geom_freqpoly(size = 1.5, binwidth = 30) +
  scale_color_manual(values = c("Avoid toll" = "#aaaaaa", "With toll" = "#2d2d2d"), name = "Scenario") + 
  xlab("Distance (km)") + ylab("Frequency") + theme_bw() + 
  facet_wrap(.~group)

ggsave(filename = "tmp/distance_scenario_4.jpg", device = "jpeg",
       width = 15, height= 10, units = "cm", scale = 1.5)


analysis %>% filter(distance < 1000e3, distance > 0) %>% 
  ggplot(aes(x= time/3600, ..density..,color = scenario)) +
  geom_freqpoly(size = 1.5, binwidth = 0.33) +
  scale_color_manual(values = c("Avoid toll" = "#aaaaaa", "With toll" = "#2d2d2d"), name = "Scenario") + 
  xlab("Average time (h)") + ylab("Frequency") + theme_bw() + 
  facet_wrap(.~group)

ggsave(filename = "tmp/time_scenario_4jpg", device = "jpeg",
       width = 15, height= 10, units = "cm", scale = 1.5)

toll_cost = 0.02 / 1000
operating_cost = 0.08 / 1000

analysis %>% filter(distance < 1000e3, distance > 0) %>% 
  ggplot(aes(x=toll_distance* toll_cost + distance * operating_cost,..density.., color = scenario)) +
  geom_freqpoly(size = 1.5, binwidth = 5) +
  scale_color_manual(values = c("Avoid toll" = "#aaaaaa", "With toll" = "#2d2d2d"), name = "Scenario") + 
  xlab("Average travel cost (EUR)") + ylab("Frequency") + theme_bw() + 
  facet_wrap(.~group)

ggsave(filename = "tmp/cost_scenario_4.jpg", device = "jpeg",
       width = 15, height= 10, units = "cm", scale = 1.5)



