pacman::p_load(readr, dplyr, tidyr, ggplot2)

source("C:/code/omx/api/r/omx2.R")

folder = "C:/models/transit_germany/output/skims/"

scenarios = c("carWithToll", "carWithoutToll")

zones = 1:11879
subsample = sample(zones,300)

analysis = data.frame()

for (scenario in scenarios){
  path = paste(folder, scenario, "/car_matrix.omx", sep = "")
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
                       distance = this_d,
                       time = this_t, 
                       toll_distance = this_toll_d)
      analysis = analysis %>% bind_rows(row)
      
    }
  }
  
}


analysis %>% filter(distance < 1000e3, distance > 0) %>% 
  ggplot(aes(x=distance/time * 3.6,..density.., color = scenario)) + 
  geom_freqpoly(size = 1, binwidth = 1) +
  xlab("Average travel speed (km/)") + ylab("Frequency") + theme_bw()

ggsave(filename = "tmp/speed_scenario_4.png", device = png(),
       width = 15, height= 10, units = "cm", scale = 1.5)


analysis %>% filter(distance < 1000e3, distance > 0) %>% 
  ggplot(aes(x=toll_distance/distance * 100, color = scenario)) +
  geom_freqpoly(size = 1, binwidth = 1) +
  xlim(1,99) + xlab("Share on toll highway (%)") + ylab("Frequency") + theme_bw()

analysis %>% filter(distance < 1000e3, distance > 0) %>% 
  ggplot(aes(x= distance/1000, color = scenario)) +
  geom_freqpoly(size = 1, binwidth = 30) +
  xlab("Distance (km)") + ylab("Frequency") + theme_bw()

analysis %>% filter(distance < 1000e3, distance > 0) %>% 
  ggplot(aes(x= time/3600, ..density..,color = scenario)) +
  geom_freqpoly(size = 1, binwidth = 0.33) +
  xlab("Average time (h)") + ylab("Frequency") + theme_bw()

ggsave(filename = "tmp/time_scenario_4.png", device = png(),
       width = 15, height= 10, units = "cm", scale = 1.5)

toll_cost = 0.05 / 1000
operating_cost = 0.08 / 1000

analysis %>% filter(distance < 1000e3, distance > 0) %>% 
  ggplot(aes(x=toll_distance* toll_cost + distance * operating_cost,..density.., color = scenario)) +
  geom_freqpoly(size = 1, binwidth = 5) +
  xlab("Average travel cost (EUR)") + ylab("Frequency") + theme_bw()

ggsave(filename = "tmp/cost_scenario_4.png", device = png(),
       width = 15, height= 10, units = "cm", scale = 1.5)



