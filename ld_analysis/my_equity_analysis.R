pacman::p_load(data.table, dplyr, ggplot2, tidyr, readr, plotly, sf, tmap, rgeos, RColorBrewer)

out_folder= "Z:/projects/2019/BASt/data/input_files_LD/germanymodel/output/carlos_with_free_flow_before_20210707/"
ld_mode_order = c("auto", "auto_toll", "bus", "rail", "air")
colors_ld_modes  =c("auto" = "#aaaaaa", "auto_toll" = "#676767", "rail"  ="#764c6e", 
                    "bus" = "#bdc9bb", "air" = "#83adb5")

save_plots = T

scenarios = c("0", 
              "1_A_0_N",
             "4_B_2", 
              "7_A_3")

scenario_names = scenarios
scenario_groups = substr(scenarios, 1,1)

scenario_parameters = read_csv("scenarios/scenario_labels.csv")
scenario_parameters = scenario_parameters %>%
  mutate(label = if_else(!is.na(var2), 
                         paste(scenario, "\n(", var1, "=", val1, ", ",  var2, "=", val2, ")", sep  ="") , 
                         paste(scenario, "\n(", var1, "=", val1, ")", sep  =""))) %>%
  mutate(label2 = if_else(!is.na(var2), 
                          paste(scenario, "\n", var1, "=", val1, "\n",  var2, "=", val2, "", sep  =""),
                          paste(scenario, "\n", var1, "=", val1, sep  ="")))

#manually correct the label of base scenario
scenario_parameters$label[1] = "0"
scenario_parameters$label2[1] = "0"

scenario_parameters = scenario_parameters %>% select(scenario, label, label2)


trips = data.frame()

for (i in 1:length(scenarios)){
  scenario_folder = scenarios[i]
  this_trips = read_csv(paste(out_folder,scenario_folder, "/0_trips.csv", sep  =""))
  this_trips$scenario_name = scenario_names[i]
  this_trips$scenario_group = scenario_groups[i]
  trips = trips %>% bind_rows(this_trips)
  print(scenario_folder)
}


t = trips %>% sample_frac(0.1) %>% rowwise() %>% mutate(time = if_else(tripMode == "auto", time_auto, 
                                                                       if_else(tripMode == "rail", time_rail,
                                                                               if_else(tripMode == "auto_noToll", time_auto_noToll,
                                                                                       if_else(tripMode == "bus", time_bus,
                                                                                               if_else(tripMode == "rail_shuttle", time_rail_shuttle,
                                                                                                       time_air)))))) %>%
  mutate(time = as.numeric(time)) %>% 
  filter(!is.na(time)) %>% 
  group_by(scenario_name) %>% arrange(time, groups = T) %>%
  mutate(i = 1, order = cumsum(i), cum_time = cumsum(time), 
         total_order = sum(i), total_time = sum(time, na.rm = T))


ggplot(t , aes(y = cum_time/total_time, x = order/total_order, color = scenario_name, group = scenario_name)) +
  geom_line(size = 1) + geom_abline(slope = 1, intercept = 0)


##alternatively with the average travel time: 

trips2 = trips

t = trips2 %>%  sample_frac(0.1)  %>% mutate(time = as.numeric(time_auto) * as.numeric(utility_auto), 
                                                      as.numeric(time_rail)* as.numeric(utility_rail), 
                                                      as.numeric(time_auto_noToll)* as.numeric(utility_auto_noToll), 
                                                      as.numeric(time_bus)* as.numeric(utility_bus), 
                                                      as.numeric(time_rail_shuttle)* as.numeric(utility_rail_shuttle), 
                                                      as.numeric(time_air)* as.numeric(utility_air)) %>%
  filter(!is.na(time)) %>% 
  group_by(scenario_name) %>% arrange(time, groups = T) %>%
  mutate(i = 1, order = cumsum(i), cum_time = cumsum(time), 
         total_order = sum(i), total_time = sum(time, na.rm = T))


ggplot(t, aes(x = time, color = scenario_name)) + geom_density() + scale_x_log10()


ggplot(t , aes(y = cum_time/total_time, x = order/total_order, color = scenario_name, group = scenario_name)) +
  geom_line(size = 1) + geom_abline(slope = 1, intercept = 0)

