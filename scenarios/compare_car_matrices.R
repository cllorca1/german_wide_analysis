pacman::p_load(dplyr, readr, ggplot2, tidyr)

car_matrices = read_csv("scenarios/car_matrix_comparison.csv")

car_matrices = car_matrices %>%
  select(o,d,dist_original = d_1_m,dist_toll_matrix = d_2_m,time_original = t_1_s, time_toll_matrix = t_2_s) 


car_matrices$dist_toll_matrix[car_matrices$dist_toll_matrix > 5e8] = Inf


car_matrices_sample = car_matrices[sample(1:nrow(car_matrices), size  = 50000),]

car_matrices_sample %>% ggplot(aes(x = dist_original/1000, y = dist_toll_matrix/1000)) +
  geom_point(alpha = 0.2) +
  geom_abline(slope = 1, intercept = 0, color = "red") + 
  xlab("distance (original matrix car_matrices.omx) (km)") + 
  ylab("distance (toll matrix car_matrix_toll.omx) (km)")



car_matrices_sample %>% 
  filter(o < 11718, d < 11718) %>% 
  ggplot(aes(x = dist_original/1000, y = dist_toll_matrix/1000)) +
  geom_point(alpha = 0.2) +
  geom_abline(slope = 1, intercept = 0, color = "red") + 
  xlab("distance (original matrix car_matrices.omx) (km)") + 
  ylab("distance (toll matrix car_matrix_toll.omx) (km)")

car_matrices %>% 
  filter(o > 11717, d > 11717) %>% 
  ggplot(aes(x = dist_original/1000, y = dist_toll_matrix/1000)) +
  geom_point(alpha = 0.2) +
  geom_abline(slope = 1, intercept = 0, color = "red") + 
  xlab("distance (original matrix car_matrices.omx) (km)") + 
  ylab("distance (toll matrix car_matrix_toll.omx) (km)")

wrong_values = car_matrices %>% filter(is.infinite(dist_toll_matrix) | is.infinite(dist_original))

wrong_values_o = wrong_values %>% group_by(z = o) %>% summarize(count_o = n() * 100)
wrong_values_d = wrong_values %>% group_by(z = d) %>% summarize(count_d = n() * 100)

wrong_values_od = wrong_values_o %>% left_join(wrong_values_d)
wrong_values_od[is.na(wrong_values_od)] = 0
wrong_values_od = wrong_values_od %>% mutate(count = (count_o + count_d))
write_csv(wrong_values_od, "tmp/zones_with_infinity.csv")

hist(wrong_values$d + wrong_values$o)

summary(car_matrices)


my_summary = car_matrices %>% 
  group_by(if_else(dist_original > 4000e3,"was > 4000","was < 4000"), 
                          if_else(dist_toll_matrix > 4000e3 ,"is > 4000","is < 4000")) %>% 
  summarise(n())
                                  
                          

# summary_o = car_matrices %>%
  # mutate(dist_original = if_else(dist_original > 4000e3, as.numeric(NA), dist_original)) %>% 
  # mutate(dist_toll_matrix = if_else(dist_toll_matrix > 4000e3, as.numeric(NA), dist_toll_matrix)) %>%
  #        group_by(o) %>% summarise(mean_original = mean(dist_original, na.rm = T)/1000, 
  #                                                      mean_toll = mean(dist_toll_matrix, na.rm = T)/1000)


