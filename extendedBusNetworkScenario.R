pacman::p_load(readr, dplyr, tidyr, ggplot2)

speed_analysis = read_csv("scenarios/extendedBusNetworkCurrentSpeeds.csv")

zonal_data = read_csv("c:/models/mito/germany/input/zoneSystemDE_2col.csv")


zonal_data$BBSR_type = recode(as.character(zonal_data$BBSR_type), 
                              "10" = "10: core",
                              "20" = "20: medium_city",
                              "30" = "30:town",
                              "40" = "40: rural")

speed_analysis = speed_analysis %>% left_join(zonal_data, by = (c("o"="Zone")))
speed_analysis = speed_analysis %>% left_join(zonal_data, by = (c("d"="Zone")),suffix = c("", "_d"))


mode_colors = c("bus" = "#489245" , "train" = "#c34e4e", "car" = "gray20")

# 
# speed_analysis %>%
#   filter(speed > 0.1, speed < 40, dist > 50000, !is.na(share_time)) %>%
#   ggplot(aes(x = speed*3.6,..density.., color = mode)) +
#   geom_freqpoly(size=1) +
#   theme_bw() + scale_color_manual(values = mode_colors) + 
#   xlab("Average speed (door to door) (km/h)") + ylab("Frequency")

# ggsave(filename = "tmp/speed_before_scenario_2.png", device = png(),
#        width = 15, height= 10, units = "cm", scale = 1.5)


speed_analysis %>%
  filter(speed > 0.1, speed < 40, !is.na(share_time)) %>%
  group_by(BBSR_type, BBSR_type_d, mode) %>%
  summarize(time = mean(time), speed = mean(speed)) %>% 
  ggplot(aes(x = BBSR_type_d, y = speed * 3.6, fill = mode)) +
  geom_bar(stat = "identity") +
  theme_bw() + scale_fill_manual(values = mode_colors) + 
  ylab("Average speed (door to door) (km/hh)")  + 
  facet_grid(BBSR_type ~ mode)



max_transfers = Inf
min_share = 0.75

label_yes = paste(" > 75% on LD bus")
label_no = paste("< 75% on LD bus")



# speed_analysis %>%
#   mutate(is_coach = if_else(mode == "bus", if_else(share_time > min_share & transfers < max_transfers, label_yes, label_no), label_yes)) %>%
#   filter(speed > 0.1, speed < 40, dist > 50000, !is.na(share_time)) %>%
#   ggplot(aes(x = speed*3.6, color = mode,..density.., linetype = is_coach)) +
#   geom_freqpoly(size = 1) + theme_bw() + scale_color_manual(values = mode_colors) + 
#   xlab("Average speed (door to door) (km/h)") + ylab("Frequency")

# ggsave(filename = "tmp/speed_with_without_scenario_2.png", device = png(),
#        width = 15, height= 10, units = "cm", scale = 1.5)

mode_bus_colors = c("#489245","#BAD6B9")


speed_analysis %>%
  mutate(is_coach = if_else(mode == "bus", if_else(share_time > min_share & transfers < max_transfers, label_yes, label_no), label_yes)) %>%
  filter(speed > 0.1, speed < 40, !is.na(share_time)) %>%
  group_by(BBSR_type, BBSR_type_d, is_coach, mode) %>%
  summarize(time = mean(time), speed = mean(speed)) %>% 
  filter(mode == "bus") %>%
  ggplot(aes(x = is_coach, y = speed*3.6, fill = is_coach)) +
  geom_bar(stat = "identity") +
  theme_bw() + 
  scale_fill_manual(values = mode_bus_colors) + 
  ylab("Average speed (door to door) (km/h)") + 
  facet_grid(BBSR_type ~ BBSR_type_d) + 
  theme(axis.text.x = element_text(angle = 90))

ggsave(filename = "tmp/speed_with_without_scenario_2.jpeg", device = "jpeg",
        width = 15, height= 10, units = "cm", scale = 1.5)


speed_analysis %>%
  mutate(is_coach = if_else(mode == "bus", if_else(share_time > min_share & transfers < max_transfers, "0yes", "1no"), "0yes"))%>%
  filter(speed > 0.1, speed < 40, !is.na(share_time)) %>%
  group_by(is_coach, mode) %>% summarize(speed_ms = mean(speed), speed_kmh = mean(speed) * 3.6, count = n())


#edit manaually
# speed_analysis %>%
#   mutate(is_coach = if_else(mode == "bus", if_else(share_time > min_share & transfers < max_transfers, label_yes, label_no), label_yes)) %>%
#   mutate(speed = if_else(is_coach == label_no, speed * 10.5/7.6, speed)) %>%
#   filter(speed > 0.1, speed < 40, dist > 50000, !is.na(share_time)) %>%
#   ggplot(aes(x = speed*3.6, color = mode,..density.., linetype = is_coach)) +
#   geom_freqpoly(size = 1) +
#   theme_bw() + scale_color_manual(values = mode_colors) + 
#   xlab("Average speed (door to door) (km/h)") + ylab("Frequency")

# ggsave(filename = "tmp/speed_after_scenario_2.png", device = png(),
#        width = 15, height= 10, units = "cm", scale = 1.5)

speed_analysis %>%
  mutate(is_coach = if_else(mode == "bus", if_else(share_time > min_share & transfers < max_transfers, label_yes, label_no), label_yes)) %>%
  mutate(speed = if_else(is_coach == label_no, speed * 10.3/7.7, speed)) %>%
  filter(speed > 0.1, speed < 40, !is.na(share_time)) %>%
  group_by(BBSR_type, BBSR_type_d, is_coach, mode) %>%
  summarize(time = mean(time), speed = mean(speed)) %>% 
  filter(mode == "bus") %>%
  ggplot(aes(x = is_coach, y = speed * 3.6, fill = is_coach)) +
  geom_bar(stat = "identity") +
  theme_bw() +
  scale_fill_manual(values = mode_bus_colors) + 
  ylab("Average speed (door to door) (km/h)") + 
  facet_grid(BBSR_type ~ BBSR_type_d) + 
  theme(axis.text.x = element_text(angle = 90))

ggsave(filename = "tmp/speed_modfied_scenario_2.jpeg", device = "jpeg",
       width = 15, height= 10, units = "cm", scale = 1.5)




new_speeds = speed_analysis %>%
  mutate(is_coach = if_else(mode == "bus", if_else(share_time > min_share & transfers < max_transfers, label_yes, label_no), label_yes)) %>%
  mutate(speed = if_else(is_coach == label_no, speed * 10.3/7.7, speed)) %>%
  mutate(time = if_else(is_coach == label_no, time / 10.3 * 7.7, time)) %>%
  filter(speed > 0.1, speed < 40, !is.na(share_time)) %>%
  group_by(BBSR_type, BBSR_type_d, mode) %>%
  summarize(time = mean(time), speed = mean(speed)) %>% 
  filter(mode == "bus") %>%
  mutate(when = "2:after")

old_speeds = speed_analysis %>%
  filter(speed > 0.1, speed < 40, !is.na(share_time)) %>%
  group_by(BBSR_type, BBSR_type_d, mode) %>%
  summarize(time = mean(time), speed = mean(speed)) %>% 
  filter(mode == "bus") %>% 
  mutate(when = "1:before")

mode_bus_colors_2 = c("#489245","#1B371A")

new_speeds %>%
  bind_rows(old_speeds) %>% 
  ggplot(aes(x = when, y = speed * 3.6, fill = when)) +
  geom_bar(stat = "identity") +
  theme_bw() +
  scale_fill_manual(values = mode_bus_colors_2) + 
  ylab("Average speed (door to door) (km/h)") +
  facet_grid(BBSR_type ~ BBSR_type_d) + 
  theme(axis.text.x = element_text(angle = 90))

ggsave(filename = "tmp/speed_finals_scenario_2.jpeg", device = "jpeg",
       width = 15, height= 10, units = "cm", scale = 1.5)


new_speeds %>%
  bind_rows(old_speeds) %>% 
  ggplot(aes(x = when, y = time /3600, fill = when)) +
  geom_bar(stat = "identity") +
  theme_bw() +
  scale_fill_manual(values = mode_bus_colors_2) + 
  ylab("Average time (door to door) (h)") +
  facet_grid(BBSR_type ~ BBSR_type_d) + 
  theme(axis.text.x = element_text(angle = 90))

ggsave(filename = "tmp/times_finals_scenario_2.jpeg", device = "jpeg",
       width = 15, height= 10, units = "cm", scale = 1.5)


# reduction_at_transfers = 60 * 15
# 
# speed_analysis %>%
#   mutate(is_coach = if_else(mode == "bus", if_else(share > min_share & transfers < max_transfers, "0yes", "1no"), "0yes")) %>%
#   mutate(calc_time = if_else(mode=="bus" & transfers > 0, time - reduction_at_transfers * transfers, time)) %>%
#   mutate(calc_speed = if_else(is_coach == "1no", dist/calc_time * 30/20, dist/calc_time)) %>%
#   filter(speed > 0, speed < 40, dist > 50000) %>%
#   ggplot(aes(x = calc_speed*3.6, color = mode,..density.., linetype = is_coach)) +
#   geom_freqpoly(size = 1) +
#   theme_bw() +
#   xlab("Speed after increasing speed of zones non-connected by bus and reducing transfer time (km/h)")
# 
# 
# speed_analysis %>%
#   filter(speed > 0, speed < 40, dist > 50000) %>%
#   mutate(is_coach = if_else(mode == "bus", if_else(share > min_share & transfers < max_transfers, "0yes", "1no"), "0yes")) %>%
#   mutate(calc_time = if_else(mode=="bus" & transfers > 0, time - reduction_at_transfers * transfers, time)) %>%
#   mutate(calc_speed = if_else(is_coach == "1no", dist/calc_time * 30/20, dist/calc_time)) %>%
#   group_by(is_coach, mode) %>% summarize(speed_ms = mean(calc_speed), speed_kmh = mean(calc_speed) * 3.6, count = n())




