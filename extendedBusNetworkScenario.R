pacman::p_load(readr, dplyr, tidyr, ggplot2)

speed_analysis = read_csv("scenarios/extendedBusNetworkCurrentSpeeds.csv")

mode_colors = c("bus" = "#489245" , "train" = "#c34e4e", "car" = "gray20")


speed_analysis %>%
  filter(speed > 0.1, speed < 40, dist > 50000, !is.na(share_time)) %>%
  ggplot(aes(x = speed*3.6,..density.., color = mode)) +
  geom_freqpoly(size=1) +
  theme_bw() + scale_color_manual(values = mode_colors) + 
  xlab("Average speed (door to door) (km/h)") + ylab("Frequency")

ggsave(filename = "tmp/speed_before_scenario_2.png", device = png(),
       width = 15, height= 10, units = "cm", scale = 1.5)

max_transfers = Inf
min_share = 0.75

label_yes = paste(" > 75% on LD bus")
label_no = paste("< 75% on LD bus")

speed_analysis %>%
  mutate(is_coach = if_else(mode == "bus", if_else(share_time > min_share & transfers < max_transfers, label_yes, label_no), label_yes)) %>%
  filter(speed > 0.1, speed < 40, dist > 50000, !is.na(share_time)) %>%
  ggplot(aes(x = speed*3.6, color = mode,..density.., linetype = is_coach)) +
  geom_freqpoly(size = 1) + theme_bw() + scale_color_manual(values = mode_colors) + 
  xlab("Average speed (door to door) (km/h)") + ylab("Frequency")

ggsave(filename = "tmp/speed_with_without_scenario_2.png", device = png(),
       width = 15, height= 10, units = "cm", scale = 1.5)


speed_analysis %>%
  mutate(is_coach = if_else(mode == "bus", if_else(share_time > min_share & transfers < max_transfers, "0yes", "1no"), "0yes"))%>%
  filter(speed > 0.1, speed < 40, dist > 50000, !is.na(share_time)) %>%
  group_by(is_coach, mode) %>% summarize(speed_ms = mean(speed), speed_kmh = mean(speed) * 3.6, count = n())


#edit manaually
speed_analysis %>%
  mutate(is_coach = if_else(mode == "bus", if_else(share_time > min_share & transfers < max_transfers, label_yes, label_no), label_yes)) %>%
  mutate(speed = if_else(is_coach == label_no, speed * 10.5/7.6, speed)) %>%
  filter(speed > 0.1, speed < 40, dist > 50000, !is.na(share_time)) %>%
  ggplot(aes(x = speed*3.6, color = mode,..density.., linetype = is_coach)) +
  geom_freqpoly(size = 1) +
  theme_bw() + scale_color_manual(values = mode_colors) + 
  xlab("Average speed (door to door) (km/h)") + ylab("Frequency")

ggsave(filename = "tmp/speed_after_scenario_2.png", device = png(),
       width = 15, height= 10, units = "cm", scale = 1.5)


reduction_at_transfers = 60 * 15

speed_analysis %>%
  mutate(is_coach = if_else(mode == "bus", if_else(share > min_share & transfers < max_transfers, "0yes", "1no"), "0yes")) %>%
  mutate(calc_time = if_else(mode=="bus" & transfers > 0, time - reduction_at_transfers * transfers, time)) %>%
  mutate(calc_speed = if_else(is_coach == "1no", dist/calc_time * 30/20, dist/calc_time)) %>%
  filter(speed > 0, speed < 40, dist > 50000) %>%
  ggplot(aes(x = calc_speed*3.6, color = mode,..density.., linetype = is_coach)) +
  geom_freqpoly(size = 1) +
  theme_bw() +
  xlab("Speed after increasing speed of zones non-connected by bus and reducing transfer time (km/h)")


speed_analysis %>%
  filter(speed > 0, speed < 40, dist > 50000) %>%
  mutate(is_coach = if_else(mode == "bus", if_else(share > min_share & transfers < max_transfers, "0yes", "1no"), "0yes")) %>%
  mutate(calc_time = if_else(mode=="bus" & transfers > 0, time - reduction_at_transfers * transfers, time)) %>%
  mutate(calc_speed = if_else(is_coach == "1no", dist/calc_time * 30/20, dist/calc_time)) %>%
  group_by(is_coach, mode) %>% summarize(speed_ms = mean(calc_speed), speed_kmh = mean(calc_speed) * 3.6, count = n())




