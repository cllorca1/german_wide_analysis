pacman::p_load(ggplot2, dplyr, tidyr)


scenario = c("base", "scen1", "scen2", "scen3")
comparison = c("BAU", "ALT", "ALT", "ALT")
auto = c(70,60,65,30)
auto_toll = c(5,0,0,42)
rail = c(10,30,15,11)
bus = c(9,4,15,8)
air = c(11,6,5,9)


data_example = data.frame(scenario, comparison, auto, auto_toll, rail, bus, air)

data_example = data_example %>% pivot_longer(cols = c(auto, auto_toll, rail, bus, air), 
                                             values_to = "share", names_to = "Mode")

data_example = data_example %>% mutate(mode_scenario = paste(Mode, comparison, sep = "-"))

ld_mode_order = c("auto", "auto_toll", "bus", "rail", "air")
data_example$Mode = factor(data_example$Mode, levels = ld_mode_order)
data_example$comparison = factor(data_example$comparison, levels = c("BAU", "ALT"))


# colors_ld_modes  =c("auto" = "#C7C7C7", "auto_toll" = "#6D6D6D", "rail"  ="#F31111", 
# "bus" = "#69E11C", "air" = "blue")
# 
# 
# ggplot(data_example, aes(x = scenario, y = share, fill = mode )) + 
#   geom_bar(stat = "identity", position = "fill") + 
#   scale_fill_manual(values = colors_ld_modes) + theme_bw()
# 
# ggplot(data_example, aes(x = scenario, y = share, color = mode, group = mode)) + 
#   geom_line(stat = "identity", size =  1.5) + geom_point(size = 3)  + 
#   scale_color_manual(values = colors_ld_modes) + theme_bw()

# colors_ld_modes  =c("auto" = "#C7C7C7", "auto_toll" = "#3B3B3B", "rail"  ="#CA4D4D", 
#                     "bus" = "#6EA46E", "air" = "#536077")
# 
# ggplot(data_example, aes(x = scenario, y = share, fill = Mode)) + 
#   geom_bar(stat = "identity", position = position_fill(reverse = T)) + 
#   scale_fill_manual(values = colors_ld_modes) + theme_bw() + 
#   xlab("Scenario") + ylab("Modal share (%)") +
#   theme(legend.position = "bottom") + 
#   ggtitle("Option 1 - Self explanatory colors")
# 
# 
# ggplot(data_example %>% filter(scenario == "base" | scenario == "scen1"),
#        aes(x = Mode, y = share, alpha = comparison, fill = Mode, color = Mode)) + 
#   geom_bar(stat = "identity", position = position_dodge2(), size = 1) + 
#   scale_fill_manual(values = colors_ld_modes, name = "Mode") + 
#   scale_color_manual(values = colors_ld_modes, name = "Mode") + 
#   scale_alpha_manual(values = c("BAU" = 1, "ALT" = 0.5), guide = "none") + 
#   theme_bw() + 
#   xlab("Scenario") + ylab("Modal share (%)") +
#   theme(legend.position = "bottom") + 
#   ggtitle("Option 1 - Self explanatory colors")
# 
# 
# linetype_ld_modes  =c("auto" = "solid", "auto_toll" = "dotted", "rail"  ="solid",
#                       "bus" = "solid", "air" = "solid")
# 
# 
# ggplot(data_example, aes(x = scenario, y = share, color = Mode, group = Mode, linetype = Mode)) + 
#   geom_line(stat = "identity", size = 1.5)  + geom_point(size = 4) + 
#   scale_color_manual(values = colors_ld_modes) +
#   scale_linetype_manual(values = linetype_ld_modes) + 
#   theme_bw() + 
#   guides(color = guide_legend(override.aes = list(size = 1) ) ) + 
#   theme(legend.position = "bottom")+ 
#   xlab("Scenario") + ylab("Modal share (%)") + 
#   ggtitle("Option 1 - Self explanatory colors")
# 
# 



colors_ld_modes  =c("auto" = "#aaaaaa", "auto_toll" = "#2d2d2d", "rail"  ="#764c6e", 
                    "bus" = "#bdc9bb", "air" = "#83adb5")

ggplot(data_example, aes(x = scenario, y = share, fill = Mode )) +
  geom_bar(stat = "identity", position = position_fill(reverse = T)) +
  scale_fill_manual(values = colors_ld_modes) + theme_bw()+ 
  xlab("Scenario") + ylab("Modal share (%)")  + theme(legend.position = "bottom") + 
  ggtitle("Modal share by scenario")

ggplot(data_example %>% filter(scenario == "base" | scenario == "scen1"),
       aes(x = Mode, y = share, alpha = comparison, fill = Mode, color = Mode, label = comparison)) + 
  geom_bar(stat = "identity", position = position_dodge2(), size = 1) + 
  scale_fill_manual(values = colors_ld_modes, name = "Mode") + 
  scale_color_manual(values = colors_ld_modes, guide = "none") + 
  scale_alpha_manual(values = c("BAU" = 1, "ALT" = 0.5), guide = "none") + 
  theme_bw() + 
  xlab("Mode by scenario") + ylab("Modal share (%)") +
  theme(legend.position = "bottom") + 
  ggtitle("Modal share by scenario - before/after comparison") + 
  geom_text(position = position_dodge2(width = 0.95), color = "black", vjust = -0.4, hjust = "middle")
 



linetype_ld_modes  =c("auto" = "dotted", "auto_toll" = "dotted", "rail"  ="solid",
                      "bus" = "solid", "air" = "solid")




# ggplot(data_example, aes(x = scenario, y = share, color = Mode, group = Mode, linetype = Mode)) +
#   geom_line(stat = "identity", size =  1.5) + geom_point(size = 4)  +
#   scale_color_manual(values = colors_ld_modes) +
#   scale_linetype_manual(values = linetype_ld_modes) + 
#   theme_bw() + 
#   guides(color = guide_legend(override.aes = list(size = 1) ) ) + 
#   theme(legend.position = "bottom")+ 
#   xlab("Scenario") + ylab("Modal share (%)")+ 
#   ggtitle("Option 2a - Serious colors")



linetype_ld_modes  =c("auto" = "solid", "auto_toll" = "solid", "rail"  ="dotted",
                      "bus" = "dotted", "air" = "dotted")

ggplot(data_example, aes(x = scenario, y = share, color = Mode, group = Mode, linetype = Mode)) +
  geom_line(stat = "identity", size =  1.5) + geom_point(size = 4)  +
  scale_color_manual(values = colors_ld_modes) +
  scale_linetype_manual(values = linetype_ld_modes) + 
  theme_bw() + 
  guides(color = guide_legend(override.aes = list(size = 1) ) ) + 
  theme(legend.position = "bottom")+ 
  xlab("Scenario") + ylab("Modal share (%)") +
  ggtitle("Line plot")


