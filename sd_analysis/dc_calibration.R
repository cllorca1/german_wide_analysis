pacman::p_load(readr, dplyr, tidyr, ggplot2)


calibration = read_csv("C:/projects/bast_entlastung/analysis/mito_sd_germany/dc_calibration_HBS_HBO_HBR_NHBO_NHBW.csv") %>% 
  bind_rows(read_csv("C:/projects/bast_entlastung/analysis/mito_sd_germany/dc_calibration_HBW_HBE.csv"))


ggplot(calibration, aes(x = iteration, y = simulated, color = purpose)) + geom_line() + 
  facet_wrap(.~purpose) + xlim(0,20)

ggplot(calibration, aes(x = iteration, y = factorDistance, color = purpose)) + geom_line() + 
  facet_wrap(.~purpose) + xlim(0,20)
