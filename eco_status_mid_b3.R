library(dplyr)
library(readr)
library(ggplot2)

hh_mid_b3 <- read_delim("c:/models/midprocessing2017/data/B3_Lokal-Datensatzpaket/CSV/MiD2017_Lokal_Haushalte.csv", 
                                ";", escape_double = FALSE, locale = locale(decimal_mark = ","), 
                                trim_ws = TRUE)



ec_status_per_5_km_zone = hh_mid_b3 %>%
  group_by(oek_status, GITTER_5km) %>%
  summarize(w = sum(H_HOCH))
ec_status_per_5_km_zone = ec_status_per_5_km_zone %>%
  group_by(GITTER_5km) %>%
  summarize(ec_status = weighted.mean(oek_status,w), hh = sum(w))

write_csv(ec_status_per_5_km_zone, "tmp/ec_status_gitter_5_km.csv")
