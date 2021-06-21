library(dplyr)
library(readr)
library(ggplot2)

MiD2017_Haushalte <- read_delim("c:/models/midprocessing2017/data/B1_Standard-Datensatzpaket/CSV/MiD2017_Haushalte.csv", 
                                ";", escape_double = FALSE, locale = locale(decimal_mark = ","), 
                                trim_ws = TRUE)

summary_germany = MiD2017_Haushalte %>% group_by(oek_status) %>% summarise(n = sum(H_HOCH))
summary_germany = summary_germany %>% mutate(total = sum(n)) %>% mutate(percentage = n / total * 100)
summary_germany

ggplot(summary_germany, aes(x = oek_status, y = percentage)) +
  geom_bar(stat = "identity", position = "dodge") 


summary_land = MiD2017_Haushalte %>% group_by(BLAND, oek_status) %>% summarise(n = sum(H_HOCH))
summary_land = summary_land %>% mutate(total = sum(n)) %>% mutate(percentage = n / total * 100)

ggplot(summary_land, aes(x = oek_status, y = percentage, group = BLAND, color = as.factor(BLAND))) +
  geom_line(stat = "identity", position = "dodge", size = 1) 



MiD2017_Haushalte %>% filter(H_EINK < 9001) %>% ggplot(aes(x = H_EINK, 
                                                           group = BLAND, color = as.factor(BLAND))) +
  stat_ecdf() 
