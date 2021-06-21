pacman::p_load(ggplot2, dplyr, tidyr, readr)

##script made by Alona, to convert the SP to 2-files and scale it down. 

options(scipen=999)

my_folder = "c:/models/mito/germany/microData/"

pp_2011 <- read_csv(paste(my_folder, "pp_2011.csv", sep = ""))
pp_new <- pp_2011[,c(1:9, 11)] # exclude column 10 which is disability
jj_2011 <- read_csv(paste(my_folder, "jj_2011.csv", sep = ""))
jj_new <- jj_2011[,c(1:3,5,6)] # exclude column 4 which is job type - do not need it duplicated

colnames(jj_new)[colnames(jj_new)=="id"] <- "JobId"
colnames(jj_new)[colnames(jj_new)=="personId"] <- "id"
colnames(jj_new)[colnames(jj_new)=="coordX"] <- "jobCoordX"
colnames(jj_new)[colnames(jj_new)=="coordY"] <- "jobCoordY" ###j -> J

pp_new <- pp_new %>% left_join (jj_new, by = c("id"))

write_csv(pp_new, paste(my_folder, "pp_mito_2011.csv", sep = ""))


hh_2011 <- read_csv(paste(my_folder, "hh_2011.csv", sep = ""))
dd_2011 <- read_csv(paste(my_folder, "dd_2011.csv", sep = ""))
dd_new <- dd_2011[,c(1, 4,5)]
hh_new <- hh_2011 %>% left_join (dd_new, by = c("id"))
hh_new <- hh_new[,c(1,3:7)]

write_csv(hh_new, paste(my_folder, "hh_mito_2011.csv", sep = ""))

## sampling 1 percent
hh_new_1perc <- hh_new %>% sample_frac(.01)
pp_new_1perc <- pp_new %>% filter(hhid %in% hh_new_1perc$id)

pp_new_1perc <- pp_new_1perc %>%
  mutate(JobId = if_else(is.na(JobId), 0, JobId))
pp_new_1perc <- pp_new_1perc %>%
  mutate(zone = if_else(is.na(zone), 0, zone))
pp_new_1perc <- pp_new_1perc %>%
  mutate(jobCoordX = if_else(is.na(jobCoordX), 0, jobCoordX))
pp_new_1perc <- pp_new_1perc %>%
  mutate(jobCoordY = if_else(is.na(jobCoordY), 0, jobCoordY))

write_csv(pp_new_1perc, paste(my_folder, "pp_mito_1_percent_2011.csv", sep = ""))
write_csv(hh_new_1perc, paste(my_folder, "hh_mito_1_percent_2011.csv", sep = ""))

## sampling 5 percent
hh_new_5perc <- hh_new %>% sample_frac(.05)
pp_new_5perc <- pp_new %>% filter(hhid %in% hh_new_5perc$id)

pp_new_5perc <- pp_new_5perc %>%
  mutate(JobId = if_else(is.na(JobId), 0, JobId))
pp_new_5perc <- pp_new_5perc %>%
  mutate(zone = if_else(is.na(zone), 0, zone))
pp_new_5perc <- pp_new_5perc %>%
  mutate(jobCoordX = if_else(is.na(jobCoordX), 0, jobCoordX))
pp_new_5perc <- pp_new_5perc %>%
  mutate(jobCoordY = if_else(is.na(jobCoordY), 0, jobCoordY))

write_csv(pp_new_5perc, paste(my_folder, "pp_mito_5_percent_2011.csv", sep = ""))
write_csv(hh_new_5perc, paste(my_folder, "hh_mito_5_percent_2011.csv", sep = ""))



setwd("Z:/projects/2019/BASt/data/synthetic_popualtion/spGermany_forMITO")
fwrite(pp_1perc_2011_noNA,"pp_1perc_2011_noNA.csv")
jj_2011_old <- read_csv("~/GitHub/germanymodel_SD/microData/1perc_pop/jj_2011_1.csv")
pp_1perc_2011 <- read_csv("~/GitHub/germanymodel_SD/microData/pp_1perc_2011.csv") # 1 - worker, 2 unemployed, 3 student
pp_1perc_2011_workers <- pp_1perc_2011[grepl("1", pp_1perc_2011$occupation),]
pp_1perc_2011_noNA <- read_csv("~/GitHub/germanymodel_SD/microData/pp_1perc_2011_noNA.csv")
## new schoool file
setwd("Z:/projects/2019/BASt/data/synthetic_popualtion/spGermany_forMITO")
hh_new <- read_csv("hh_100perc_2011.csv")
colnames(hh_new)[colnames(hh_new)=="id"] <- "hhid"
coord <- hh_new %>%
  dplyr::group_by(zone) %>%
  dplyr::summarize(count = n(), coordX = sum(coordX), coordY = sum(coordY))
coord$ee_coordX <- coord$coordX / coord$count
coord$ee_coordY <- coord$coordY / coord$count
coord <- coord[,c(1,5,6)]
pp_new <- read_csv("pp_100perc_2011.csv")
ee_new <- pp_new[grepl(3, pp_new$occupation),]
ee_new1 <- ee_new[,c(1:5,10)]
ee_new2 <- merge(ee_new1, hh_new, by = c("hhid"), all.x = TRUE)
#ee_new1 <- ee_new[,c(5, 15,18,19)]
schools <- ee_new2 %>%
  dplyr::group_by(zone) %>%
  dplyr::summarize(capacity = n())
schools <- merge(schools, coord, by = c("zone"), all.x = TRUE)
schools$id <- 1:nrow(schools)
schools$type <- 1
schools$occupancy <- schools$capacity
schools$startTime <- 0
schools$duration <- 0
colnames(schools)[colnames(schools)=="ee_coordX"] <- "coordX"
colnames(schools)[colnames(schools)=="ee_coordY"] <- "coordY"
schools <- schools[,c(5,1,6,2,7,3,4,8,9)]
setwd("Z:/projects/2019/BASt/data/synthetic_popualtion/spGermany_forMITO")
fwrite(schools,"schools_1perc_2011.csv")
