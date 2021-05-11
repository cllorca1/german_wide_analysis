pacman::p_load(readr, dplyr, tidyr, ggplot2)

folder = "C:/models/germanymodel/matsim/externalDemand/ld_wei_1_percent/"

trips = read_csv(paste(folder, "ldTrips_5percent_allDestinations.csv", sep =""))


trips$id = trips$tripId
trips$person = trips$personId
trips$purpose = if_else(trips$tripState == "daytrip", "HBO", "NHBO")

trips = trips %>% filter(tripState != "away")

trips$mode = trips$tripMode
trips$distance = trips$travelDistance_km
trips$departure_time = trips$departureTimeMin
trips$departure_time_return = trips$departureTimeReturnDaytrip

trips$time_auto = trips$travelTimeByMode_h*60
trips$time_train  = trips$travelTimeByMode_h*60
trips$time_tram_metro  = trips$travelTimeByMode_h*60
trips$time_bus  = trips$travelTimeByMode_h*60


trips$originX = if_else(!trips$ReturnOvernightTrip, trips$origX, trips$destX)
trips$originY = if_else(!trips$ReturnOvernightTrip, trips$origY, trips$destY)
trips$destinationX = if_else(!trips$ReturnOvernightTrip, trips$destX, trips$origX)
trips$destinationY = if_else(!trips$ReturnOvernightTrip, trips$destY, trips$origY)

write_csv(trips, quote = F,paste(folder, "ldTrips_5percent_like_sd.csv", sep =""))
