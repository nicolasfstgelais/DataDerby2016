rm(list=ls(all=TRUE))
library(stationaRy)

# see wheather station in a given geographical area
station=get_isd_stations(lower_lat = 45.000,upper_lat = 46.000,lower_lon = -74.000,upper_lon = -73.000)

# select "716270-99999" for Trudeau station
# select "713710-99999" for St-Hubert station
data=get_isd_station_data("716270-99999", 2000,2015, local_tz = TRUE)
?get_isd_station_data

setwd("C://Users//Nicolas//Documents//GitHub//DataDerby2016//QUALO_raw_data")
write.csv(data,"trudeau_weather_gen.csv")
