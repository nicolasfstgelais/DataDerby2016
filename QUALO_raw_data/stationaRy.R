rm(list=ls(all=TRUE))
library(stationaRy)
library(rnoaa)
library('plyr')
library('rnoaa')
rnoaa::isd_stations_search(lat = 38.4, lon = -123, radius = 250)

#won't work in Rstudio

#You'll need an API key to use this package (essentially a password). 
#Go to the NCDC website (http://www.ncdc.noaa.gov/cdo-web/token) to get
#one. You can't use this package without an API key.

#insert your api key 
options(noaakey = "QWlWJuQsHjCHRPUfYuOybgyeADcltdVJ")

# see wheather station in a given geographical area
station=get_isd_stations(lower_lat = 45.000,upper_lat = 46.000,lower_lon = -74.000,upper_lon = -73.000)

isd_stations_search(lat = 45, lon = -74, radius = 100)

# select "716270-99999" for Trudeau station
# select "713710-99999" for St-Hubert station
data=get_isd_station_data("716270-99999", 2000,2015, local_tz = TRUE)
?get_isd_station_data

noaa_locs(locationcategoryid = "Montreal")



setwd("C://Users//Nicolas//Documents//GitHub//DataDerby2016//QUALO_raw_data")
write.csv(data,"trudeau_weather_gen.csv")


## lat, long, radius
isd_stations_search(lat = 38.4, lon = -123, radius = 250)
ncdc(datasetid='GHCND', stationid='716270-99999', startdate = '2013-10-01',
enddate = '2013-12-01')

getwd()

loc=ncdc_locs(locationcategoryid = "CITY", sortfield = "name")
ncdc_locs(locationcategoryid='CA', limit=52)
loc=ncdc_locs(limit=1000)

# montreal - CITY:CA000005
loc$data
write.csv(loc$data,"write.csv")

noaa_stations(datasetid = "GHCND",
              stationid = "CITY:CA000005")

head(loc)
