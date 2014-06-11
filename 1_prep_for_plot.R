library(sqldf)
library(maptools)
library(plyr)
library(reshape)

#------load cleaned data--------
divvy.db <- dbConnect(SQLite(), dbname='divvy.db')
trips.data <- dbGetQuery(divvy.db, 'select * from trip')

# read in shapefile for divvy station locations
stations.shp <- readShapePoints('Divvy_Stations_2013.shp')

#------prepare data for plotting----------
# aggregate departure and destination station data by 
# weekday/weekend and hour of day
trips.from <- ddply(trips.data, c('from_station_name', 'is_weekday', 'hr_of_day_start'), 
                    summarize, 
                    n_trips = length(trip_id))

trips.to <- ddply(trips.data, c('to_station_name', 'is_weekday', 'hr_of_day_stop'),
                  summarize,
                  n_trips = length(trip_id))

# reshape data into wide format
from.cast <- cast(trips.from, from_station_name ~ is_weekday + hr_of_day_start, value='n_trips')
to.cast <- cast(trips.to, to_station_name ~ is_weekday + hr_of_day_stop, value='n_trips')

# replace NAs with 0
from.cast[is.na(from.cast)] <- 0
to.cast[is.na(to.cast)] <- 0

# merge with station.shp data
stations.shp@data <- merge(stations.shp@data, from.cast, 
                           by.x='name', by.y='from_station_name')
names(stations.shp@data)[6:ncol(stations.shp@data)] <- sapply(
  names(stations.shp@data)[6:ncol(stations.shp@data)],
  paste, '_fr', sep='')

stations.shp@data <- merge(stations.shp@data, to.cast, 
                           by.x='name', by.y='to_station_name')
names(stations.shp@data)[54:ncol(stations.shp@data)] <- sapply(
  names(stations.shp@data)[54:ncol(stations.shp@data)],
  paste, '_to', sep='')

# save as new shp file
writePointsShape(stations.shp, 'divvy_stations_2013_byhr_bywkday')

