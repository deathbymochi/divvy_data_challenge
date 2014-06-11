library(sqldf)

#-------load data, do basic cleaning--------
stations.data <- read.csv('Divvy_Stations_2013.csv', stringsAsFactors=F)

trips.data <- read.csv('Divvy_Trips_2013.csv', stringsAsFactors=F)

# remove commas from trip duration to convert to integer
trips.data$tripduration <- sapply(trips.data$tripduration, gsub, pattern=',',
                                  replacement='')
trips.data$tripduration <- as.integer(trips.data$tripduration)

trips.data$from_station_id <- as.integer(trips.data$from_station_id)
trips.data$to_station_id <- as.integer(trips.data$to_station_id)

# 'Congress Pkwy & Ogden Ave' doesn't have a valid station id so add that:
# assign as 500
trips.data$from_station_id[
  trips.data$from_station_name=='Congress Pkwy & Ogden Ave'] <- 500
trips.data$to_station_id[
  trips.data$to_station_name=='Congress Pkwy & Ogden Ave'] <- 500

# change start time and stop time to POSIXct date time format
trips.data$starttime <- sapply(trips.data$starttime, function(x) {
  return(as.character(as.POSIXct(x, format='%m/%d/%Y %H:%M')))
  })

trips.data$stoptime <- sapply(trips.data$stoptime, function(x) {
  return(as.character(as.POSIXct(x, format='%m/%d/%Y %H:%M')))
  })

#------make db to store cleaned data, add date-time conversions-------
divvy.db <- dbConnect(SQLite(), dbname='divvy.db')

dbWriteTable(conn=divvy.db, name='Trip', value=trips.data,
             row.names=F)
dbWriteTable(conn=divvy.db, name='Station', value=stations.data,
             row.names=F)

# add day of week column to Trip table (0-6, 0=Sunday)
dbSendQuery(divvy.db, paste('alter table trip',
                            'add day_of_wk varchar'))
dbSendQuery(divvy.db, paste('update trip',
                            'set day_of_wk = strftime("%w", starttime)'))

# add weekday indicator column to Trip table
dbSendQuery(divvy.db, paste('alter table trip',
                            'add is_weekday int'))
dbSendQuery(divvy.db, paste('update trip',
                            'set is_weekday = case when',
                            "day_of_wk in ('1', '2', '3', '4', '5')", 
                            'then 1 else 0 end'))

# add hour of day columns to Trip table
dbSendQuery(divvy.db, paste('alter table trip',
                            'add hr_of_day_start int'))
dbSendQuery(divvy.db, paste('update trip',
                            'set hr_of_day_start = strftime("%H", starttime)'))
dbSendQuery(divvy.db, paste('alter table trip',
                            'add hr_of_day_stop int'))
dbSendQuery(divvy.db, paste('update trip',
                            'set hr_of_day_stop = strftime("%H", stoptime)'))

dbDisconnect(divvy.db)
