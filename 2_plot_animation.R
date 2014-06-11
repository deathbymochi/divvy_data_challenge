library(maptools)
library(ggmap)
library(ggplot2)
library(animation)
library(gridExtra)

#-------load prepped data--------
# read in prepped and raw stations shapefiles
stations.shp.prep <- readShapePoints('divvy_stations_2013_byhr_bywkday.shp')

#------build base chicago map----------
# use bbox from divvy stations as boundary
b <- stations.shp@bbox
b[1, ] <- (b[1, ] - mean(b[1, ])) * 1.1 + mean(b[1, ]) # longitude
b[2, ] <- (b[2, ] - mean(b[2, ])) * 0.8 + mean(b[2, ]) # latitude
chicago1 <- get_map(location=b, color='bw', source='stamen',
                   maptype='toner')

#----------set up heat map plot & animation--------------
# blue-green-yellow gradient
grad1 <- c('#112356', '#009876', '#00c54b', 
           '#1afb00', '#affd3f', '#e4ff72')

# blue-purple-pink gradient
grad2 <- c('#112356', '#AC00BC', '#d80098',
           '#ff029e', '#ff71af', '#ffb6b6')

# create function for plotting divvy station heatmap for specified column
# in stations.shp@data, using specified gradient of n colors
plotter <- function(colname, gradient, limits, title.paren.text=NULL) {
  # repeat station coordinates by count of trips to simulate density
  lon.list <- mapply(rep, stations.shp.prep@data$longitude, each=(stations.shp.prep@data[[colname]]))
  lat.list <- mapply(rep, stations.shp.prep@data$latitude, each=(stations.shp.prep@data[[colname]]))
  unlisted.count <- as.data.frame(cbind(unlist(lon.list), unlist(lat.list)))
  names(unlisted.count) <- c('lon', 'lat')
  
  current.hr <- gsub('([[:alpha:]]\\d_)|(_[[:alpha:]]+)', '', colname)
  if (as.integer(current.hr) == 0) {
    current.hr <- '12 AM'
  } else if (as.integer(current.hr) == 12) {
    current.hr <- '12 PM'
  } else if (as.integer(current.hr) >= 1  && 
               as.integer(current.hr) < 12) {
    current.hr <- paste(current.hr, 'AM')
  } else {
    current.hr <- paste(as.integer(current.hr) - 12, 'PM')
  }

  ggmap(chicago) + geom_point(data=stations.shp.prep@data, 
                              aes(x=longitude, y=latitude), colour='#0099cc') +
    coord_equal() + stat_density2d(data=unlisted.count, 
                                   aes(x=lon, y=lat, fill=..level..), 
                                   geom='polygon', alpha=0.2) +
    scale_fill_gradientn("total trips", limits=limits, 
                         colours=gradient) +
    ggtitle(bquote(paste("Divvy Usage at ", .(current.hr), ' ', .(title.paren.text)))) + 
    theme(axis.title.x=element_blank(), axis.text.x=element_blank(),
          axis.title.y=element_blank(), axis.text.y=element_blank(),
          axis.ticks=element_blank(), axis.line=element_blank())
}

# create function that loops through plotting images 
# that compose the animation
plot.looper <- function(colname.list, gradient, limits, title.paren.text=NULL) {
  lapply(colname.list, function(x) {
    print(plotter(x, gradient, limits, title.paren.text))
    ani.pause()
  })
}

fr.colname.list <- names(stations.shp.prep)[6:53]
to.colname.list <- names(stations.shp.prep)[54:101]

# if on windows, need to install imagemagick first before this works:
saveGIF(plot.looper(to.colname.list[1:24], grad2, c(100, 1000)), movie.name='to_wkend.gif',
        outdir=getwd())
saveGIF(plot.looper(to.colname.list[25:48], grad2, c(100, 5000)), movie.name='to_wkday.gif',
        outdir=getwd())
saveGIF(plot.looper(fr.colname.list[1:24], grad1, c(100, 2000)), movie.name='fr_wkend.gif',
        outdir=getwd())
saveGIF(plot.looper(fr.colname.list[25:48], grad1, c(100, 7000)), movie.name='fr_wkday.gif',
        outdir=getwd())
