# UCI Cyclocross Calendar

library(ggmap) # for geocoding and plotting 
library(geosphere) # for distance calculations
library(knitr) # for making a nice table

# load data
data <- read.csv("UCI2018.csv",header=FALSE, stringsAsFactors=FALSE, fileEncoding="latin1")
colnames(data) = c("date","Race","Location","Country","Class")

data$cityCTRY <- do.call(paste, c(data[c("Location", "Country")], sep = ", ")) 
  
# get coordinates for names
raceGeo <- geocode(data$cityCTRY[1:4], output='all', messaging=TRUE, override_limit=TRUE)
raceGeoshort <- geocode(data$cityCTRY, output = 'latlon')

#lat <- c()
#lon <- c()

#for (row in 1:length(raceGeo)) {
#lat[row] <- raceGeo[[row]]$results[[1]]$geometry$location$lat
#lon[row] <- raceGeo[[row]]$results[[1]]$geometry$location$lng
#}

#raceLoc <- data.frame(lat = lat, lon = lon)


#make a map
qmplot(lon, lat, data = raceGeoshort, maptype = "watercolor", color = I("red")) + geom_path(color = "red")

## try interactive
library(plotly)
library(ggplot2)

data(canada.cities, package="maps")
p <- ggplot(canada.cities, aes(long, lat)) +
  coord_equal() 
 # geom_point(aes(text=canada.cities$name, size=canada.cities$pop), colour="red", alpha=1/2, name="cities")
p <- ggplotly(p)
