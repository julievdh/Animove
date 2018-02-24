# UCI Cyclocross Calendar

library(ggmap) # for geocoding and plotting 
library(geosphere) # for distance calculations
library(RJSONIO)
library(RCurl)
library(plotly)
library(ggplot2)


# load data
data <- read.csv("UCI2018.csv",header=FALSE, stringsAsFactors=FALSE, fileEncoding="latin1")
colnames(data) = c("date","Race","Location","Country","Class")

data$cityCTRY <- do.call(paste, c(data[c("Location", "Country")], sep = ", ")) 
  
# get coordinates for names

getGeoData <- function(location){
location <- gsub(' ','+',location)
geo_data <- getURL(paste("https://maps.googleapis.com/maps/api/geocode/json?address=",location,"&key=AIzaSyB_3udGBiWkWidY8fPLMJ6VONxrth0UAVs", sep=""))
raw_data_2 <- fromJSON(geo_data)
return(raw_data_2)
}
getGeoData(data$cityCTRY[1])

geocoded <- data.frame(stringsAsFactors = FALSE)
for(i in 1:nrow(data))
{
  
  test <- getGeoData(data$cityCTRY[i])
  data$lon[i] <- as.numeric(test$results[[1]]$geometry$location[2])
  data$lat[i] <- as.numeric(test$results[[1]]$geometry$location[1])
  
}

q <- ggplot(data, aes(lon, lat)) +
  coord_equal() +
  borders("world", colour="gray70", fill="gray90") +
  geom_point(aes(text=paste(Race,date)), colour="red", alpha=1/2, name="UCI 2018")
  #scale_colour_gradient(low = "#132B43", high = "#56B1F7", space = "Lab", na.value = "grey50", guide = "colourbar")

q <- ggplotly(q)
q

