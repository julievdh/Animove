#########
# Animove recursion exercise
# Chloe Bracis and Thomas Mueller
# chloe.bracis@gmail.com



#install recurse package

#clean working directory
rm(list =ls())

#load libraries
library(recurse)
library(move)
library(ggmap)
library(RgoogleMaps)
library(raster)
library(scales)
library(lubridate)
library(rworldmap)
library(png)
library(cluster)


############################
#two function for color palettes

#get continuous palette from blue (low values) to red (high values)
getContinuousPalette = function(n, alpha = 1)
{
  cols = alpha(brewer_pal(palette = "RdYlBu", direction = -1)(9), alpha)
  return( gradient_n_pal(cols)(seq(0, 1, length = n)) )
}

######################
#discrete sequential palette, e.g. for one color per day 
getDiscreteSequentialPalette = function(n, alpha = 1)
{
  return(alpha(rainbow(n), alpha))
}
####################################

############################
# read movebank elephant "Habiba" from Wall et al. 2014

Path = "./scripts/Recursion/"

elephants =  split(move(paste0(Path,"Elliptical Time-Density Model (Wall et al. 2014) African Elephant Dataset (Source-Save the Elephants).csv")))
habiba = elephants$Habiba
plot(habiba)



#summarize data
summary(habiba)
range(habiba$study.local.timestamp)
diff(range(habiba$utm.easting))
diff(range(habiba$utm.northing))
##################################


#Color code data day by day
uniqueDays = unique(day(habiba$timestamp))
dayIdx = sapply(day(habiba$timestamp), function(x) which(x == uniqueDays))
move::plot(habiba, 
           col = getDiscreteSequentialPalette(length(uniqueDays))[dayIdx])
# -> it appears that the looping behavior occurs at night
#####################################



#####################################
#Examine the Revistation data
######################

# project to a equidistant projection (move's spTransform uses aeqd by default)
# don't want to do this with latlong because length of a degree changes with latitude!
habibaAEQD = spTransform(habiba, center = TRUE)

# calculate recursions for a 500m Radius
Radius = 500# 500m 

habibavisits = getRecursions(habibaAEQD , radius = Radius) 
names(habibavisits)
#habibavisits = getRecursions(habiba.df , radius = Radius) 

# examine output object
str(habibavisits)
habibavisits$revisits

#plot revisits
hist(habibavisits$revisits, breaks = 10, col = "darkgrey", border = NA, main = "", xlab = "Revisits (radius 500m)")

#plot revisits on trajectory
plot(habibavisits, habibaAEQD, alpha = 1, legendPos = c(-3000, 4000))
# draw circle as a reference for scale
drawCircle(max(habibaAEQD@coords[,1]) - Radius, min(habibaAEQD@coords[,2]) + Radius, radius = Radius)
######################################


######################################
# plot in google maps
# note: google maps expects latlong coordinates
habiba.map.df = as(habiba,'data.frame')
map.habiba <- qmap(bbox(extent(habiba)), zoom = 13, maptype = 'hybrid', legend="topright")
print(map.habiba + 
        geom_path(data = habiba.map.df, aes(x = coords.x1, y = coords.x2), color = "white", size = 0.3) + 
        geom_point(data = habiba.map.df, aes(x = coords.x1, y = coords.x2), 
                   color = getContinuousPalette(max(habibavisits$revisits), 0.5)[habibavisits$revisits]))



###############################################################
# there is more than just the revisits in the recursion object:
# accessing the data frame for the recursion statistics
habibavisits$revisitStats[1:15,]

#################################
# examine first passage time
# the first visits passes through the center of the circle, thus representing a first passage time
# the first visit at each location equals the first passage time
# this only works in teh recurse package if you use single individuals, not available for multiple individuals
habibavisits$firstPassageTime = habibavisits$revisitStats$timeInside[habibavisits$revisitStats$visitIdx==1]


hist(as.numeric(habibavisits$firstPassageTime), breaks = 20, col = "darkgrey", border = NA, main = "", xlab = "First passage (hrs)")
# the histogram indicates a bimodal distribution potentially indicating two different behaviors where


#plot locations with first passage larger or smaller 6 hrs on map
cutOff = 6
print(map.habiba + 
        geom_path(data = habiba.map.df, aes(x = coords.x1, y = coords.x2),color = "white", size = 0.3) + 
        geom_point(data = habiba.map.df, aes(x = coords.x1, y = coords.x2), 
                   color=alpha(ifelse(habibavisits$firstPassageTime >cutOff, "blue", "grey"),.5)))


#it seems there is a different behavior at sections of the loop, maybe related to daytime if loops are daily
boxplot(as.numeric(habibavisits$firstPassageTime) ~ hour(habiba$timestamp), 
		outline = FALSE, col = "grey", xlab = "Daytime (hrs)", ylab = "First passage (hrs)")


########################



########################
# examine residence/utilization time, the sum of all visits around a focal point
head(tapply(habibavisits$revisitStats$timeInside,habibavisits$revisitStats$coordIdx,sum))
# or easier use the pre-calculated output in the object
head(habibavisits$residenceTime)


hist(as.numeric(habibavisits$residenceTime), breaks = 20, col = "darkgrey", border = NA, main = "", xlab= "Utilization time (hrs)")
# there seems to be a bimodal distribution too, separated at about 20 hrs total visit time
cutOff = 20
print(map.habiba + 
        geom_path(data = habiba.map.df, aes(x = coords.x1, y = coords.x2), color = "white", size = 0.3) + 
        geom_point(data = habiba.map.df, aes(x = coords.x1, y = coords.x2), 
                   color = alpha(ifelse(habibavisits$residenceTime > cutOff, "blue", "grey"), 0.5)))
# these areas are very different than the areas with longer first passage, maybe related to resources?

#look at NDVI at these areas to explore relation to resources
# note: the NDVI raster is in UTM coordinates
ndvi = raster(paste0(Path,"MOD13Q1.A2014033.250m_16_days_NDVI.tif")) / 10000
habiba.ndvi = data.frame(habiba[,c("utm.easting", "utm.northing", "timestamp")] )[,1:3] # ignore extra cols move package adds
habiba.ndvi$ndvi = extract(ndvi, habiba.ndvi[,c("utm.easting", "utm.northing")])


# plot NDVI with data
plot(ndvi)
lines(habiba.ndvi$utm.easting, habiba.ndvi$utm.northing, col = "white")
points(habiba.ndvi$utm.easting, habiba.ndvi$utm.northing, 
       col = alpha(ifelse(habibavisits$residenceTime > cutOff, "blue", "grey"), 0.3), pch = 16, cex = 0.7)
drawCircle(max(habiba.ndvi$utm.easting) - Radius, min(habiba.ndvi$utm.northing) + Radius, radius = Radius)

# check if there is any difference in NDVI between residence time categories
cutOff = 20
graphics::boxplot(habiba.ndvi$ndvi ~ habibavisits$residenceTime > cutOff, 
				  outline = FALSE, col = "grey", notch = FALSE, 
				  xlab = paste0("Utilization > ", cutOff," hrs"), ylab = "NDVI")



# check if there is any difference in NDVI between times of day
graphics::boxplot(habiba.ndvi$ndvi ~ hour(habiba$timestamp), 
                  outline = FALSE, col = "grey", xlab = "Time of day (hrs)", ylab = "NDVI")



# look at revisitation
hist(as.numeric(habibavisits$revisitStats$timeSinceLastVisit / 24), freq = TRUE, 
     xlab = "Time since last visit (days)" , col = "darkgrey", border = NA, main = "")

returnsAfterOneWeek = as.vector(na.omit(habibavisits$revisitStats$coordIdx[as.numeric(habibavisits$revisitStats$timeSinceLastVisit / 24) > 7]))

print(map.habiba + 
        geom_path(data = habiba.map.df, aes(x = coords.x1, y = coords.x2), color = "white", size = 0.3) + 
        geom_point(data = habiba.map.df[returnsAfterOneWeek, ], aes(x = coords.x1, y = coords.x2), 
                   color = alpha("blue", 0.2)))


# look at revisitation times at shorter time intervals
hist(as.numeric(habibavisits$revisitStats$timeSinceLastVisit / 24), freq = TRUE, 
   xlab = "Time since last visit (days)" , xlim = c(0, 3), ylim = c(0, 400), 
   breaks = 70, col = "darkgrey", border = NA, main = "")
# there seems to be a hint for periodivity - possible suggests periodogram analyses


# what if we pick a smaller radius?
Radius = 100 # 100m 

habibavisits = getRecursions(habibaAEQD, radius = Radius) 

hist(habibavisits$revisits, breaks = 10, col = "darkgrey", border = NA, main = "", xlab = "Revisits (50m radius)")

plot(habibavisits, habibaAEQD, alpha = 1, legendPos = c(-3000, 4000))
drawCircle(max(habibaAEQD@coords[,1]) - Radius, min(habibaAEQD@coords[,2]) + Radius, radius = Radius)


cutOff = 7
graphics::boxplot(habiba.ndvi$ndvi ~ habibavisits$revisits > cutOff, 
                  outline = FALSE, col = "grey", notch = FALSE, 
                  xlab = paste0("Revisitation > ", cutOff), ylab = "NDVI")



#Examine Leo the Vulture
#---------------------------------------------------------------
# Initial data visualization
#---------------------------------------------------------------

# leo
leo = move(x = paste0(Path, "Leo-65545.csv"))


# plot leo on a map
leomap = getMap(resolution = "low")
plot(leomap, xlim = range(leo@coords[,1]), ylim = range(leo@coords[,2]), axes = TRUE)
points(leo@coords[,1], leo@coords[,2], col = "red", pch = ".")

#select summering area
leo = leo[leo@coords[,1] < -105 & leo@coords[,2] > 52,]

summary(leo)
plot(leo)

#---------------------------------------------------------------
# Calculate revisits
#---------------------------------------------------------------

# project leo to utm, but keep leoGeo for lat-long
leoGeo = leo
leo = spTransform(leo, CRS = "+proj=utm +zone=13 +datum=WGS84")

# example for calling recurse with data frame instead of move object
# create a data frame with the x, y, time, and id (this time we are using the UTM projection)
leo.df = as.data.frame(leo@coords)
leo.df$time = leo@timestamps
leo.df$id = "leo"
leovisit50 = getRecursions(leo.df, 50) # uses 50m radius

#---------------------------------------------------------------
# Analyze revisits
#---------------------------------------------------------------

#revisits
hist(leovisit50$revisits, breaks = 100, main = "", xlab = "Revisits (radius 50m)")
hist(leovisit50$revisits, breaks = 100, xlim = c(0,100), main = "", xlab = "Revisits (radius 50m)")


# start with 1 to see all data, then change to 10 to see commonly visited places, 
# then 100 to see frequently visited places


revisitThreshold = 75
leoGeo.map.df = as(leoGeo,'data.frame')
leoGeo.map.df.subset = leoGeo.map.df[leovisit50$revisits > revisitThreshold,]
revistSubset = leovisit50$revisits[leovisit50$revisits > revisitThreshold]  

# set zoom = 8 to see entire breeding area or zoom = 13 to see frequently visited places
map.leoGeo = qmap(bbox(extent(leoGeo[leovisit50$revisits > revisitThreshold,])), 
				  zoom = 13, maptype = "road", legend = "topright")
print(map.leoGeo + 
        geom_point(data = leoGeo.map.df, aes(x = location.long, y = location.lat), 
        		   size = 0.5, color = alpha(1, 0.5))
      + geom_point(data = leoGeo.map.df.subset, aes(x = location.long, y = location.lat), 
      			 size = 0.5, color = "red"))


#---------------------------------------------------------------
# Analyze revisits for revisitThreshold = 75
#---------------------------------------------------------------


# assign location to each of the 5 clusters
revisitThreshold = 75
leoGeo.map.df = as(leoGeo,'data.frame')
leoGeo.map.df.subset = leoGeo.map.df[leovisit50$revisits > revisitThreshold,]
clusters = fanny(leoGeo.map.df.subset[,c("location.long", "location.lat")], k = 5)
leoGeo.map.df.subset$cluster = as.factor(clusters$clustering)

# save map with clusters colored
siteCols = rainbow(max(clusters$clustering)) # one color for each cluster
png(paste0(Path,"map.png"), width = 1800, height =1800)
# use map.leoGeo with zoom = 13
print(map.leoGeo 
      + geom_point(data = leoGeo.map.df.subset, aes(x = location.long, y = location.lat),
                   size = 30, color = siteCols[clusters$clustering]))
dev.off()

# plot map and histograms for visit times to each cluster
map = readPNG(paste0(Path,"map.png"))

pdf(paste0(Path,"leo.pdf"), width = 9, height = 6)
par(mfrow = c(2, 3), mar = c(2, 2, 2, 2))

# map
plot(1:2, type='n',axes = F, xlab = "", ylab = "")
lim <- par()
rasterImage(map,lim$usr[1], lim$usr[3], lim$usr[2], lim$usr[4], asp = 1)

# histograms of visit time for each cluster
for(i in 1:5)
{	
  hist(leoGeo.map.df.subset$timestamp[leoGeo.map.df.subset$cluster ==i], 
       breaks = 100, xlim = range(leoGeo.map.df.subset$timestamp), col = siteCols[i],
       main = "", xlab = "Year", freq = T, ylim = c(0,200))
}
dev.off()


















