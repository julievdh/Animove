# Load the data sets
setwd("/Users/julievanderhoop/Documents/R/Animove/Animove/")

###############

library(move)
load("./data/swstack.RData")
plot(swstack)

################

library(mapdata)
library(scales) # can deal with overplotting: alpha sets transparency
# map('worldHires', col="grey", fill=T) # this is plotting the whole world
points(t(colMeans(coordinates(swstack))), col=alpha('red',0.5), pch=16)
points(t(colMeans(coordinates(swstack))), col='cyan')
################

(e<-bbox(extent(swstack)*5)) # set the bounding box 
# note here that the brackets around the assignment 
# ensure that the result is also printed to the console
map('worldHires', xlim = e[1, ], ylim = e[2, ])
points(swstack)
lines(swstack)

################

library("ggmap")
library("mapproj")
#coerce move object to a data frame
sw_df <- as(swstack, "data.frame")
#request map data from google
m <- get_map(e, zoom=9, source="google", maptype="terrain")
#plot the map and add the locations 
# separated by individual id
ggmap(m)+geom_path(data=sw_df, aes(x=lon, 
                                     y=lat, 
                                     colour=trackId))

################

# Create gray scale - different individuals have different gray shades
swGray<-gray((nrow(idData(swstack))-1):0/nrow(idData(swstack))) 
# Plot with gray scale
plot(swstack, col=swGray, 
     xlab="Longitude", ylab="Latitude")

################

n.locs(swstack) # should always be reported in papers

################

timeLags <- timeLag(swstack, units='hours') # applies to all individuals in stack

################

timeLagsVec <- unlist(timeLags) # compiles all values across individuals
summary(timeLagsVec)
hist(timeLagsVec, breaks=50, main=NA, xlab="Time lag in hours")
arrows(24.5,587.5,20.7,189.7, length=0.1)
arrows(49.5,587.5,45.7,189.7, length=0.1)

################
# select time lags lower than 1 
hist(timeLagsVec[timeLagsVec<1], breaks="FD", 
     main=NA, xlab="Time lag in hours")
summary(as.factor(round(timeLagsVec, 4)), maxsum=5) # round timelags to four digits and define them as a factor

################
# at what times were data collected? 
ts <- timestamps(swstack)
library('lubridate')
tapply(ts, hour(ts), length) # we have observations from 9 pm to 4 am

################

summary(unlist(distance(swstack))) # look at all the bats, not each individually.
# distances in m based on greater circle distance

################

speeds <- unlist(speed(swstack)) # bats is in GCS, so in m. Speed is calculated in map units per second
summary(speeds) # can see we have a supersonic bat (1100 m/s). Likely have very close timestamps

################
## CAN WE PLOT THE ONES THAT ARE RESULTING IN THE FASTEST SPEEDS? 
speedsRealistic <- speeds[speeds<20] # select speeds less than 20 m/s 
speedVsTimeLag <- data.frame(timeLag=timeLagsVec, 
                             speeds=speeds) 
speedVsTimeLag <- speedVsTimeLag[speedVsTimeLag$timeLag<10 
                                 & speedVsTimeLag$speeds<20,]
plot(speedVsTimeLag$timeLag, speedVsTimeLag$speeds, 
     xlab='Time lag (hours)', ylab='Speed (m/s)', pch=19) # time lag vs speeds to see if reason for errors
hist(speedsRealistic, main=NA, xlab="Speed in m/s", breaks="FD", ylab="Frequency")

################

library(classInt)
# select bat X191
sw193a <- swstack[["sw17_225a"]] # select an individual in a move stack with double brackets
# store speed
v <- speed(sw193a)
# find 49 breaks in the speed vector
my.class <- classIntervals(unlist(v),n=49,style="equal")
# assign colours in 50 shades of grey
my.pal <- findColours(my.class,grey(0:49/49)) # 49 intervals to make 50 colours

# make a data frame that is later used to draw the segments
# with x0, y0, x1, y1 coordinates
segdat <- as.data.frame(cbind(coordinates(sw193a)[-n.locs(sw193a),]
                              , coordinates(sw193a)[-1,])) # shift coordinates by 1 because want to plot from-to. Makes dataframe with x, y, x2, y2 
# add colours to the data frame
segdat$col <- as.vector(my.pal)
# add the speed
segdat$v <- v
# sort the data frame by increasing speed
# this will make sure high speed segments are plotted on top
# of low speed segments
segdat <- segdat[order(segdat$v),]

# change the margins of the plot region
par(mar=c(5,4,4,5))
# create a plot with the appropriate size but no points
plot(sw193a, xlab="Longitude", ylab="Latitude", 
     main="Speed in m/s", asp=1, type="n")
# get the size of the plot region
u <- par("usr")
# draw a rectangle of the size of the plot region and colour it 
# with the colour that corresponds to the median speed colour
rect(u[1], u[3], u[2], u[4], 
     col = segdat$col[length(v[v <= median(v)])]) # interesting to see anomaly better
# now draw the segments
segments(segdat[,1],segdat[,2], segdat[,3], segdat[,4], 
         col=segdat[,5], lwd=3)
# add a legend
plot(raster(nrows=1, ncols=length(v), vals=v), 
     legend.only=TRUE, col=grey(0:49/49), legend.mar=4.5)
# can see that fast speeds are all in a row


#################

direction <- angle(sw193a) 
summary(unlist(direction)) # good - -180 t0 180 --> our data make sense here

############
turnAngles <- unlist(turnAngleGc(sw193a)) # needs to be in geographical coordinate system
hist(unlist(turnAngles), breaks=18, xlab="Turning Angle", main=NA) # lots of 180 turns -- usually turning angle is biased to zero (straight-line movement)

##############

                                     