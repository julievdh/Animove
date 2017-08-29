# Load the data sets
setwd("/Users/julievanderhoop/Documents/R/Animove/Animove/")

###############

library(move)
bats <- move("Parti-colored bat Safi Switzerland.csv")
plot(bats)

################

library(mapdata)
library(scales)
map('worldHires', col="grey", fill=T)
points(t(colMeans(coordinates(bats))), col=alpha('red',0.5), pch=16)
points(t(colMeans(coordinates(bats))), col='cyan')
################

(e<-bbox(extent(bats)*5))
# note here that the brackets around the assignment 
# ensure that the result is also printed to the console
map('worldHires', xlim = e[1, ], ylim = e[2, ])
points(bats)
lines(bats)

################

library("ggmap")
library("mapproj")
#coerce move object to a data frame
bats_df <- as(bats, "data.frame")
#request map data from google
m <- get_map(e, zoom=9, source="google", maptype="terrain")
#plot the map and add the locations 
# separated by individual id
ggmap(m)+geom_path(data=bats_df, aes(x=location.long, 
                                     y=location.lat, 
                                     colour=trackId))

################

load("buffalo_cleaned.Rdata")
# Create gray scale
buffaloGray<-gray((nrow(idData(buffalo))-1):0/nrow(idData(buffalo)))
# Plot with gray scale
plot(buffalo, col=buffaloGray,
     xlab="Longitude", ylab="Latitude")

################

which.max(coordinates(buffalo)[,1])
# drop the point with the largest coordinate values
buffalo <- buffalo[-which.max(coordinates(buffalo)[,1])]
plot(buffalo, col=buffaloGray,
     xlab="Longitude", ylab="Latitude")
save(buffalo, file="buffalos.Rdata")

################

n.locs(buffalo)
n.locs(bats)

################

timeLags <- timeLag(bats, units='hours')

################

timeLagsVec <- unlist(timeLags)
summary(timeLagsVec)
hist(timeLagsVec, breaks=50, main=NA, xlab="Time lag in hours")
arrows(24.5,587.5,20.7,189.7, length=0.1)
arrows(49.5,587.5,45.7,189.7, length=0.1)

################

hist(timeLagsVec[timeLagsVec<1], breaks="FD", 
     main=NA, xlab="Time lag in hours")
summary(as.factor(round(timeLagsVec, 4)), maxsum=5)

################

ts <- timestamps(bats)
library('lubridate')
tapply(ts, hour(ts), length)

################

tapply(ts, list(month(ts),hour(ts)), length)

################

summary(unlist(distance(bats)))

################

speeds <- unlist(speed(bats))
summary(speeds)

################

speedsRealistic <- speeds[speeds<20]
speedVsTimeLag <- data.frame(timeLag=timeLagsVec, 
                             speeds=speeds)
speedVsTimeLag <- speedVsTimeLag[speedVsTimeLag$timeLag<10 
                                 & speedVsTimeLag$speeds<20,]
plot(speedVsTimeLag$timeLag, speedVsTimeLag$speeds, 
     xlab='Time lag (hours)', ylab='Speed (m/s)', pch=19)
hist(speedsRealistic, main=NA, xlab="Speed in m/s", breaks="FD", ylab="Frequency")

################

library(classInt)
# select bat X191
bat191 <- bats[["X191"]]
# store speed
v <- speed(bat191)
# find 49 breaks in the speed vector
my.class <- classIntervals(unlist(v),n=49,style="equal")
# assign colours in 50 shades of grey
my.pal <- findColours(my.class,grey(0:49/49))

# make a data frame that is later used to draw the segments
# with x0, y0, x1, y1 coordinates
segdat <- as.data.frame(cbind(coordinates(bat191)[-n.locs(bat191),]
                              , coordinates(bat191)[-1,]))
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
plot(bat191, xlab="Longitude", ylab="Latitude", 
     main="Speed in m/s", asp=1, type="n")
# get the size of the plot region
u <- par("usr")
# draw a rectangle of the size of the plot region and colour it 
# with the colour that corrensponds to the median speed colour
rect(u[1], u[3], u[2], u[4], 
     col = segdat$col[length(v[v <= median(v)])])
# now draw the segments
segments(segdat[,1],segdat[,2], segdat[,3], segdat[,4], 
         col=segdat[,5], lwd=3)
# add a legend
plot(raster(nrows=1, ncols=length(v), vals=v), 
     legend.only=TRUE, col=grey(0:49/49), legend.mar=4.5)

#################

direction <- angle(bats)
summary(unlist(direction))

############
turnAngles <- unlist(turnAngleGc(bats))
hist(unlist(turnAngles), breaks=18, xlab="Turning Angle", main=NA)

##############

library("lubridate")
# load the data
leroy <- move(system.file("extdata","leroy.csv.gz",package="move"))
# get the time stamps of entries without location
pattrn <- data.frame(time=leroy@dataUnUsedRecords$timestamp, 
                     status='Not Successful')
# add the time stamps of positions obtained
pattrn <- rbind(pattrn, data.frame(time=timestamps(leroy), 
                                   status='Successful'))
# change time to local time zone
pattrn$time<-with_tz(pattrn$time, tz="America/New_York")
# Load ggplot library for plotting
library("ggplot2")
# Plot histogram that is filled out for proportions and is binned per hours
ggplot(pattrn, aes(x=hour(time), fill=status))+
  geom_histogram(binwidth=1, position='fill')+scale_fill_grey()

################

ggplot(pattrn, aes(x=time, fill=status))+
  geom_histogram(binwidth=24*60*60)+scale_fill_grey()

###############

                                     