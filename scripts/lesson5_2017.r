library(maptools)
library(move)
# we define night as the time when the sun at the beginning and end of a segment is 6 degrees below the horizon
Leroy <- move(system.file("extdata","leroy.csv.gz",package="move"))
DayNight <- rep("Day", n.locs(Leroy)-1) # can also use sunriset function as in Intro_Move_2017 script
DayNight[solarpos(Leroy[-n.locs(Leroy)], timestamps(Leroy)[-n.locs(Leroy)])[,2] < -6 & 
           solarpos(Leroy[-1], timestamps(Leroy)[-1])[,2] < -6] <- "Night"
Leroy.burst <- move::burst(x=Leroy, f=DayNight) # bursted move object - separate with categorical variable
plot(Leroy.burst, type="l", col=c("grey", "black"))

###########
# identify corridors: directional, movement in short sequences
Leroy <- spTransform(Leroy, CRS("+proj=longlat"))
LeroyCorr <- corridor(Leroy)
plot(LeroyCorr, type="l", xlab="Longitude", ylab="Latitude", 
     col=c("black", "grey"), lwd=c(1,2))
legend("bottomleft", c("Corridor", "Non-corridor"),
       col=c("black", "grey"), lty=c(1,1), bty="n")

###########
setwd("/Users/julievanderhoop/Documents/R/Animove/Animove/")

###############

library(move)
bats <- move("./data/Parti-colored bat Safi Switzerland.csv")
medianSpeed<-unlist(lapply(speed(bats), median))
timeTracked<-unlist(lapply(timeLag(bats, units='days'), sum))
distanceTracked<-unlist(lapply(distance(bats), sum))
individualData<-data.frame(medianSpeed, timeTracked, distanceTracked)
head(individualData) # some great descriptive statistics of the trajectories to report

###########

summary(bats) # 

########### include reference data - merge sex of individual

bats_ref <- read.csv("./data/Parti-colored bat Safi Switzerland-reference-data.csv", sep=",", as.is=TRUE)
bats_ref <- bats_ref[!is.na(bats_ref$animal.id), c("animal.id", "animal.sex")]
bats_ref$animal.id <- paste("X", bats_ref$animal.id, sep="")
individualData$animal.id <- row.names(individualData)
individualData <- merge(individualData, bats_ref, by="animal.id")

boxplot(log10(I(distanceTracked/timeTracked))~animal.sex, data=individualData, names=c("Females", "Males"), ylab=expression(paste(Log_10, " of cumulative distance in m per day", sep="")))

# t test to compare distance as a function of sex
t.test(log(I((distanceTracked/1000))/timeTracked)~animal.sex, data=individualData)
# what is the function of time tracked? 
mod <- glm(sqrt(distanceTracked)~as.factor(animal.sex)+timeTracked, data=individualData)
plot(mod)
summary(mod) # the longer you track an animal, the further it will go. Have to account for time tracked. 

###########
library(MASS)
boxplot(log(medianSpeed)~animal.sex, data=individualData, names=c("Females", "Males"), ylab="Median speed in m/s")
wilcox.test(medianSpeed~animal.sex, data=individualData) # because non-normal, unequal variance
bc <- boxcox(medianSpeed~as.factor(animal.sex)+timeTracked, data=individualData[-15,])

modII <- glm(I(medianSpeed^bc$x[which.max(bc$y)])~as.factor(animal.sex)+timeTracked, data=individualData[c(-15),])
plot(modII)
summary(modII) # males are significantly faster than females

########### look at effect of sampling resolution in a simulated plot: 
library(scales)
set.seed(7478)
r.track <- as(simm.crw(1:1000, 1, 0.99), "Move") # create a track
# use a Kolomgorov-Smirnov two sample test - compare full track to a track with 1/3 sampling frequency
ks.test(sqrt(speed(r.track)), sqrt(speed(r.track[seq(1,nrow(coordinates(r.track)),3),])))
hist(sqrt(speed(r.track[seq(1,nrow(coordinates(r.track)),3),])), freq=F, xlim=c(0,2), breaks="FD", col=alpha("grey", 0.5), xlab="Square root transformed speed", main=NA)
hist(sqrt(speed(r.track)), freq=F, add=T, breaks="FD", col=alpha("white", 0.5))
wilcox.test(sqrt(speed(r.track)), sqrt(speed(r.track[seq(1,nrow(coordinates(r.track)),3),])))
t.test(sqrt(speed(r.track)), sqrt(speed(r.track[seq(1,nrow(coordinates(r.track)),3),])))
