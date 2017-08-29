# load Leo
Leo <- Leo <- move("Leo-65545.csv.gz")
plot(Leo, type="l")

tapply(timestamps(Leo), list(year(timestamps(Leo)), month(timestamps(Leo))), length)
###########
library(move)
library(lubridate)
library(circular)
library(fields)

Leo <- Leo[year(Leo$timestamp)<2012,]
# initialise a new variable
Leo$cat <- NA
# define the months of North migration
Leo$cat[month(Leo$timestamp) %in% c(4,5)] <- "North migration"
# define breeding
Leo$cat[month(Leo$timestamp) %in% c(6,7,8)] <- "Breeding"
# define South migration
Leo$cat[month(Leo$timestamp) %in% c(9,10)] <- "South migration"
# define Wintering
Leo$cat[month(Leo$timestamp) %in% c(11,12,1,2,3)] <- "Wintering"
# assign NA to segments between two seasons
Leo$cat[which(diff(as.numeric(as.factor(Leo$cat)))!=0)] <- NA

# store the information in a new data frame
azimuth <- data.frame(D=angle(Leo),
                      V=speed(Leo), 
                      Season=Leo$cat[-1])
# remove stationary segments
azimuth <- azimuth[azimuth$V>=2,]
# remove missing values
azimuth <- azimuth[complete.cases(azimuth),]
# define a vector that is used to set the order of plotting
seasons <- c("South migration", "Wintering", "North migration", "Breeding")
# change margins of plot
par(mar=c(1,1,1,1))
# plot all the azimuths
plot(as.circular(azimuth$D, 
                 rotation="clock", 
                 units="degrees", 
                 type="angles", 
                 modulo="asis", 
                 zero=0, 
                 template="geographic"), 
     stack=T, shrink=1.6, pch=16,
     sep=0.05, col="grey")
# loop through seasons and plot a line denisty per season
for(i in 1:length(seasons))
{
  # subset the azimuth
  x <- as.circular(azimuth$D[azimuth$Season==seasons[i]], 
                   rotation="clock", 
                   units="degrees", 
                   type="angles", 
                   modulo="asis", 
                   zero=0, 
                   template="geographic")
  # calculate density and plot as a line
  lines(density(x, bw=180, kernel="vonmises"), 
        lwd=2, lty=i)
  # draw an arrow showing mean and resultant length
  arrows.circular(mean(x), y=rho.circular(x), lwd=2, 
                  length=0.1, lty=i)
}
# add a legend
legend("bottomleft", lty=c(1,2,3,4), seasons, bty="n", cex=0.85)
par(mar=c(5, 4, 4, 2) + 0.1)

###########
library(circular)
azimuth$cols <- "grey3"
azimuth$cols[azimuth$Season=="North migration"] <- "forestgreen"
azimuth$cols[azimuth$Season=="South migration"] <- "firebrick"
azimuth$cols[azimuth$Season=="Breeding"] <- "royalblue"
azimuth <- azimuth[order(azimuth$V), ]
par(mar=c(1, 1, 1, 1) + 0.1)
plot(I(azimuth$V*sin(rad(azimuth$D)))~I(azimuth$V*cos(rad(azimuth$D))),
     asp=1, xaxt="n", yaxt="n", type="n",
     xlim=c(-40, 40), ylim=c(-40, 40), bty="n",
     xlab=NA, ylab=NA)
segments(0,-31,0,31)
segments(-31,0,31,0)
points(I(azimuth$V*sin(rad(azimuth$D)))~I(azimuth$V*cos(rad(azimuth$D))), pch=16,
       col=alpha(azimuth$cols, 0.3), cex=0.5)
points(I(azimuth$V*sin(rad(azimuth$D)))~I(azimuth$V*cos(rad(azimuth$D))),
       col=alpha(azimuth$cols, 0.5), cex=0.5) 
for(i in rep(seq(0, 31, length.out=5)))
{
  lines(I(rep(i, 1000)*sin(seq(0,2*pi,length.out=1000)))~ 
          I(rep(i, 1000)*cos(seq(0,2*pi,length.out=1000))), lty=2, col="grey")
}
for(i in rep(seq(0, 31, length.out=5)))
{
  rect(-1.33, i-1.33, 1.33, i+1.33, col=alpha("white",0.75), border=NA)
  text(0, i, as.character(signif(i,2)), cex=0.5)
}

text(42, 40, "Ground speed", pos=2, cex=1)
text(42, 35, substitute(paste("[", ms^-1, "]", sep="")), pos=2, cex=0.75)
text(0, -40, "S", cex=1)
text(-40, 0, "W", cex=1)
text(0, 40, "N", cex=1)
text(40, 0, "E", cex=1)

par(mar=c(5, 4, 4, 2) + 0.1)



###########
par(list(mfrow=c(2,2)))
for(i in c("Breeding", "South migration", "Wintering", "North migration"))
{
  windrose(x=as.circular(azimuth$D[azimuth$Season==i], 
                         rotation="clock", 
                         units="degrees", 
                         type="angles", 
                         modulo="asis", 
                         zero=0, 
                         template="geographic"), 
           y=azimuth$V[azimuth$Season==i],
           main=i, plot.mids=T, cir.ind = 0.2, mids.size=1,
           increment=5, bins=36, fill.col=grey(seq(1,0, length.out=6)), 
           shrink=1)
}
par(mfrow=c(1,1))
###########
turn <- data.frame(angle=turnAngleGc(Leo))

v <- speed(Leo)
turn$Vm <- rowMeans(cbind(as.numeric(v)[-1],
                          as.numeric(v)[-length(as.numeric(v))]))
segSeason <- rowMeans(cbind(as.numeric(as.factor(Leo$cat))[-1],
                            as.numeric(as.factor(Leo$cat))[-length(Leo$cat)]))
turn$season <- rowMeans(cbind(segSeason[-1],
                              segSeason[-length(segSeason)]))
turn$season <- factor(turn$season, labels=levels(as.factor(Leo$cat)))
turn <- turn[turn$Vm>2,]
turn <- turn[!is.na(turn$season),]
angle360 <- turn$angle[!is.na(turn$angle)]
angle360[angle360<0] <- angle360[angle360<0]+360
par(bty="n")
windrose(as.circular(angle360, 
                     rotation="clock", 
                     units="degrees", 
                     type="direction", 
                     modulo="asis", 
                     zero=0, 
                     template="none"), 
         y=turn$Vm[!is.na(turn$angle)],
         plot.mids=T, cir.ind = 0.2, mids.size=1,
         increment=5, bins=72, fill.col=grey(seq(1,0, length.out=6)),
         main="Turning angle and speed", tcl.text=-0.07)
###########
library(scales)
par(mfrow=c(2,2))
for(i in unique(turn$season)){
  plot(Vm~angle, data=turn[turn$season==i,], 
       xlab="Turning angle", ylab="Speed in m/s",
       bty="n", pch=16, ylim=c(0,max(turn$Vm)),
       col=alpha("black", 0.3), main=i)
}
par(mfrow=c(1,1))

###########

#### Movement process and effects on path metrics
library(adehabitatLT)
library(move)
sets <- sort(rep((0.8 + log10(c(seq(1,100, length.out=10)))/10)[1:9],500))
rCRW <- lapply(lapply(sets, simm.crw, date=1:1000, h=1), as, "Move")
rNSD <- unlist(lapply(lapply(lapply(rCRW, coordinates), spDistsN1, pt=c(0,0)), "^", 2))

mNSD <- tapply(rNSD, list(sort(rep(sets,1000)) , rep(1:1000, length(sets))) , mean)
par(mar=c(5, 4, 4, 4) + 0.1)
plot(0,0, type="n", xlim=c(0,1300), ylim=c(0, max(mNSD)),
     bty="n", xlab="Step", ylab="Net square distance", xaxt="n")
axis(1, at=c(0,200,400,600,800,1000))
test <- apply(as.matrix(mNSD), 1, lines, x=1:1000)
text(cbind(rep(c(1250, 1100), length.out=length(row.names(mNSD))), mNSD[,ncol(mNSD)]), 
     paste("r=", as.character(round(as.numeric(row.names(mNSD)),3)),sep=""), cex=0.5)

###########
layout(matrix(c(1,1,2,3), ncol=2, byrow=T))
plot(Leo$timestamp, 
     (spDistsN1(coordinates(Leo),
                coordinates(Leo)[1,]))^2, type="l",
     xlab="Time", ylab="Net square distance", main="All data")

leo <- Leo[which(Leo$cat=="Breeding" & year(Leo$timestamp)==2008),]
plot(leo$timestamp,
     (spDistsN1(coordinates(leo),
                coordinates(Leo)[1,])^2), type="l",
     xlab="Time", ylab="Net square distance", main="Breeding 2008")

leo <- Leo[which(Leo$cat=="Wintering"),]
leo <- Leo[c(which(year(Leo$timestamp)==2008 & 
                     month(Leo$timestamp)%in%c(11,12)),
             which(year(Leo$timestamp)==2009 & 
                     month(Leo$timestamp)%in%c(1,2,3))),]
plot(leo$timestamp,
     (spDistsN1(coordinates(leo),
                coordinates(Leo)[1,])^2), type="l",
     xlab="Time", ylab="Net square distance", main="Winter 2008/2009")
layout(matrix(c(1), ncol=1, byrow=T))

###########
Leo <- spTransform(Leo, center=T)

###########
# First passage time
###########
library(adehabitatLT)
fptLeo <- fpt(as(Leo, "ltraj"), r=10^seq(3, 6, length.out=150), units="days")
meanFPT <- colMeans(fptLeo[[1]], na.rm=T)
radiiFPT <- attributes(fptLeo)$radii
plot(meanFPT~radiiFPT,
     type="l", lwd=2, xlab="Radii in meters",
     ylab="First passage time in days", log="xy")
###########
vars <- varlogfpt(fptLeo, graph=F)
plot(as.numeric(vars)~radiiFPT,
     type="l", lwd=1, lty=2, 
     log="x", ylab="Variance of log first passage time", 
     xlab="Radius in meters")
###########
plot(log10(meanFPT)~log10(radiiFPT),
     type="l", lwd=2, xlab="Log radii in meters",
     ylab="Log first passage time in days")
lm1 <- lm(log10(meanFPT[1:which.min(vars[1:which.max(vars)])])~
            log10(radiiFPT[1:which.min(vars[1:which.max(vars)])]))
lm2 <- lm(log10(meanFPT[which.min(vars[1:which.max(vars)]):which.max(vars)])~
            log10(radiiFPT[which.min(vars[1:which.max(vars)]):which.max(vars)]))

abline(lm1, lty=2)
abline(lm2, lty=3)
text(4, 0.1, paste(signif(summary(lm1)$coefficients[2,1], 2), 
                   "±", 
                   signif(summary(lm1)$coefficients[2,2], 2)), pos=4, cex=0.75)
text(4, 1, paste(signif(summary(lm2)$coefficients[2,1], 2), 
                 "±", 
                 signif(summary(lm2)$coefficients[2,2], 2)), pos=4, cex=0.75)
###########
plot(as.numeric(vars)~radiiFPT,
     type="l", lwd=1, lty=2, 
     ylab="Variance of log first passage time", 
     xlab="Radius in meters", log="x")
breaks <- which(diff(floor(diff(as.numeric(vars))))==-1)+1
abline(v=radiiFPT[breaks])
###########
plot(log10(meanFPT)~log10(radiiFPT),
     type="n", lwd=4, xlab="Log radii in meters",
     ylab="Log first passage time in days")

lm1 <- lm(log10(meanFPT[1:breaks[1]])~
            log10(radiiFPT[1:breaks[1]]))
lm2 <- lm(log10(meanFPT[breaks[1]:breaks[2]])~
            log10(radiiFPT[breaks[1]:breaks[2]]))
lm3 <- lm(log10(meanFPT[breaks[2]:breaks[3]])~
            log10(radiiFPT[breaks[2]:breaks[3]]))
lm4 <- lm(log10(meanFPT[breaks[3]:breaks[4]])~
            log10(radiiFPT[breaks[3]:breaks[4]]))
lm5 <- lm(log10(meanFPT[breaks[4]:length(as.numeric(vars))])~
            log10(radiiFPT[breaks[4]:length(as.numeric(vars))]))

abline(lm1, lty=2, lwd=1 + summary(lm1)$coefficient[2,1], col=alpha("black", 0.8))
abline(lm2, lty=2, lwd=1 + summary(lm2)$coefficient[2,1], col=alpha("black", 0.8))
abline(lm3, lty=2, lwd=1 + summary(lm3)$coefficient[2,1], col=alpha("black", 0.8))
abline(lm4, lty=2, lwd=1 + summary(lm4)$coefficient[2,1], col=alpha("black", 0.8))
abline(lm5, lty=2, lwd=1 + summary(lm5)$coefficient[2,1], col=alpha("black", 0.8))

lines(log10(meanFPT)~log10(radiiFPT),
      type="l", lwd=4, col=alpha("grey40", 0.8))
legend("bottomright", lty=c(2,2,2,2,2), lwd=signif(c(1+summary(lm1)$coefficient[2,1],
                                                     1+summary(lm2)$coefficient[2,1],
                                                     1+summary(lm3)$coefficient[2,1],
                                                     1+summary(lm4)$coefficient[2,1],
                                                     1+summary(lm5)$coefficient[2,1]),2),
       c(paste(c(1000, round(radiiFPT[breaks],0))[1:2], collapse=" - "),
         paste(c(1000, round(radiiFPT[breaks],0))[2:3], collapse=" - "),
         paste(c(1000, round(radiiFPT[breaks],0))[3:4], collapse=" - "),
         paste(c(1000, round(radiiFPT[breaks],0))[4:5], collapse=" - "),
         paste(c(round(radiiFPT[breaks],0)[4], 100000), collapse=" - ")),
       bty="n", cex=0.75)
###########
par(mfrow=c(2,2))
for(i in 4:1)
{
  plot(fptLeo[[1]][,breaks[i]]~ Leo$timestamp, type="n",
       xlab="Time", ylab="FPT (days)",
       main=paste("Radius ", round(radiiFPT[breaks[i]],0), "meters"),
       bty="n")
  points(fptLeo[[1]][,breaks[i]]~ Leo$timestamp, pch=16, col=alpha("grey", 0.1))
  lines(fptLeo[[1]][,breaks[i]]~ Leo$timestamp)
}
par(mfrow=c(1,1))

###########
# dBBMM
###########
library(move)
library(lubridate)
Leroy <- move(system.file("extdata","leroy.csv.gz",package="move"))
Leroy <- spTransform(Leroy, center=T)
LeroyVar <- brownian.motion.variance.dyn(Leroy, location.error=rep(25, n.locs(Leroy)),
                                         window.size=71, margin=21)
VarDat <- data.frame(var=getMotionVariance(LeroyVar), hour=hour(LeroyVar$study.local.timestamp))
boxplot(VarDat$var~VarDat$hour, xlab="Hour of the day", ylab="mean Brownian variance", pch="*")

###########
# simulated tracks
###########
steps <- 1000
duration <- 3600
start.time <- Sys.time()
simmBrown<-   move(x=cumsum(rnorm(steps, 0, 1)),
                   y=cumsum(rnorm(steps, 0, 1)),
                   time=seq(start.time, start.time+duration, length.out=steps),
                   id="SimmBrown")

steps <- 1000
duration <- 3600
start.time <- Sys.time()
simmBias<-   move(x=cumsum(rnorm(steps, 0.1, 1)),
                  y=cumsum(rnorm(steps, 0.1, 1)),
                  time=seq(start.time, start.time+duration, length.out=steps),
                  id="SimmBiased")
sims <- moveStack(list(simmBrown, simmBias))
plot(sims, type="n")
lines(sims, col=c("grey", "black"))
###########
steps <- 1000
duration <- 3600
start.time <- Sys.time()
simm<-   move(x=cumsum(runif(steps, min=-1.96, max=1.96)),
              y=cumsum(runif(steps, min=-1.96, max=1.96)),
              time=seq(start.time, start.time+duration, length.out=steps),
              id="Simm")
plot(simm, type="l", xlab="Longitude", ylab="Latitude")
###########
hist(speed(simm), col="grey", xlim=c(0,1.1), main=NA, xlab="Speed")
hist(speed(sims[[1]]), breaks="FD", ylim=c(0,250), add=T, col=alpha("white", 0.5))
###########
library("adehabitatLT")
set.seed(5323)
crw <- simm.crw(1:100, r=.99)

plot(crw)
###########
rand.seg <- function(x, rep)
{
  s <- NULL
  for(i in 1:rep)
  {
    t <- data.frame(rbind(coordinates(x)[1,],coordinates(x)[1,]+
                            apply(apply(coordinates(x)-coordinates(x)[1,],2,diff)[sample(1:(n.locs(x)-1)),], 2, cumsum)))
    t <- move(x=t$x,
              y=t$y,
              time=timestamps(x),
              data=t,
              animal=paste("Repl.", i, sep=""))
    s <- c(s, list(t))
  }
  return(moveStack(s))
}

library(adehabitatLT)
library(move)
library(scales)
set.seed(12343)
rw <- simm.crw(1:20, h=1, r=0.9, c(10,10))
rw <- as(rw, "Move")
limes <- extent(bbox(rw))*1.3
plot(rw, xlim=c(limes@xmin, limes@xmax), 
     ylim=c(limes@ymin, limes@ymax), type="l", 
     lwd=1.6, bty="L",
     xlab="X-coordinates", ylab="Y-coordinates")

repl <- rand.seg(rw, 99)
lines(repl, col=alpha("black", 0.30), lty=2)
points(10,10, pch=17, cex=1.2)
points(tail(coordinates(rw),1), pch=19)
legend("topleft", pch=c(NA, NA, 17, 19), lty=c(1,2,NA,NA), 
       c("Empirical track", "Randomised track", "Start", "End"), 
       bty="n", cex=0.75)

###########

cor(speed(Leroy)[-length(speed(Leroy))], speed(Leroy)[-1])

###########

lag <- 2
cor(speed(Leroy)[seq(1, length(speed(Leroy))-lag, 1)],
    speed(Leroy)[seq(1+lag, length(speed(Leroy)), 1)])

###########

r <- NULL
p <- NULL
for(lag in 1:100)
{
  r <- c(r, cor.test(speed(Leroy)[seq(1, length(speed(Leroy))-lag, 1)],
                     speed(Leroy)[seq(1+lag, length(speed(Leroy)), 1)], method="spearman")$estimate)
  p <- c(p, cor.test(speed(Leroy)[seq(1, length(speed(Leroy))-lag, 1)],
                     speed(Leroy)[seq(1+lag, length(speed(Leroy)), 1)], method="spearman")$p.value)
}
plot(r, type="n", xlab="Lag", ylab="Correlation coefficient")
points(r[p<=0.05]~seq(1:100)[p<=0.05], pch=16, col="grey40")
points(r, type="b")
points(40,0.5, pch=16, col="grey40")
points(40,0.5)
text(41,0.49, expression(p<=0.05), pos=4)

###########

az0 <- as.circular(angle(Leroy), type="direction", 
                   units="degrees", template="geographic", 
                   zero="0", rotation="clock", modulo="asis")
r <- NULL
p <- NULL
for(lag in 1:100)
{
  r <- c(r, cor.circular(az0[seq(1, length(az0)-lag, 1)],
                         az0[seq(1+lag, length(az0), 1)]))
  p <- c(p, cor.circular(az0[seq(1, length(az0)-lag, 1)],
                         az0[seq(1+lag, length(az0), 1)], test=T)$p.value)
}
plot(r, type="n", xlab="Lag", ylab="Correlation coefficient", 
     ylim=c(-0.1, 1), main="Azimuth")
points(r[p<=0.05]~seq(1:100)[p<=0.05], pch=16, col="grey40")
points(r, type="b")
points(40,0.5, pch=16, col="grey40")
points(40,0.5)
text(41,0.49, expression(p<=0.05), pos=4)

Leroy <- spTransform(Leroy, CRS("+proj=longlat"))
turn0 <- as.circular(turnAngleGc(Leroy), type="angle", 
                     units="degrees", template="geographic", 
                     zero="0", rotation="clock", modulo="asis")
r <- NULL
p <- NULL
for(lag in 1:100)
{
  r <- c(r, cor.circular(turn0[seq(1, length(turn0)-lag, 1)],
                         turn0[seq(1+lag, length(turn0), 1)]))
  p <- c(p, cor.circular(turn0[seq(1, length(turn0)-lag, 1)],
                         turn0[seq(1+lag, length(turn0), 1)], test=T)$p.value)
}
plot(r, type="n", xlab="Lag", ylab="Correlation coefficient", 
     ylim=c(-0.1, 1), main="Turning angles")
points(r[p<=0.05]~seq(1:100)[p<=0.05], pch=16, col="grey40")
points(r, type="b")
points(40,0.5, pch=16, col="grey40")
points(40,0.5)
text(41,0.49, expression(p<=0.05), pos=4)

###########

library(scales)
maxBreaks <- floor(timeLag(Leroy, units="mins")[timeLag(Leroy, units="mins")>17] / min(timeLag(Leroy, units="mins")[timeLag(Leroy, units="mins")<17]))
minBreaks <- ceiling(timeLag(Leroy, units="mins")[timeLag(Leroy, units="mins")>17] / max(timeLag(Leroy, units="mins")[timeLag(Leroy, units="mins")<17]))
minBreaks[minBreaks==1] <- 2
breaks <- mapply("[", mapply(seq, minBreaks, maxBreaks, 1), lapply(lapply(mapply(seq, minBreaks, maxBreaks, 1), length), sample, size=1))
lags <- (timeLag(Leroy, units="secs")[timeLag(Leroy, units="mins")>17]) / breaks
mts <- lapply(mapply(rep, lags, (breaks-1)), cumsum)
TS <- as.POSIXct(unlist(mapply("+", timestamps(Leroy)[which(timeLag(Leroy, units="mins")>17)], mts)), origin=origin, tz="UTC")
LeroyInt <- interpolateTime(Leroy, TS, spaceMethod="greatcircle")
LeroyII <- data.frame(x=c(coordinates(Leroy)[,1], coordinates(LeroyInt)[,1]),
                      y=c(coordinates(Leroy)[,2], coordinates(LeroyInt)[,2]),
                      timestamp=c(timestamps(Leroy), timestamps(LeroyInt)))
LeroyII <- LeroyII[order(LeroyII$timestamp),]
LeroyII <- move(x=LeroyII$x, y=LeroyII$y, time=LeroyII$timestamp, id="LeroyInt")
par(mfcol=c(1,2))
plot(Leroy, type="l")
points(Leroy, pch=16, col=alpha("grey", 0.3))
points(LeroyInt, pch=16, col=alpha("black", 0.5), cex=0.5)
acf(speed(LeroyII), lag.max=100)

###########
par(mfrow=c(1,1))
plot(distance(Leroy)[timeLag(Leroy, units="mins")>17],
     timeLag(Leroy)[timeLag(Leroy, units="mins")>17],
     xlab="Dinstance in meters",
     ylab="Time lag in minutes", log="xy")

###########
