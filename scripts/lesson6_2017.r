setwd("/Users/julievanderhoop/Documents/R/Animove/Animove/")

###############

library(move)
bats <- move("./data/Parti-colored bat Safi Switzerland.csv")


library(adehabitatHR) # uses spatial points data frames and l-track 
X330 <- bats[["X330"]]
X330$id <- "X330" # package needs an id field 
mcpX330<-mcp(as(X330[,'id'], 'SpatialPointsDataFrame')) # minimum convex polygons -- encloses points within area. converts move -> spatial data frame within mcp 
plot(X330, type="n", bty="na", xlab="Longitude", ylab="Latitude")
plot(mcpX330, col="grey90", lty=2, lwd=1.25, add=TRUE)
points(X330, pch=16)
points(X330, pch=1, col="white")
legend("topright", as.character("95% MCP"), fill="grey90", bty="n")
# there are two points outside of MCP -- assumes by default you want 95% of points only. 
# we lose the temporal domain, here. 
###########

library("rgeos")
bats$id <- trackId(bats) 
mcpData<-mcp(as(bats[,'id'],'SpatialPointsDataFrame'))
#first option -- calculate MCP then project polygon
bats.proj <- spTransform(bats, CRS("+proj=somerc +lat_0=46.95240555555556 +lon_0=7.439583333333333 +x_0=600000 +y_0=200000 +ellps=bessel +units=m +no_defs"))
mcpData.proj <- mcp(as(bats.proj[, 'id'],'SpatialPointsDataFrame'))
#second option -- project then calculate
projection(mcpData) <- CRS("+proj=longlat +datum=WGS84")
mcpData <- spTransform(mcpData, CRS("+proj=somerc +lat_0=46.95240555555556 +lon_0=7.439583333333333 +x_0=600000 +y_0=200000 +ellps=bessel +units=m +no_defs"))
plot(bats.proj[["X21"]], bty="na", xlab="Longitude", ylab="Latitude")
plot(mcpData.proj[mcpData.proj$id=="X21",], add=TRUE)
plot(mcpData[mcpData$id=="X21",], add=TRUE, lty=2)
legend("bottomleft", c("First reproject then mcp", "First mcp then reproject"), lty=c(1,2), bty="n")

legend("topleft", sprintf("Area = %.2f", c(gArea(mcpData.proj, byid=TRUE)["X21"],gArea(mcpData, byid=TRUE)["X21"])/1000^2), lty=c(1,2), bty="n")
# because MCP calculates distances, the two methods have different results. 
# One is being calculated in meters, other in degrees
# METERS is better -- REPROJECT DATA FIRST. 
###########

hrBootstrap(bats[['X21']], rep=500, levelMax=95) # discovery curve

###########

library(raster)
template <- raster(extent(split(bats.proj)[[1]]))
res(template)<-500 # resolution
count <- rasterize(split(bats.proj)[[1]], template,field=1,  fun="count") 
# rasterize translates between spatial objects, counts n points in each raster cell 
plot(count, col=grey(10:0/12))
plot(mcpData.proj[1,], add=TRUE)
points(bats.proj[[1]], pch=16, cex=0.5)
# area of MCP is growing as collect data, 

########### Use kernels instead -- 2D probability distribution
# CLASSIC method assumes points are independent. CHRIS's method acknowledges autocorrelation

X21 <- bats.proj[['X21']] 
kern1 <- kernelUD(as(X21, "SpatialPoints"), h=500) # trying different h = smooth factors
kern2 <- kernelUD(as(X21, "SpatialPoints"))
kern3 <- kernelUD(as(X21, "SpatialPoints"), h=2000)
kern4 <- kernelUD(as(X21, "SpatialPoints"), h="LSCV") # least-squares cross validation -- inappropriate with autocorrelated data
par(mfrow=c(2,2))
par(mar=c(1,0.5,3,0.5))
kern <- c("kern1", "kern2", "kern3", "kern4")
hName <- c("h=500",
           "h='ad-hoc'",
           "h=2000",
           "h=LSCV")
library(scales)
for(i in 1:4)
{
  plot(getverticeshr(get(kern[i])))
  points(X21, pch=16, cex=0.75, col=alpha("black", 0.2))
  points(X21, cex=0.75)
  title(hName[i])
}

###########

library(ks) # can be used to calculate bandwitdh kernels -- kernel smoothers
library(scales)
pos <- coordinates(X21)
H.bcv <- Hbcv(x=pos)
H.pi <- Hpi(x=pos) 
H.lscv <- Hlscv(x=pos) 
H.scv <- Hscv(x=pos) 

par(mfrow=c(2,2))
par(mar=c(1,0.5,3,0.5))
H <- c("H.bcv", "H.pi", "H.lscv", "H.scv")
hT <- c("Biased cross-validation (BCV)",
        "Plug-in",
        "Least-squares cross-validation",
        "Smoothed cross-validation")
for(i in 1:4)
{
  fhat <- kde(x=pos, H=get(H[i]), compute.cont=TRUE) 
  plot(fhat, cont=c(50, 5), bty="n", 
       xaxt="n", yaxt="n", 
       xlab=NA, ylab=NA, asp=1)
  points(X21, pch=16, cex=0.75, col=alpha("black", 0.2))
  title(hT[i])
}

########### What if we make lots of small polygons and combine those? LOCAL Convex Polygon

par(list(mfrow=c(2,2), mar=c(2,2,2,2)))
library(move)
library(maptools)
library(adehabitatHR)
leroy <- spTransform(leroy, center=TRUE)

leroy.mcp <- mcp(as(leroy, "SpatialPoints"), percent=95)
plot(leroy.mcp, col=grey(0.9), lty=2, lwd=2) # original MCP 
points(leroy, col="#00000060", pch=16, cex=0.5)
lines(leroy, col="#00000030")
title("Minimum convex polygon")

kLoc <- LoCoH.k(as(leroy, "SpatialPoints"), 75) # based on k neighbours - are sequential in time
plot(kLoc, col=grey((0:length(kLoc)/length(kLoc))*0.7), border=NA)
title("k-NNCH LoCoH")
# much more structure in movement

rLoc <- LoCoH.r(as(leroy, "SpatialPoints"), 800) # these are the radii 
plot(rLoc, col=grey((0:length(rLoc)/length(rLoc))*0.7), border=NA)
title("r-NNCH LoCoH") # based on a radius - not subsequent but close together in space

aLoc <- LoCoH.a(as(leroy, "SpatialPoints"), 9000)
plot(aLoc, col=grey((0:length(aLoc)/length(aLoc))*0.7), border=NA)
title("a-NNCH LoCoH") # based on max number of neighbours where sum of distances is <= a

# LoCoH has great tutorials with visualisation etc. 

########### These thresholds can really affect the measurements of space use estimation
# websites through LoCoH to talk about threshold selection: 

######par(mfrow=c(1,3))
kLocArea <- LoCoH.k.area(as(leroy, "SpatialPoints"), 
                         krange=floor(seq(75, 500, length=10)), 
                         percent=90)
title("k-NNCH LoCoH")
rLocArea <- LoCoH.r.area(as(leroy, "SpatialPoints"), 
                         rrange=seq(500, 1600, 100), 
                         percent=90)
title("r-NNCH LoCoH")
aLocArea <- LoCoH.a.area(as(leroy, "SpatialPoints"), 
                         arange=seq(5000, 13000, 1000), 
                         percent=90)
title("a-NNCH LoCoH")

#####

library(tlocoh)
leroy.lxy <- move.lxy(leroy)
leroy.lxy <- lxy.nn.add(leroy.lxy, s=0, k=seq(5, 105, 10))
leroy.lhs <- lxy.lhs(leroy.lxy, k=seq(5, 105, 10), s=0)
leroy.lhs <- lhs.iso.add(leroy.lhs)

###########

par(mfrow=c(1,2))
par(list(mar=c(5, 4, 4, 2) + 0.1), bty="n")
iso.info.all <- do.call(rbind, lapply(leroy.lhs, function(myhs) do.call(rbind, 
                                                                        lapply(myhs$isos, function(myiso) data.frame(id = myhs[["id"]], 
                                                                                                                     mode = myhs[["mode"]], s = myhs[["s"]], param.val = myhs[[myhs[["mode"]]]], 
                                                                                                                     sort.metric = myiso[["sort.metric"]], myiso[["polys"]]@data[c("iso.level", 
                                                                                                                                                                                   "area")])))))
iso.info.all$area <- iso.info.all$area/(1000*1000)
plot(area~param.val, type="n", data=iso.info.all, 
     xlab="Number of neighbours (k)", 
     ylab=expression(paste("Area in ", km^2, sep="")), ylim=c(-0.1, 16.1))
for(i in 1:length(unique(iso.info.all$iso.level)))
{
  tmp <- iso.info.all[iso.info.all$iso.level==unique(iso.info.all$iso.level)[i],]
  lines(area~param.val, type="l", data=tmp, lty=i)
}
legend("topleft", as.character(unique((iso.info.all$iso.level))), lty=1:length(unique(iso.info.all$iso.level)), cex=0.6, bty="n")
par(mar=c(1,1,1,1))
plot(leroy.lhs[[8]]$isos[[1]]$polys, col=grey((0.7*seq(0.1,0.99,length=5))), border=NA)
points(leroy, col="#00000060", pch=16, cex=0.5)
lines(leroy, col="#00000030")
text(0,3000,"k-NNCH LoCoH for k=75", pos=4, cex=0.75)
legend(-1636, -2018, as.character(unique((iso.info.all$iso.level))), fill=grey((0.7*seq(0.1,0.99,length=5))), cex=0.6, bty="n")

###########

par(mfrow=c(1,1))
par(list(mar=c(5, 4, 4, 2) + 0.1), bty="o")
leroy.lxy <- lxy.nn.add(leroy.lxy, s=0, a=9000)
leroy.lhs <- lxy.lhs(leroy.lxy, a=9000, s=0, iso.levels = seq(0.1, 0.99, length=10))
leroy.lhs <- lhs.iso.add(leroy.lhs, iso.levels = seq(0.1, 0.99, length=10))
plot(leroy.lhs[[1]]$isos[[1]]$polys, col=grey((0.7*seq(0.1,0.99,length=10))), border=NA)
points(leroy, col="#00000060", pch=16, cex=0.5)
lines(leroy, col="#00000030")
title(expression(paste("a-NNCH LoCoH for ", a <= 9000, sep="")))

###########

leroy.lxy <- move.lxy(leroy)
leroy.lxy <- lxy.ptsh.add(leroy.lxy)

###########

lxy.plot.sfinder(leroy.lxy, delta.t=3600*c(6,12,24,36,48,54,60))

###########

leroy.lxy <- lxy.nn.add(leroy.lxy, s=0.05, k=seq(10, 100, 10))
leroy.lhs <- lxy.lhs(leroy.lxy, s=0.05, k=seq(10, 100, 10))
leroy.lhs <- lhs.iso.add(leroy.lhs, k=seq(10, 100, 10))

###########

lhs.plot.isoear(leroy.lhs)
plot(leroy.lhs, iso=TRUE, record=TRUE)
points(leroy, col="#00000060", pch=16, cex=0.5)
lines(leroy, col="#00000030")
title(expression(paste("a-NNCH LoCoH for ", a <= 9000, sep="")))

########### Isopleths

par(mfrow=c(1,2))
par(list(mar=c(5, 6, 4, 2) + 0.1), bty="n")
iso.info.all <- do.call(rbind, lapply(leroy.lhs, function(myhs) do.call(rbind, 
                                                                        lapply(myhs$isos, function(myiso) data.frame(id = myhs[["id"]], 
                                                                                                                     mode = myhs[["mode"]], s = myhs[["s"]], param.val = myhs[[myhs[["mode"]]]], 
                                                                                                                     sort.metric = myiso[["sort.metric"]], myiso[["polys"]]@data[c("iso.level", 
                                                                                                                                                                                   "area", "edge.len")])))))
plot(I(edge.len/area)~param.val, type="n", data=iso.info.all, 
     xlab="Number of neighbours (k)", 
     ylab=expression(edge%/%area))
for(i in 1:length(unique(iso.info.all$iso.level)))
{
  tmp <- iso.info.all[iso.info.all$iso.level==unique(iso.info.all$iso.level)[i],]
  lines(I(edge.len/area)~param.val, type="l", data=tmp, lty=i)
}
legend("topright", as.character(unique((iso.info.all$iso.level))), lty=1:length(unique(iso.info.all$iso.level)), cex=0.6, bty="n")
par(mar=c(1,1,1,1))
plot(leroy.lhs[[5]]$isos[[1]]$polys, col=grey((0.7*seq(0.1,0.99,length=5))), border=NA)
points(leroy, col="#00000060", pch=16, cex=0.5)
lines(leroy, col="#00000030")
text(0,3000,"k-NNCH LoCoH for k=50", pos=4, cex=0.75)
legend(-1636, -2018, as.character(unique((iso.info.all$iso.level))), fill=grey((0.7*seq(0.1,0.99,length=5))), cex=0.6, bty="n")

########### Brownian Bridges

library(move)
leroy <- move(system.file("extdata","leroy.csv.gz",package="move"))
leroy <- spTransform(leroy, center=TRUE) # center projection to minimize distortion - notice the locations are centered on (0,0)
BB.leroy <- brownian.bridge.dyn(leroy, ext=2, 
                                dimSize=150, 
                                location.error=100, # in map units -- CAN MAKE THIS A VECTOR OF E.G. ARGOS ERRORS
                                margin=21,  # 21 locations - size of which we cannot calculate break points -- WHAT IS THE TIME/SPACE RANGE THAT ANIMAL CHANGE BEHAVIOUR?
                                window.size=99, # larger window = less likely to find spurious changes -- 99 is ~ 1 day for Leroy
                                time.step=2)
# always need odd numbers to leave one out. Documentation has improved recently
udleroy <- getVolumeUD(BB.leroy)
plot(leroy, col="#00000060", pch=16, cex=0.5,
     bty="n", xaxt="n", yaxt="n", xlab=NA, ylab=NA)
lines(leroy, col="#00000030")
contour(udleroy, levels=c(0.5, 0.95), add=TRUE, lwd=c(2, 1), lty=c(2,1))
title("Dynamic brownian bridge")

###########

par(mfrow=c(3,3), mar=c(1,2,2,1))
margins <- c(15, 9, 3)
windows <- c(101, 67, 33)
runs <- expand.grid(margins, windows)
for(i in 1:nrow(runs)) # error here - wants a larger raster - probabilities are at border of raster
{
  BB.leroy <- brownian.bridge.dyn(leroy, dimSize=150, 
                                  location.error=100, 
                                  margin=runs[i,1], window.size=runs[i,2], 
                                  time.step=2) # add ext = 2, for example. -- extends extent of raster by factor
  udleroy <- getVolumeUD(BB.leroy)
  contour(udleroy, levels=c(0.5, 0.95), bty="n", xaxt="n", 
          yaxt="n", xlab=NA, ylab=NA, asp=1)
  mtext(paste("Margin = ", runs[i,1], sep=""), 2)
  mtext(paste("Window size = ", runs[i,2]), 3)
}

########### DBB will be strongly influenced by sampling rate 

par(mfrow=c(1,1))
par(list(mar=c(5, 4, 4, 2) + 0.1), bty="o")
set.seed(3628492)
steps <- 100000
prop=seq(0.01,1,0.01)

track <- as(simm.crw(date=1:steps, h=1, r=0.8), "Move")
thin <- lapply(prop, function(x) track[round(seq(1, steps, length.out=steps * x)), ])
short <- lapply(prop, function(x) track[1:round(steps * x),])
ThinAreas <- lapply(lapply(lapply(thin, as, "SpatialPoints"), kernelUD), kernel.area, percent=95)
ShortAreas <- lapply(lapply(lapply(short, as, "SpatialPoints"), kernelUD), kernel.area, percent=95)

plot(I(unlist(ThinAreas)/min(unlist(ThinAreas)))~seq(1:100), 
     xlab="Percent of the track", 
     ylab="Relative area", ylim=c(0,1.75), 
     type="l", lwd=2, lty=1)
lines(I(unlist(ShortAreas)/max(unlist(ShortAreas)))~seq(1:100),
      lty=2, lwd=2)
abline(h=1)
legend("topright", c("Thinned trajectory", "Shortened trajectory"), 
       lty=c(1,2), lwd=c(2,2), bty="n")
# method we are using is intended to give us info on place where animal was when we did not observe it
# when we increase sample rate, we know where animal was at any given point in time -- this area becomes smaller

###########

