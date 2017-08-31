# how do our animals function in relation to environmental resources? 
# put trajectories into context 

require(raster)
empty <- raster(xmn=-180, xmx=180, ymn=-90, ymx=90, 
                resolution=0.1, crs="+proj=longlat +datum=WGS84")
empty # create an empty raster - useful as a template to cut things, calculate things on


###### Creating spatial objects: what were conditions in specific place? 
Obj1 <- data.frame(x=c(-1, 0, 1), y=c(0, 1, 0), # give coordinates
                   pID=c("A", "B", "C")) # give some attributes
coordinates(Obj1) <- ~x+y # define x and y as coordinates
projection(Obj1) <- CRS("+proj=longlat +datum=WGS84")
str(Obj1)


##### download info from the web - similar to getData

dir.create("DEM")
download.file("http://biogeo.ucdavis.edu/data/climate/worldclim/1_4/grid/cur/alt_10m_bil.zip", 
              destfile="./DEM/alt_10m_bil.zip")
unzip("DEM/alt_10m_bil.zip", exdir="./DEM/alt_10m")

#####

require(raster)
dem <- raster("DEM/alt_10m/alt.bil")
plot(dem, col=terrain.colors(256)) 

require(mapdata)
map("worldHires", add=T)

require(raster)
require(scales)
dem <- raster("DEM/alt_10m/alt.bil")
DEMsub <- crop(dem, extent(c(-15.5, 47.2, 25.5, 70.2)))
plot(DEMsub, col=terrain.colors(256))
DEMslope <- terrain(DEMsub, opt="slope")
DEMaspect <- terrain(DEMsub, opt="aspect")
DEMhs <- hillShade(DEMslope, DEMaspect, angle=60, direction=270)

image(DEMhs, col=grey(0:16/16), asp=1, xlab="Longitude", ylab="Latitude")
image(DEMsub, col=alpha(terrain.colors(256), 0.66), add=T)



######
require(move)
Leo <- move("./data/Leo-65545.csv.gz")
elev <- extract(dem, Leo, method="bilinear") # overlay on raster and extract values. linear interpolation
plot(elev~timestamps(Leo), type="l",
     xlab="Distance [km]", ylab="Elevation [m]")
# leo overwinters on coast, sometimes crosses very high things... 

Leo$Elevation <- elev 

############# Global Land Cover
dir.create("GLC")
download.file("http://due.esrin.esa.int/files/Globcover2009_V2.3_Global_.zip", 
              destfile="./GLC/Globcover2009_V2.3_Global_.zip")
unzip("GLC/Globcover2009_V2.3_Global_.zip", exdir="./GLC/GLC09")


list.files("GLC/GLC09/")

#########################
require(raster)
require(readxl)
GLC <- raster("GLC/GLC09/GLOBCOVER_L4_200901_200912_V2.3.tif")
glc <- crop(GLC, extent(Leo)*1.5, filename="./GLC/cropped_GlobCover09", overwrite=TRUE)
ratt <- read_excel("GLC/GLC09/Globcover2009_Legend.xls") # have to read in excel 
ratt$col <- rgb(cbind(ratt$Red, ratt$Green, ratt$Blue), maxColorValue = 255) # remap land cover types
names(ratt) <- c("ID", "landcover", "r", "g", "b", "col")
levels(glc) <- ratt
row.names(ratt) <- ratt$ID
image(glc, col=ratt$col)
lines(Leo)
barplot(sort(summary(as.factor(extract(glc, Leo))), decreasing=T)[1:5], 
        names=names(sort(summary(as.factor(extract(glc, Leo))), decreasing=T)[1:5]),
        col=ratt[names(sort(summary(as.factor(extract(glc, Leo))), decreasing=T)[1:5]), "col"])
legend("topright", c("Closed to open herbaceous vegetation",
                     "Mosaic vegetation",
                     "Closed needleleaved evergreen forest",
                     "Closed to open mixed forest",
                     "Closed broadleaved deciduous forest"),
       pch=rep(16,5),
       # col=ratt[names(sort(summary(as.factor(extract(glc, Leo))), decreasing=T)[1:5]), "col"],
       cex=0.66)

# these are static maps so easy to overlay/extract

##############

################### What about extracting from time series? Extracting from raster stacks
# or interpolating between layers in space and time, can be weighted
dir.create("MeanTemp")
download.file("http://biogeo.ucdavis.edu/data/climate/worldclim/1_4/grid/cur/tmean_10m_bil.zip", 
              destfile="./MeanTemp/tmean_10m_bil.zip")
unzip("MeanTemp/tmean_10m_bil.zip", exdir="./MeanTemp/MeanTemp10Deg")
MeanT <- list()
for(i in 1:12) # compare month of layer and month of locations 
{
  tmp <- raster(paste("MeanTemp/MeanTemp10Deg/tmean", i, ".bil", sep=""))
  MeanT <- c(MeanT, list(tmp))
}
MeanT <- stack(MeanT)
annualT <- extract(MeanT, Leo)
require(lubridate)
Leo$meanT <- mapply(function(i, j) annualT[i,j], 1:nrow(annualT), month(timestamps(Leo)))/10
hist(Leo$meanT, breaks="FD")
plot(Leo$meanT~yday(timestamps(Leo)), type="l")

# year-day temp high in jan, leo goes to cooler temperatures in summer
# spends winter in areas warmer than summers

##########################
library(move)
library(raster)
library(adehabitatHR)
library(adehabitatHS)
#read the bat data
bats <- move("./data/Parti-colored bat Safi Switzerland.csv")

#read the landuse data as csv and adjust the separator -- KAMI WILL SEND FILE. Land use data from Swiss statistics center
nlcd <- read.csv("./data/AREA_NOAS04_17_131004.csv", as.is=T, sep=";")
#select the landuse classification for the year 1997 in 17 categories
nlcd <- nlcd[,c("X", "Y", "AS97R_17")]
#convert into a raster and declare the Swiss grid projection
nlcd <- rasterFromXYZ(nlcd, crs = CRS("+proj=somerc +lat_0=46.95240555555556 
                                      +lon_0=7.439583333333333 +x_0=600000 
                                      +y_0=200000 +ellps=bessel 
                                      +units=m +no_defs"))
#project the bat data into the projection of the raster data
batsProj <- spTransform(bats, projection(nlcd)) # now projecting the projected points
#overlay points with raster and count the ocurrences per individual

#urbanised areas
nlcd[nlcd%in%c(1,2,3,4,5)] <- 1 # replace things cleverly :) 
#agricultural areas
nlcd[nlcd%in%c(6,7,8,9)] <- 2 
#forest
nlcd[nlcd%in%c(10,11,12,15)] <- 3 
#lakes and rivers
nlcd[nlcd%in%c(13,14)] <- 4
#unproductive alpine areas
nlcd[nlcd%in%c(16,17)] <- 5


freqLoc <- lapply(lapply(split(batsProj), function(x) extract(nlcd, x, method="simple")), table)
#convert to spdf, calculate the 95\% mcp, overlay with the raster and count the number of ocurrences per individual
freqMCP <- lapply(lapply(lapply(lapply(split(batsProj), as, "SpatialPointsDataFrame") # SPLIT THE MOVESTACK
                                , mcp)
                         , function(x) extract(nlcd, x, method="simple")) # EXTRACT LAND COVER DATA
                  , table) # TABLE THAT
#set the categories that did not appear to 0
freqLocFilled <- lapply(freqLoc, function(x) {templ <- rep(0, 5); templ[as.numeric(names(x))] <- x; return(templ)})
# do the same with the categories in the mcps that did not occur
freqMCPFilled <- lapply(freqMCP, function(x) {templ <- rep(0, 5); templ[as.numeric(names(x))] <- x; return(templ)})
# calculate the proportions
PropLoc <- t(mapply("/", freqLocFilled, lapply(freqLocFilled, sum))) # proportion used versus available
PropMCP <- t(mapply("/", freqMCPFilled, lapply(freqMCPFilled, sum)))

# derive percentages
PctLoc <- 100 * PropLoc
PctMCP <- 100 * PropMCP

# combine the list data in a data frame
freqLocFilled <- do.call("rbind", freqLocFilled)
freqMCPFilled <- do.call("rbind", freqMCPFilled)

# read movement reference for sexes
bats_ref <- read.csv("./data/Parti-colored bat Safi Switzerland-reference-data.csv", sep=",", as.is=TRUE)
# keep only observations with known attributes
bats_ref <- bats_ref[!is.na(bats_ref$animal.id), 
                     c("animal.id", "animal.sex")]
# rename variables
names(bats_ref) <- c("id", "sex")
# match notation
bats_ref$id <- paste0("X", bats_ref$id)
# calculate sample size
N <- data.frame(N=unlist(lapply(split(bats), n.locs)))
# define id
N$id <- row.names(N)
# merge data frames
bats_ref <- merge(bats_ref, N, by="id")
row.names(bats_ref) <- bats_ref$id
# identify which bats to keep and which to drop from analysis
dropList <- bats_ref$id[bats_ref$N<50] # keep only those who have > 50 locations
bats_ref <- bats_ref[bats_ref$N>50,]

# reduce the proprtions to only those individuals that will be retained
PropLoc <- PropLoc[!row.names(PropLoc) %in% dropList,]
PropMCP <- PropMCP[!row.names(PropMCP) %in% dropList,]

# the same for the percentage data
PctLoc <- PctLoc[!row.names(PctLoc) %in% dropList,]
PctMCP <- PctMCP[!row.names(PctMCP) %in% dropList,]

# and the same for the frequencies
freqLocFilled <- freqLocFilled[!row.names(freqLocFilled) %in% dropList,]
freqMCPFilled <- freqMCPFilled[!row.names(freqMCPFilled) %in% dropList,]

# run eisera for the different classes
eis <- eisera(freqLocFilled, PropMCP, nf=6, scannf = F)
# show the results
plot(eis$li[,1:2], pch=3+as.numeric(as.factor(bats_ref[row.names(eis$wij),"sex"])),
     xlab="Principal coordinate 1", ylab="Principal coordinate 2")
abline(v=0)
abline(h=0)
legend("topright", c("Female", "Male"), pch=c(4,5))

#### Compositional analysis

PctLocF <- PctLoc[row.names(bats_ref[bats_ref$sex=="f",]),]
PctMCPF <- PctMCP[row.names(bats_ref[bats_ref$sex=="f",]),]
HS_F <- compana(PctLocF[,-5], PctMCPF[,-5], nrep = 10000)
HS_F$test # NSD means they are using land categories as they present themselves, as they appear. 

PctLocM <- PctLoc[row.names(bats_ref[bats_ref$sex=="m",]),]
PctMCPM <- PctMCP[row.names(bats_ref[bats_ref$sex=="m",]),]
HS_M <- compana(PctLocM[,-5], PctMCPM[,-5], nrep = 10000)
HS_M$test # males have land use associations significantly different than what is available to them

sort(HS_M$rank, decreasing = T)
HS_M$rm # appear over water more than we would expect


######## IF THE BATS WENT IN A STRAIGHT LINE ONLY, WHAT WOULD BE AVAILABLE WITHIN A DAY'S TRAVEL?
# select for females
FemaleBats <- batsProj[batsProj@trackId %in% bats_ref[bats_ref$sex=="f","id"],] 
# calculate daily distance
DDist <- mapply("/", lapply(distance(FemaleBats), sum), 
                lapply(split(FemaleBats), function(x) sum(timeLag(x, units="days"))))
# calculate center of locations per individual
centerP <- lapply(lapply(split(FemaleBats), coordinates), colMeans)
# get landuse data for a daily distance around a center of positions
freqStudyFilled <- mapply(function(x,y) table(getValues(nlcd)[getValues(distanceFromPoints(nlcd, x) <= y/2)]),
                          centerP, DDist, SIMPLIFY = F)
# merge in a data frame
PctStudyF <- do.call("rbind", lapply(freqStudyFilled, function(x) x/sum(x)*100))
# compare composition dropping column 5
HS_II_F <- compana(PctMCPF[,-5], PctStudyF[,-5], nrep = 10000)
# output the test statistics
HS_II_F$test
sort(HS_II_F$rank, decreasing = T) # 4 is WAY over represented compared to what would be expected within a day's travel
# females are highly selective to a central location, fill MCP; males highly selective WITHIN MCP. 
# males and females select at different levels. 
HS_II_F$rm

## LUNCH 

#####
plot(nlcd, col=c("grey", "forestgreen", "wheat", "blue", "white"),
     xlim=as.vector(extent(FemaleBats)*2)[1:2],
     ylim=as.vector(extent(FemaleBats)*2)[3:4])
lines(FemaleBats)
points(colMeans(coordinates(FemaleBats))[1], colMeans(coordinates(FemaleBats))[2], pch=20, col="blue", cex=4.5) 
points(colMeans(coordinates(FemaleBats))[1], colMeans(coordinates(FemaleBats))[2], pch="*", col="white", cex=4) 




### Resource utilisation functions

# load the buffalo data
load("buffalos.rdata")
# define study area as twice the bounding box
studyArea <- extent(buffalo)*2

# First download monthly temperature
download.file("http://biogeo.ucdavis.edu/data/climate/worldclim/1_4/grid/cur/tmean_30s_bil.zip", 
              "tmean_30s_bil.zip")
# unzip in temporary folder
unzip("tmean_30s_bil.zip", exdir = tempdir() )
# Read all months and store in a raster stack
tempGlob <- stack(list.files(tempdir(), pattern=".bil", full.names = T))
# create a new raster stack cropped to the study area
tempKruger <- crop(tempGlob, studyArea)
# Calculate mean annual temperature and store on disk for later use
meanTKruger <- stackApply(tempKruger, 1, fun=mean, na.rm=T, filename="Temperature.tif")
# Transform the data in degrees Celsius and round
meanTKruger <- round(meanTKruger/10, 2)
# Delete the unzipped files
unlink(list.files(tempdir(), pattern="tmean", full.names = T))

# No repeat every step for monthly precipitation 
download.file("http://biogeo.ucdavis.edu/data/climate/worldclim/1_4/grid/cur/prec_30s_bil.zip", 
              "prec_30s_bil.zip")
unzip("prec_30s_bil.zip", exdir=tempdir())
precGlob <- stack(list.files(tempdir(), pattern=".bil", full.names = T))
precKruger <- crop(precGlob, studyArea)
meanPKruger <- stackApply(precKruger, 1, fun=mean, na.rm=T, filename="Precipitation.tif")
unlink(list.files(tempdir(), pattern="prec", full.names = T))

# Get digital elevation
# SRTM reaches from 60°North to 60°South and the data is 
# distributed in 5° tiles counting from the topleft tile
# based on the bounding box of the study area we calculate 
# the tile numbers.
xtile <- ceiling((bbox(studyArea)[1,]+180)/5)
ytile <- 24 - floor(((60+bbox(studyArea)[2,])/5))
# For all tile number combinations we try to download
# the digital elevation model data and unzip them
# into the temporaray folder.
# Note that there might be failed download attempts
# as there is only data for land.
for(i in xtile[1]:xtile[2])
{
  if(i < 10)
  {
    ii <- paste0("0", i)
  }else{
    ii <- i
  }
  
  for(j in ytile[1]:ytile[2])
  {
    if(j < 10)
    {
      jj <- paste0("0", j)
    }else{
      jj <- j
    }
    tile <- paste("srtm_", ii, "_" , jj, ".zip", sep="")
    tileURL <- paste("http://srtm.csi.cgiar.org/SRT-ZIP/SRTM_V41/SRTM_Data_GeoTiff/", tile, sep="")
    download.file(tileURL, paste(tempdir(), tile, sep="/"))
    unzip(paste(tempdir(), tile, sep="/"), exdir=tempdir())
  }
}
# Read the tiles in a list of single raster objects
dems <- lapply(list.files(tempdir(), pattern=".tif", full.names = T), raster)
# Merge the tiles into a single raster
DEM <- do.call("merge", dems)
# Reduce to the extent of the study area
DEM <- crop(DEM, studyArea)
# Reduce resolution to match the other environmental rasters
DEMlr <- aggregate(DEM, fact=10, fun=mean, na.rm=T)
demKruger <- resample(DEMlr, meanTKruger, method="bilinear", 
                      filename="DigitalElevationModel.tif")
# Join all layers in a single raster stack with the same topology
EnvK <- stack(meanTKruger, meanPKruger, demKruger) 

EnvK <- stack(list.files("./", pattern=".tif", full.names = T))



library(move)
library(RStoolbox)
# project and center the movement data
buffalo.proj <- spTransform(buffalo, center=T)
# project the raster into the same spatial space
EnvK.proj  <- projectRaster(EnvK, crs=projection(buffalo.proj))
# calculate a principal component analysis on the raster data
EnvK.projPCA <- rasterPCA(EnvK.proj)
# investigate the PCA
EnvK.projPCA$model$loadings

########################## RUF ##################################
cillaUD <- brownian.bridge.dyn(buffalo.proj[[1]], EnvK.projPCA$map[[1]],
                               location.error = rep(25, sum(n.locs(buffalo.proj[[1]]))),
                               margin = 11, window.size = 71)
cillaBBMM <- getVolumeUD(cillaUD)
cillaUD[cillaBBMM>0.99] <- NA
EnvK.Cilla <- EnvK.projPCA$map
EnvK.Cilla[cillaBBMM>0.99] <- NA

dataTable <- data.frame(coordinates(cillaUD), UD=getValues(cillaUD), getValues(EnvK.Cilla))
dataTable <- dataTable[complete.cases(dataTable),]
str(dataTable)

############################
library(mgcv)
mod <- gam(log(UD) ~ PC1 + PC2 + PC3 + s(x,y), 
           select=T, data=dataTable)
par(mfrow=c(2,2))
gam.check(mod)

############################
library(ncf)
correlog1.1 <- correlog(dataTable$x, dataTable$y, residuals(mod), 
                        na.rm=T, increment=1, resamp=999)
plot(correlog1.1$correlation[1:20], 
     pch=abs(as.numeric(correlog1.1$p[1:20]<0.05))*15+1, 
     type="b")
par(mfrow=c(1,1))
summary(mod)

##################################
library(scales)
vis.gam(mod, view = c("x", "y"), plot.type = "contour", color="gray")
points(buffalo.proj[["Cilla"]], pch=16, cex=0.3, col=alpha("white", 0.3))
lines(buffalo.proj[["Cilla"]], pch=1, cex=0.3, col=alpha("white", 0.7))
################## RSF #####################
library(adehabitatHR)
mcpB <- mcp(buffalo.proj, percent=100)
used <- extract(EnvK.projPCA$map, buffalo.proj, method="bilinear")
used <- data.frame(used, x=coordinates(buffalo.proj)[,1],
                   y=coordinates(buffalo.proj)[,2], 
                   ID=as.character(buffalo.proj$individual.local.identifier))
used$pres <- 1
randLoc <- spsample(mcpB, sum(unlist(n.locs(buffalo.proj))), "random")
avail <- extract(EnvK.projPCA$map, randLoc, method="bilinear")
avail <- data.frame(avail, coordinates(randLoc), 
                    ID=as.character(buffalo.proj$individual.local.identifier))
avail$pres <- 0
data <- rbind(used, avail)
dataCilla <- data[data$ID=="Cilla",]
CillaMod <- glm(pres~PC1+PC2+PC3, 
                data=dataCilla, family="binomial")

#######################
dataThin <- rbind(used[seq(from=1, to=nrow(used), by=6),], avail)
dataCillaThin <- dataThin[dataThin$ID=="Cilla",]
CillaModThin <- glm(pres~PC1+PC2+PC3, 
                    data=dataCillaThin, 
                    family="binomial")


PC1 <- EnvK.projPCA$map[[1]]
PC1[is.na(over(as(EnvK.proj[[1]], "SpatialGridDataFrame"), mcpB)$area)] <- NA
plot(mcpB)
image(PC1, add=T, col=gray(8:1/8))
plot(mcpB, add=T)
mtext(paste("Total number of grid cells\nwithin the 100% mcp: ", 
            sum(!is.na(getValues(PC1))), sep=""), side=3)

#########################
library(lubridate)
subModelling <- function(track, UAratio=1) {
  used <- extract(EnvK.projPCA$map, track, method="bilinear")
  used <- data.frame(used, x=coordinates(track)[,1],
                     y=coordinates(track)[,2], 
                     ID="ind1")
  used$pres <- 1
  randLoc <- spsample(mcpB, floor(n.locs(track)*UAratio), "stratified")
  avail <- extract(EnvK.projPCA$map, randLoc, method="bilinear")
  avail <- data.frame(avail, coordinates(randLoc), 
                      ID="ind1")
  avail$pres <- 0
  names(avail) <- names(used) 
  data <- rbind(used, avail)
  Mod <- glm(pres~PC1+PC2+PC3, 
             data=data, family="binomial")
  return(Mod)
}
Cilla <- buffalo.proj[["Cilla"]]
subCilla <- Cilla[tapply(1:n.locs(Cilla), yday(timestamps(Cilla)), 
                         function(x,y) sample(x,1)),]

simRatio <- lapply(10^seq(0, 2, length.out=20), function(x) do.call("rbind", lapply(lapply(rep(x,1000), subModelling, track=subCilla),"[[", 1)))
simMeanSD <- lapply(simRatio, function(x) rbind(apply(x, 2, mean), apply(x,2,sd)))
SimMean <- do.call("rbind", simMeanSD)[seq(1,40,2),]
SimVar <- do.call("rbind", simMeanSD)[seq(2,40,2),]

par(mfrow=c(2,2))
par(mar=c(1,4,4,2)+0.1)
plot(SimMean[,"(Intercept)"]~I(10^seq(0, 2, length.out=20)), 
     ylim=c(min(SimMean[,"(Intercept)"]-SimVar[,"(Intercept)"]), max(SimMean[,"(Intercept)"]+SimVar[,"(Intercept)"])), 
     type="b", ylab="Estimate of Intercept", xlab=NA, pch=20)
lines(I(SimMean[,"(Intercept)"]-SimVar[,"(Intercept)"])~I(10^seq(0, 2, length.out=20)), type="l", lty=2)
lines(I(SimMean[,"(Intercept)"]+SimVar[,"(Intercept)"])~I(10^seq(0, 2, length.out=20)), type="l", lty=2)

plot(SimMean[,"PC1"]~I(10^seq(0, 2, length.out=20)), 
     ylim=c(min(SimMean[,"PC1"]-SimVar[,"PC1"]), max(SimMean[,"PC1"]+SimVar[,"PC1"])), 
     type="b", ylab="Estimate of PC1", xlab=NA, pch=20)
lines(I(SimMean[,"PC1"]-SimVar[,"PC1"])~I(10^seq(0, 2, length.out=20)), type="l", lty=2)
lines(I(SimMean[,"PC1"]+SimVar[,"PC1"])~I(10^seq(0, 2, length.out=20)), type="l", lty=2)

par(mar=c(5,4,2,2)+0.1)
plot(SimMean[,"PC2"]~I(10^seq(0, 2, length.out=20)), 
     ylim=c(min(SimMean[,"PC2"]-SimVar[,"PC2"]), max(SimMean[,"PC2"]+SimVar[,"PC2"])), 
     type="b", ylab="Estimate of PC2", xlab="Ratio of available over used", pch=20)
lines(I(SimMean[,"PC2"]-SimVar[,"PC2"])~I(10^seq(0, 2, length.out=20)), type="l", lty=2)
lines(I(SimMean[,"PC2"]+SimVar[,"PC2"])~I(10^seq(0, 2, length.out=20)), type="l", lty=2)

plot(SimMean[,"PC3"]~I(10^seq(0, 2, length.out=20)), 
     ylim=c(min(SimMean[,"PC3"]-SimVar[,"PC3"]), max(SimMean[,"PC3"]+SimVar[,"PC3"])), 
     type="b", ylab="Estimate of PC3", xlab="Ratio of available over used", pch=20)
lines(I(SimMean[,"PC3"]-SimVar[,"PC3"])~I(10^seq(0, 2, length.out=20)), type="l", lty=2)
lines(I(SimMean[,"PC3"]+SimVar[,"PC3"])~I(10^seq(0, 2, length.out=20)), type="l", lty=2)


#####################################      
rand.seg <- function(x, repeats=10)
{
  coordinateDiffs <- cbind(diff(coordinates(x)[,1]-coordinates(x)[1,1]) , diff(coordinates(x)[,2]-coordinates(x)[1,2]))
  t <- replicate(repeats, apply(rbind(coordinates(x)[1,], coordinateDiffs[sample(1:(n.locs(x)-1)),]), 2, cumsum), simplify=F)
  options(warn=-1)
  MO <- moveStack(lapply(t, function(s) move(x=s[2:(n.locs(x)-1),1], y=s[2:(n.locs(x)-1),2], 
                                             time=timestamps(x)[2:(n.locs(x)-1)], 
                                             data=as.data.frame(s[2:(n.locs(x)-1),]), 
                                             animal="RandTrack")))
  options(warn=0)
  return(MO)
}
CillaThin <- Cilla[seq(1, n.locs(Cilla), 6),]
randCilla <- rand.seg(CillaThin, 50)
used <- extract(EnvK.projPCA$map, Cilla, method="bilinear")
used <- data.frame(used, x=coordinates(Cilla)[,1],
                   y=coordinates(Cilla)[,2], 
                   ID="Cilla", pres=1)

avail <- extract(EnvK.projPCA$map, randCilla, method="bilinear")
avail <- data.frame(avail, x=coordinates(randCilla)[,1],
                    y=coordinates(randCilla)[,2], 
                    ID="randCilla", pres=0)
avail <- avail[!duplicated(avail[, c("x", "y")]), ]
data <- rbind(used, avail)
Mod <- glm(pres~PC1+PC2+PC3, 
           data=data, family="binomial")
summary(Mod)

#############################
library(scales)
plot(randCilla, type="n", xlab=NA, ylab=NA, xaxt="n", yaxt="n")
plot(EnvK.projPCA$map[[2]], add=T, col=gray(8:1/8))
lines(randCilla, col="black")
lines(Cilla, col="white")
legend("bottomleft", c("Random tracks", "Actual track"), lty=c(1,1), col=c("black", "white"), bty="o", bg=alpha("grey", 0.75), cex=0.66, box.col=alpha("grey", 0.75))
mtext("Gradient of PC2")

#######################
library(adehabitatHR)
mcpB <- mcp(buffalo.proj, percent=100)
used <- extract(EnvK.projPCA$map, buffalo.proj, method="bilinear")
used <- data.frame(used, x=coordinates(buffalo.proj)[,1],
                   y=coordinates(buffalo.proj)[,2], 
                   ID=as.character(buffalo.proj$individual.local.identifier))
used$pres <- 1
randLoc <- spsample(mcpB, sum(unlist(n.locs(buffalo.proj)))*10, "random")
avail <- extract(EnvK.projPCA$map, randLoc, method="bilinear")
avail <- data.frame(avail, coordinates(randLoc), 
                    ID=as.character(buffalo.proj$individual.local.identifier))
avail$pres <- 0
data <- rbind(used, avail)

data <- data[!duplicated(data[, c("x", "y")]),]


library(MASS)
library(nlme)
spForm <- corSpher(form=~x+y)
MixMod <- glmmPQL(pres~PC1+PC2+PC3, 
                  random = list(~ 1 | ID, ~ PC2|ID),
                  data=data,
                  family="binomial", correlation = spForm)
summary(MixMod)
