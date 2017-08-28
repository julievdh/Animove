
## ---- Load package --------------------------------------------------------------------------
library("move")

## ---- check version -------------------------------------------------------------------------
sessionInfo()


## ---- 1. Reading in a .csv file downloaded from a Movebank ----------------------------------
setwd("/Users/julievanderhoop/Documents/R/Animove/Animove/")


## ---- 2. Directly downloading data from Movebank -------------------------------------------
# store the movebank credentials
# cred <- movebankLogin(username="jvanderhoop", password="RezTm4SUKq")
# cred <- movebankLogin() # alternative if you want to share script and not your password

# get data for a specific time range e.g. between "2002-06-02 23:06:15"
# and "2002-06-11 22:18:25". Time format: 'yyyyMMddHHmmssSSS'
# bats.subset <- getMovebankData(study="Parti-colored bat Safi Switzerland",
#                              login=cred,timestamp_start="20020602230615000",
#                              timestamp_end="20020611221825000")
# bats.subset



## ---- 3. Creating a move object from any data set: ------------------------------------
# read the data and store in a data frame

library('R.matlab')
mymoveobjects<-list()
# dtagfile <- readMat('./data/sw17_193atrk.mat')
myfilenames<-c("sw17_193a","sw17_196a","sw17_204a","sw17_225a")
for(i in myfilenames){
dtagfile <- readMat(paste('./data/',i,'trk.mat',sep=''))
# convert into a data frame
dtagfile$timestamp <- as.POSIXct((dtagfile$t - 719529)*86400, origin = "1970-01-01 00:00:00", tz = "UTC",format ="%Y-%m-%d %H:%M:%S")
dtagfile1 <- as.data.frame(dtagfile)
# convert MATLAB time time
dtagimport <- move(x=dtagfile1$lon,y=dtagfile1$lat,
                  time=dtagfile1$timestamp,
                  data=dtagfile1,proj=CRS("+proj=longlat"),
                  animal="sw17_193a", sensor="gps")

mymoveobjects[[i]]<-dtagimport
}

# make a move stack 
mymovestack <- moveStack(mymoveobjects, forceTz="UTC")


## ---- buffalo data -----------------------------------------------------------------------
buffalo <- move("./data/Kruger African Buffalo, GPS tracking, South Africa.csv.gz")

# create a data frame
bf <- 'data/Kruger African Buffalo, GPS tracking, South Africa.csv.gz'
buffalo.df <- read.csv(bf, as.is=TRUE)

#get a quick overview
head(buffalo.df, n=2)

# first make sure the date/time is correct
buffalo.df$timestamp <- as.POSIXct(buffalo.df$timestamp, format="%F %T ", tz="UTC")

# also ensure that timestamps and individuals are ordered
buffalo.df <- buffalo.df[order(buffalo.df$individual.local.identifier, buffalo.df$timestamp),]

## ----duplicated timestamps --------------------------------------------------------------
# get the duplicated timestamps
dup <- getDuplicatedTimestamps(buffalo.df)
dup[1]

# get an overview of the amount of duplicated timestamps
table(unlist(lapply(dup,function(x)length(x)))) 

buffalo.clean <- buffalo.df

# A while loop will ensure that the loop continues until each duplicate is removed
## ==> loop starts here
while(length(dup <- getDuplicatedTimestamps(buffalo.clean))>0){
allrowsTOremove <- lapply(1:length(dup), function(x){
  rown <- dup[[x]]
   # checking if the positions are exaclty the same for all timestamps
  if(any(duplicated(buffalo.clean[rown,c("timestamp", "location.long", "location.lat", "individual.local.identifier")]))){
    dup.coor <- duplicated(buffalo.clean[rown,c("timestamp", "location.long", "location.lat", "individual.local.identifier")])
    rowsTOremove <- rown[dup.coor] # remove duplicates
  }else{
      # subset for the individual, as distances should be measured only within the individual
      # create a row number ID to find the duplicated time stamps in the subset per individual
      buffalo.clean$rowNumber <- 1:nrow(buffalo.clean)
      ind <- unlist(strsplit(names(dup[x]),split="|", fixed=T))[1]
      subset <- buffalo.clean[buffalo.clean$individual.local.identifier==ind,]

      # if the duplicated positions are in the middle of the table
      if(subset$rowNumber[1]<rown[1] & subset$rowNumber[nrow(subset)]>max(rown)){
        # calculate total distance throught the first alternate location
        dist1 <- sum(distHaversine(subset[subset$rowNumber%in%c((rown[1]-1),(max(rown)+1)),c("location.long", "location.lat")],
                                   subset[subset$rowNumber==rown[1],c("location.long", "location.lat")]))
         # calculate total distance throught the second alternate location
        dist2 <- sum(distHaversine(subset[subset$rowNumber%in%c((rown[1]-1),(max(rown)+1)),c("location.long", "location.lat")],
                                    subset[subset$rowNumber==rown[2],c("location.long", "location.lat")]))
          # omit the aternate location that produces the longer route
        if(dist1<dist2){rowsTOremove <- rown[2]}else{rowsTOremove <- rown[1]}
      }

      # incase the duplicated timestamps are the first positions
      if(subset$rowNumber[1]==rown[1]){
        dist1 <- sum(distHaversine(subset[subset$rowNumber==(max(rown)+1),c("location.long", "location.lat")],
                                   subset[subset$rowNumber==rown[1],c("location.long", "location.lat")]))
        dist2 <- sum(distHaversine(subset[subset$rowNumber==(max(rown)+1),c("location.long", "location.lat")],
                                   subset[subset$rowNumber==rown[2],c("location.long", "location.lat")]))
        if(dist1<dist2){rowsTOremove <- rown[2]}else{rowsTOremove <- rown[1]}
      }

       # incase the duplicated timestamps are the last positions
      if(subset$rowNumber[nrow(subset)]==max(rown)){
        dist1 <- sum(distHaversine(subset[subset$rowNumber==(rown[1]-1),c("location.long", "location.lat")],
                                    subset[subset$rowNumber==rown[1],c("location.long", "location.lat")]))
        dist2 <- sum(distHaversine(subset[subset$rowNumber==(rown[1]-1),c("location.long", "location.lat")],
                                   subset[subset$rowNumber==rown[2],c("location.long", "location.lat")]))
       if(dist1<dist2){rowsTOremove <- rown[2]}else{rowsTOremove <- rown[1]}
      }
    }
return(rowsTOremove)
})
buffalo.clean <- buffalo.clean[-unique(sort(unlist(allrowsTOremove))),]
buffalo.clean$rowNumber <- NULL
}
## ==> and ends here

# define the data.frame as a move object after cleaning
buffalo <- move(x=buffalo.clean$location.long,
                y=buffalo.clean$location.lat,
                time=buffalo.clean$timestamp,
                data=buffalo.clean,
                proj=CRS("+proj=longlat +datum=WGS84"),
                animal=buffalo.clean$individual.local.identifier,
                sensor=buffalo.clean$sensor.type)


## ---- Summary overview --------------------------------------------------------------------
Leroy

## ---- structure -----------------------------------------------------------------------------
str(Leroy)

## ----  access the information ---------------------------------------------------------------
timestamps(Leroy)[1:5]
coordinates(Leroy)[1:5,]
idData(Leroy)

## ---- extract properties  ------------------------------------------------------------------
n.locs(Leroy)
timeLag(Leroy, units='mins')[1:5]
speed(Leroy)[1:5]
distance(Leroy)[1:5]


## ----subseting ---------------------------------------------------------------------------------
# for example, lets make a subset with the locations
# with the highest 5% of the speeds
Leroy$speed <- c(speed(Leroy), NA) # adding this NA makes the vector the same length as the locations
Leroy1 <- Leroy[!is.na(Leroy$speed),]
LeroyFast <- Leroy[Leroy1$speed > quantile(Leroy1$speed, probs=0.95),] # take the 95th percentile (fastest speeds)
LeroyFast

## ---- basic ploting --------------------------------------------------------------------------
plot(Leroy)
lines(Leroy)
plot(Leroy, type ='l')

plot(Leroy, type ='l', lwd=4, col='blue')
points(Leroy, pch=16)


## ----Plot with various backgrounds ------------------------------------------------------------
library("ggmap")
map<-get_map(bbox(extent(Leroy)*2), source='google')
dataDf<-as.data.frame(Leroy)
ggmap(map)+
geom_path(data=dataDf, aes(x=coords.x1, y=coords.x2))+
xlab('Longitude')+ylab('Latitude')

map<-get_map(bbox(extent(mymovestack)*2), source='google')
dataDf<-as.data.frame(mymovestack)
ggmap(map)+
  geom_path(data=dataDf, aes(x=coords.x1, y=coords.x2))+
  xlab('Longitude')+ylab('Latitude')

# current version on cran of "ggmap" seems to be causing some trouble,
# try installing ggmap from github:
# library("devtools")
# devtools::install_github("dkahle/ggmap")


## ---- movestack -----------------------------------------------------------------------------
class(buffalo)
levels(buffalo@trackId)

# get a specific individual
queen <- buffalo[['Queen']]
queen
# or several
CillaGabs <- buffalo[[c("Cilla",'Gabs')]]
CillaGabs

## ---- split a movestack --------------------------------------------------------------------------
buffalo.split <- split(buffalo) # this can be useful for L-apply (for-loops)

## ---- stack a list of move objects ---------------------------------------------------------
buffalo.stk <- moveStack(buffalo.split, forceTz="UTC")
buffalo.stk@timestamps[1]

buffalo.stk2 <- moveStack(buffalo.split)
buffalo.stk2@timestamps[1]

buffalo.stk3 <- moveStack(list(CillaGabs,queen),forceTz="UTC")

## ----plot movestack -----------------------------------------------------------------------------
plot(buffalo, type="l")

## ---- Segmentation ----------------------------------------------------------------------------------
library("maptools")
mymoveobjects$sw17_196aTs <- timestamps(mymoveobjects$sw17_196a)
mymoveobjects$sw17_196a$sunrise<-sunriset(mymoveobjects$sw17_196a,mymoveobjects$sw17_196aTs,POSIXct.out=T, direction='sunrise')$time
mymoveobjects$sw17_196a$sunset<-sunriset(mymoveobjects$sw17_196a,mymoveobjects$sw17_196aTs,POSIXct.out=T, direction='sunset')$time

mymoveobjects$sw17_196a$DayTime <- "Night"
mymoveobjects$sw17_196a$DayTime[mymoveobjects$sw17_196aTs<mymoveobjects$sw17_196a$sunset & mymoveobjects$sw17_196aTs>mymoveobjects$sw17_196a$sunrise] <- "Day"

table(mymoveobjects$sw17_196a$DayTime)

mymoveobjects$sw17_196aBurst<-burst(mymoveobjects$sw17_196a,f=mymoveobjects$sw17_196a$DayTime[-n.locs(mymoveobjects$sw17_196a)]) # burst does segmentation. Segments are 1 shorter than locations.
mymoveobjects$sw17_196aBurst

plot(mymoveobjects$sw17_196aBurst,type="o", lwd=2, pch=20, cex=.7)


## ---- Outputting data-------------------------------------------------------------------------
#save the move object for later
save(buffalo, file="./data/buffalo_cleaned.Rdata")

# save as a text file
buffaloDF <- as.data.frame(buffalo)
write.table(buffaloDF, file="./data/buffalo_cleaned.csv", sep=",", row.names = FALSE)

# save as a shape file
# writeOGR(buffalo, "./", layer="buffalo", driver="ESRI Shapefile")

## ----kml or kmz of movestack ---------------------------------------------------------------
library("plotKML")
# open a file to write the content
kml_open('buf.kml')
# write the movement data individual-wise
for(i in levels(trackId(buffalo)))
  kml_layer(as(buffalo[[i]],'SpatialLines'))
# close the file
kml_close('buf.kml')


## ----export KML using writeOGR--------------------------------------------------------------------
for(i in 1:nrow(buffalo@idData))
{
  writeOGR(as(buffalo[[i]], "SpatialPointsDataFrame"),
           paste(row.names(buffalo@idData)[i],
                 ".kml", sep=""),
           row.names(buffalo@idData)[i], driver="KML")

  writeOGR(as(buffalo[[i]], "SpatialLinesDataFrame"),
         paste(row.names(buffalo@idData)[i],
               "-track.kml", sep=""),
         row.names(buffalo@idData)[i], driver="KML")

  print(paste("Exported ", row.names(buffalo@idData)[i],
            " successfully.", sep=""))
}


## ---- A. Download non location data in move object ----------------------------------------------------
stork <- getMovebankData(study="MPIO white stork lifetime tracking data (2013-2014)",login=cred,
                         animalName="DER AR439",includeExtraSensors=TRUE)

str(stork)


## ----extract the data frame containing the data for the non-location sensors-----------------------------
stork.acc <- as.data.frame(unUsedRecords(stork))
str(stork.acc)

## ----B. Download data as a data.frame ----------------------------------------------------------------------
acc <- getMovebankNonLocationData(study="MPIO white stork lifetime tracking data (2013-2014)",
                                  sensorID="Acceleration",
                                  animalName="DER AR439", login=cred)
str(acc)

## ---- sensors available in a specific study ---------------------------------------------------------
getMovebankSensors(study="MPIO white stork lifetime tracking data (2013-2014)", login=cred)[1:10,]

## ----all available sensor types on Movebank-----------------------------------------------------------
getMovebankSensors(login=cred)[,3:5] 


## ---- download data from Movebank Data Repository -----------------------------------------------------
repos <- getDataRepositoryData("doi:10.5441/001/1.2k536j54")
repos
