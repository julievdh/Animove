
## ---- Load package --------------------------------------------------------------------------
library("move")

## ---- check version -------------------------------------------------------------------------
sessionInfo()


## ---- 1. Reading in a .csv file downloaded from a Movebank ----------------------------------
setwd("/home/anne/ownCloud/Animove17_SafiLab/LectureAnne/")
Bats <- move("./data/Parti-colored bat Safi Switzerland.csv")
Bats

## ---- also read tar-compressed csv exports or EvData .zip files -----------------------------
file.info("data/Leo-65545.csv")$size
file.info("data/Leo-65545.csv.gz")$size

Leo <- move("data/Leo-65545.csv.gz")


## ---- 2. Directly downloading data from Movebank -------------------------------------------
# store the movebank credentials
cred <- movebankLogin(username="R-Book", password="Obstberg1")
cred <- movebankLogin()

# search for studies using keywords
searchMovebankStudies(x="Parti-colored bat", login=cred)

#### ---- set the curlHandle if necessary ---------------------------#######
curl <- getCurlHandle()
options(RCurlOptions = list(capath = system.file("CurlSSL", "cacert.pem",
                                                 package = "RCurl"),
                            ssl.verifypeer = FALSE, ssl.verifyhost = FALSE))
curlSetOpt(.opts = list(proxy = 'proxyserver:port'), curl = curl)
##### ---------------------------------------------------------------######

## ---- browse the database --------------------------------------------------------------
# get the meta data of the study
getMovebankStudy(study="Parti-colored bat Safi Switzerland",login=cred)

# check for information of the animals
getMovebankAnimals(study="Parti-colored bat Safi Switzerland",login=cred)[1:3,]

## ----Download the location data ------------------------------------------------------
# get the all data
Bats <- getMovebankData(study="Parti-colored bat Safi Switzerland", login=cred)
Bats

# get only bat "191"
bat191 <- getMovebankData(study="Parti-colored bat Safi Switzerland",
                         animalName="191", login=cred)
bat191

# get data for a specific time range e.g. between "2002-06-02 23:06:15"
# and "2002-06-11 22:18:25". Time format: 'yyyyMMddHHmmssSSS'
bats.subset <- getMovebankData(study="Parti-colored bat Safi Switzerland",
                              login=cred,timestamp_start="20020602230615000",
                              timestamp_end="20020611221825000")
bats.subset



## ---- 3. Creating a move object from any data set: ------------------------------------
# read the data and store in a data frame
file <- read.table(system.file("extdata","leroy.csv.gz", package="move"), 
                   header=TRUE, sep=",", dec=".")

# convert a data frame into a move object
Leroy <- move(x=file$location.long,y=file$location.lat,
              time=as.POSIXct(file$timestamp,format="%Y-%m-%d %H:%M:%S",tz="UTC"),
              data=file,proj=CRS("+proj=longlat"),
              animal="Leroy", sensor="gps")
Leroy

## ----computer time zome ----------------------------------------------------------------
Sys.time()


## ---- check projection -------------------------------------------------------------------
projection(Leroy)


## ---- reproject -----------------------------------------------------------------------
LeroyProj <- spTransform(Leroy, CRSobj="+proj=utm +zone=18 +north +ellps=WGS84 +datum=WGS84")
projection(LeroyProj)




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

# A while loop will ensure that the loop continues untill each duplicate is removed
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
Leroy$speed <- c(speed(Leroy), NA)
Leroy1 <- Leroy[!is.na(Leroy$speed),]
LeroyFast <- Leroy[Leroy1$speed > quantile(Leroy1$speed, probs=0.95),]
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

# current version on cran of "ggmap" seems to be causing some trouble,
# try installing ggmap from github:
library("devtools")
devtools::install_github("dkahle/ggmap")


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
buffalo.split <- split(buffalo)

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
LeroyTs <- timestamps(Leroy)
Leroy$sunrise<-sunriset(Leroy,LeroyTs,POSIXct.out=T, direction='sunrise')$time
Leroy$sunset<-sunriset(Leroy,LeroyTs,POSIXct.out=T, direction='sunset')$time

Leroy$DayTime <- "Night"
Leroy$DayTime[LeroyTs<Leroy$sunset & LeroyTs>Leroy$sunrise] <- "Day"

table(Leroy$DayTime)

LeroyBurst<-burst(Leroy,f=Leroy$DayTime[-n.locs(Leroy)])
LeroyBurst

plot(LeroyBurst,type="o", lwd=2, pch=20, cex=.7)


## ---- Outputting data-------------------------------------------------------------------------
#save the move object for later
save(buffalo, file="buffalo_cleaned.Rdata")

# save as a text file
buffaloDF <- as.data.frame(buffalo)
write.table(buffaloDF, file="buffalo_cleaned.csv", sep=",", row.names = FALSE)

# save as a shape file
writeOGR(buffalo, "./", layer="buffalo", driver="ESRI Shapefile")

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
