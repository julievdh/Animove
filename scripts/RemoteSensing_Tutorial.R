# Remote sensing class 

install.packages("raster")
library(raster)

germany <- getData("GADM", country = "DEU", level = 2)
plot(germany)
prec <- getData("worldclim",var = "prec", res = 0.5, lon = 10, lat = 51) # pricipitation data during these lats and lons
plot(prec) # plot the precipitation

prec_ger1 <- crop(prec,germany) # crop the precipitation to the extent of germany 
spplot(prec_ger1)

prec_ger2 <- mask(prec_ger1, germany) # mask precipitation to shape of germany
spplot(prec_ger2)

# calculate average precipitation -- other statistics are possible too 
prec_avg <- cellStats(prec_ger2,stat="mean") 

## try precipitation in the Azores 
portugal <- getData("GADM", country = "PRT", level = 1) # only need level 1 
plot(portugal)
aprec <- getData("worldclim",var = "prec", res = 0.5, lon = -25, lat = 37) # pricipitation data during these lats and lons
# crop portugal to azores
azores <- subset(portugal,NAME_1 == "Azores")
plot(azores)

prec_az1 <- crop(aprec,azores) # crop the precipitation to the azores 
spplot(prec_az1)

## --- R and Raster Data --- Benny 
# RGIS tutorial available 

# 1 --- import rasters - using data from book 
band1 <- raster("/Users/julievanderhoop/Documents/R/Animove/data_book/raster_data/final/p224r63_1988_masked.gri", band =1)
band2 <- raster("/Users/julievanderhoop/Documents/R/Animove/data_book/raster_data/final/p224r63_1988_masked.gri", band =2)

# 2 --- combine rasters
allbands <- stack(band1,band2)
# or can stack directly from files: stack(c("/path/fileA.tif","/path/fileB/tif"))
allbands <- brick("/Users/julievanderhoop/Documents/R/Animove/data_book/raster_data/final/p224r63_1988_masked.gri")

# 3 --- plot with different band ocmbinations
plotRGB(allbands, 3,2,1)
plotRGB(allbands, 3,2,1, stretch = "lin") # color stretch 
# can crop
ext <- drawExtent()
band2.crop <- crop(band2,ext)
ext <- ext*2 # change size of extent 

# or use GGplot
library(RStoolbox)
ggRGB(allbands, 3, 2, 1, stretch = "lin")
ggR(allbands, layer = 4)

# 4 --- Raster statistics 
# cellStats, summary, moran, zonal, quantile, freq 
# example: cellStats(allbands,stat = "mean") 

# 5 --- get value from raster
data(lsat); data(leroy)
env <- raster(leroy, vals = rnorm(100))

# extract pixel values for where we have animal track 
x <- extract(env, leroy) # now have one value for every line of leroy. Connect vector data to raster data 
# plot leroy coloured by x? 

# 6 --- set value examples
# lsat[] <- rnorm(ncell(lsat))
# lsat[lsat < 0] <- NA # set all to NA

# 7 --- transformation, weight averages, resample, reprojecting etc. Examples in presentation 
# 8 --- you need special saving functions 
# writeRaster(your_raster, datatype = "FLT4S", filename = "new_data.tif", format = "GTiff", overwrite = TRUE)
# calc()
# KML()


