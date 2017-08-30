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

