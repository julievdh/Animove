vanderHoop\_Animove
================
Julie van der Hoop
August 23, 2017

Animove Notes - Aug 27 - 9 Sept 2017
====================================

Here are all the packages to install: install.packages(c("moveVis","pbs","dplyr","RStoolbox","move","ggmap","TwoStepCLogit","pbs","dplyr",'lubridate","fdrtool","circular","CircStats"),dependencies = T)

Lecture Notes - Kami 28 Aug 2017
================================

Lagrangian versus Eulerian methods: moving versus static. Lagrangian observe moving entity; eulerian observe space. Movement: Sequence of Location in time

Example data sets: Bat radio tracking data, African buffalo GPS data, "Leo" the vulture, "Leroy" and "Ricky" the fishers from Albany. --&gt; all on movebank.org and data associated with R library move.

This is a test

Loading Datasets
----------------

``` r
library("move")
```

    ## Loading required package: geosphere

    ## Loading required package: sp

    ## Loading required package: raster

    ## Loading required package: rgdal

    ## rgdal: version: 1.2-8, (SVN revision 663)
    ##  Geospatial Data Abstraction Library extensions to R successfully loaded
    ##  Loaded GDAL runtime: GDAL 2.1.3, released 2017/20/01
    ##  Path to GDAL shared files: /Library/Frameworks/R.framework/Versions/3.4/Resources/library/rgdal/gdal
    ##  Loaded PROJ.4 runtime: Rel. 4.9.3, 15 August 2016, [PJ_VERSION: 493]
    ##  Path to PROJ.4 shared files: /Library/Frameworks/R.framework/Versions/3.4/Resources/library/rgdal/proj
    ##  Linking to sp version: 1.2-4

``` r
# setwd("...")
# Bats <- move(...)
```

Including Plots
---------------

You can also embed plots, for example:

![](vanderHoop_Animove_files/figure-markdown_github-ascii_identifiers/pressure-1.png)

Note that the `echo = FALSE` parameter was added to the code chunk to prevent printing of the R code that generated the plot.
