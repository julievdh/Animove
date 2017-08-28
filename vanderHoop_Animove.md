vanderHoop\_Animove
================
Julie van der Hoop
August 23, 2017

R Markdown
----------

This will be info on what we've done at Animove.

Here are all the packages to install: install.packages(c("moveVis","pbs","dplyr","RStoolbox"))

``` r
library(moveVis)
get_imconvert
```

    ## function (dir = "auto") 
    ## {
    ##     log_level <- 1
    ##     out <- function(input, type = 1) {
    ##         signs <- c("[LOG]: ", "[WARNING]: ")
    ##         if (type == 2 & log_level <= 2) {
    ##             warning(paste(signs[2], input))
    ##         }
    ##         else {
    ##             if (type == 3) {
    ##                 stop(input, call. = FALSE)
    ##             }
    ##             else {
    ##                 if (log_level == 1) {
    ##                   cat(paste(signs[1], input), sep = "\\n")
    ##                 }
    ##             }
    ##         }
    ##     }
    ##     if (dir == "auto") {
    ##         dir <- tempdir()
    ##     }
    ##     if (.Platform$OS.type == "windows") {
    ##         if (length(grep("convert.exe", list.files(paste0("C:/Program Files/", 
    ##             grep("ImageMagick", list.files("C:/Program Files/"), 
    ##                 value = TRUE))))) != 0) {
    ##             conv_dir <- paste0("C:/Program Files/", list.files("C:/Program Files/")[grep("ImageMagick", 
    ##                 list.files("C:/Program Files/"))], "/convert.exe")
    ##         }
    ##         else {
    ##             if (!file.exists(paste0(dir, "/imagick/convert.exe"))) {
    ##                 print("Downloading portable ImageMagick...")
    ##                 ftp.dir <- "ftp://ftp.imagemagick.org/pub/ImageMagick/binaries/"
    ##                 zip.dir <- grep(".zip$", grep("portable", unlist(strsplit(getURL(ftp.dir, 
    ##                   dirlistonly = TRUE), "[\\\\\\\\]|[^[:print:]]", 
    ##                   fixed = FALSE)), value = TRUE), value = TRUE)
    ##                 f.dir <- grep(unlist(strsplit(Sys.getenv("R_ARCH"), 
    ##                   "/"))[2], zip.dir, value = TRUE)
    ##                 f.dir <- paste0(ftp.dir, f.dir[length(f.dir)])
    ##                 download.file(url = f.dir, destfile = paste0(dir, 
    ##                   "/imagick.zip"), method = "auto")
    ##                 unzip(paste0(dir, "/imagick.zip"), exdir = paste0(dir, 
    ##                   "/imagick"))
    ##                 file.remove(paste0(dir, "/imagick.zip"))
    ##             }
    ##             conv_dir <- paste0(dir, "\\\\imagick\\\\convert.exe")
    ##         }
    ##         return(conv_dir)
    ##     }
    ##     else {
    ##         tryit <- try(system("convert", ignore.stdout = TRUE, 
    ##             ignore.stderr = TRUE))
    ##         if (tryit != 1) {
    ##             out("No ImageMagick installation could be found. Please install manually.", 
    ##                 type = 1)
    ##             out("On Linux, open the terminal, enter 'sudo apt-get install imagemagick' and then rerun get_imconvert().", 
    ##                 type = 1)
    ##             out("On other systems, install manually from 'https://www.imagemagick.org/script/download.php' and then rerun get_imconvert().", 
    ##                 type = 1)
    ##         }
    ##         else {
    ##             conv_dir <- "convert"
    ##             return(conv_dir)
    ##         }
    ##     }
    ## }
    ## <environment: namespace:moveVis>

Including Plots
---------------

You can also embed plots, for example:

![](vanderHoop_Animove_files/figure-markdown_github-ascii_identifiers/pressure-1.png)

Note that the `echo = FALSE` parameter was added to the code chunk to prevent printing of the R code that generated the plot.
