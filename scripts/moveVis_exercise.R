#How to use moveVis
#---------------------------

#[1] PACKAGES ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#If not already installed, get the newest versions of "moveVis", "move" and "raster"
install.packages(c("moveVis","move","raster"))

#Load the packages
library(moveVis); library(move); library(raster)



#[2] DATA ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#Get the sample movement data from the moveVis package
data("move_data") # white stork data package -- data frame

#Convert the timestamps to the POSIXct format
move_data$dt <- as.POSIXct(strptime(move_data$dt, "%Y-%m-%d %H:%M:%S", tz = "UTC"))

#Create a moveStack class object
data_ani <- move(move_data$lon, move_data$lat, proj=CRS("+proj=longlat +ellps=WGS84"),
                       time = move_data$dt, animal=move_data$individual, data=move_data)



#[3] PREPARING animate_move() CALL +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#Find command or directory to convert tool of ImageMagick
conv_dir <- get_imconvert() #or: conv_dir <- "/dir/to/convert"

#Specify output directory
out_dir <- "/Users/julievanderhoop/Documents/R/Animove/Animove/" #or

#Specify some optional appearance variables such as title etc.
img_title <- "Movement of the White Stork population at Lake Constance, Germany"
img_sub <- paste0("including individuals ",paste(rownames(idData(data_ani)), collapse=', '))
img_caption <- "Projection: Geographical, WGS84; Sources: Movebank 2013; Google Maps"


#[6] CALLING animate_move() ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#(a) Let's create a simple animation first, paths mode: true_data
animate_move(data_ani, out_dir, conv_dir,
             paths_mode = "true_data",
             img_title = img_title, img_sub = img_sub, img_caption = img_caption,
             frames_nmax = 50)

## TRY IT 
load("./data/swstack.RData")
plot(swstack)
#Specify some optional appearance variables such as title etc.
img_titlesw <- "Movement of sperm whales in the Azores, sw17"
img_subsw <- paste0("Individuals ",paste(rownames(idData(swstack)), collapse=', '))
img_captionsw <- "Projection: Geographical, WGS84; Sources: Movebank 2013; Google Maps"

animate_move(swstack, out_dir, conv_dir,
             paths_mode = "simple", # start all the whales at the same time
             out_name = "swfinal_gif", # change output file title
             frames_interval = 1, # change the frame rate
             img_title = img_titlesw, img_subsw = img_sub, img_captionsw = img_caption,
             frames_nmax = 50)

#(b) Use a different path mode and check out the difference: simple
animate_move(data_ani, out_dir, conv_dir,
             paths_mode = "simple",
             img_title = img_title, img_sub = img_sub, img_caption = img_caption,
             frames_nmax = 50)

#(c) Use a different path mode and check out the difference: true_time
animate_move(data_ani, out_dir, conv_dir,
             paths_mode = "true_time",
             img_title = img_title, img_sub = img_sub, img_caption = img_caption,
             frames_nmax = 50)

#(d) Tell Google to give us a map instead of aerial/satellite imagery, and use black fonts for the elements
animate_move(data_ani, out_dir, conv_dir,
             paths_mode = "true_data", map_type = "roadmap",
             img_title = img_title, img_sub = img_sub, img_caption = img_caption,
             scalebar_col = "black", north_col = "black",
             frames_nmax = 50)



#[7] USING ENVIRONMENTAL DATA INSTEAD OF GOOGLE DATA +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#Load example MODIS NDVI data
data("basemap_data")

#Extract raster list and timestamp list
layer = basemap_data[[1]]
layer_dt = basemap_data[[2]]

#You can change the caption, since we are not using Google Maps as a source anymore
img_caption <- "Projection: Geographical, WGS84; Sources: Movebank 2013; MODIS NDVI 2013"



#[8] CALLING animate_move() ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#(a) Tell animate_move(), what type of layer you are using: gradient 
animate_move(data_ani, out_dir, conv_dir,
             layer = layer,layer_dt = layer_dt, layer_type = "gradient",
             paths_mode = "true_data",
             img_title = img_title, img_sub = img_sub, img_caption = img_caption,
             frames_nmax = 50,
             extent_factor = 0.0002) #higher the difference between your move data extent and the map extent



#[9] ADDING STATIC POINTS ON TOP OF THE MAP ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#Create a data.frame -- reference points, e.g. 
static_data <- data.frame(x = c(8.94,8.943), y = c(47.75,47.753), names = c("Station 1","Station 2"))



#[10] CALLING animate_move() +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
animate_move(data_ani, out_dir, conv_dir,
             layer = layer,layer_dt = layer_dt, layer_type = "gradient",
             paths_mode = "true_data",
             img_title = img_title, img_sub = img_sub, img_caption = img_caption,
             frames_nmax = 50,
             extent_factor = 0.0002,
             static_data = static_data)

#Create your own movement and environmental data animations. Check out the help pages for details on all
#function parameters.

