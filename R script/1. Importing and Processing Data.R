

#############################################
#importing and processing data


#this package has the github function
#install.packages("devtools")

#install package WildRtrax
#remotes::install_github("ABbiodiversity/wildrtrax", force = TRUE)

#load the package
install.packages("writexl")
install.packages("ggplot2")
install.packages("ggspatial")
install.packages("sf")
install.packages("prettymapr")
install.packages("rnaturalearth")
install.packages("rnaturalearthdata")


library(devtools)
library(wildrtrax)
library(dplyr)
library(png)
library(writexl)
library(RColorBrewer)
library(terra)
library(raster)
library(ggspatial)
library(sf)
library(prettymapr)
library(grid)
library(tidyverse)
library(rnaturalearth)
library(rnaturalearthdata)
library(tidyterra)

setwd("~/Desktop/Analysis/Learning/learning/spatial")


#load species data
species_all <- read.csv("SpeciesRawData (Oct 31)/SPECIES_ALL_LIST.csv")

#create independent detections
species_ind_data <- wt_ind_detect(
  x = species_all,
  threshold = 30,
  units = "minutes",
  datetime_col = image_date_time,
  remove_human = TRUE,
  remove_domestic = TRUE)

#Summarising the independent detections by month

summarised_month <- wt_summarise_cam(
  # Supply your detection data
  detect_data = species_ind_data,
  # Supply your raw image data
  raw_data = species_all,
  # Now specify the time interval you're interested in
  time_interval = "month",
  # What variable are you interested in?
  variable = "detections",
  # Your desired output format (wide or long)
  output_format = "wide",
  start_col_det = "start_time")


#colnames(species_ind_det)
#head(species_ind_det)
summarised_week <- species_ind_data

#Summarising the independent detections by week
summarised_week <- wt_summarise_cam(
  # Supply your detection data
  detect_data = species_ind_data,
  # Supply your raw image data
  raw_data = species_all,
  # Now specify the time interval you're interested in
  time_interval = "week",
  # What variable are you interested in?
  variable = "detections",
  # Your desired output format (wide or long)
  output_format = "wide",
  start_col_det = "start_time")


##pulling out the camera ID, week, n effort, griz and gray wolf and muskox 

# Select the first and second columns
selected_mammals_week <- summarised_week %>%
  dplyr::select(2,3,4,5,14,28,31,41)


# Extract last 2 characters from the 'location' column because it is the camera site and create a new column 'camera'
selected_mammals_week <- selected_mammals_week%>%
  mutate(camera = substr(location, nchar(location) - 1, nchar(location)))

# Extract first 11 characters from the 'location' column and create a new column 'cluster'
selected_mammals_week <- selected_mammals_week%>%
  mutate(cluster = substr(location, 1, 11))

#renaming columns 
# Rename column 'old_name' to 'new_name'
selected_mammals_week <- selected_mammals_week%>%
  rename("gray_wolf" = "Gray Wolf")

selected_mammals_week <- selected_mammals_week%>%
  rename("grizzly_bear" = "Grizzly Bear")


#loading in tdn boundary

TDN_boundary <- st_read("~/Desktop/Analysis/Learning/learning/spatial/shapefiles/TDN_Boundary.shp")


tdn_raw_camera <- read.csv("~/Desktop/Analysis/Learning/learning/SpeciesRawData (Oct 31)/SPECIES_ALL_LIST.csv")

camera_locations <- read_csv("~/Desktop/Analysis/Learning/learning/SpeciesRawData (Oct 31)/NWTBM_Thaidene_Nëné_Biodiversity_Project_2021_location_report.csv") %>%
  drop_na("longitude") %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326) %>%
  st_transform(32612) #this number corresponds to the epsg code for utm code 12

#creating 300 m buffers for cameras
camera_buffer <- st_buffer(camera_locations, 300)

SCANFI_landcover <- rast("~/Desktop/Analysis/Learning/learning/raw data/SCANFI_att_nfiLandCover_SW_2020_v1.2.tif")

SCANFI_landcover_cropped <- crop(SCANFI_landcover, TDN_boundary %>% st_transform(crs(SCANFI_landcover))) %>% 
  project("EPSG:32612", method = "near") #SCANFI_landcover_cropped is the scanfi data with the camera buffers

cats <- data.frame(id = 1:8, cover = c("Bryoid", "Herbs", "Rock", "Shrub",
                                       "Treed broadleaf", "Treed conifer",
                                       "Treed mixed", "Water"))
coltab <- data.frame(id = 1:8, cover = c("#408A73","#BAD48F","#A8ABAE","#B38A33",
                                         "#148C3D","#003D00","#5C752B","#4C70A3"))
levels(SCANFI_landcover_cropped) <- cats
coltab(SCANFI_landcover_cropped) <- coltab
writeRaster(SCANFI_landcover_cropped, "spatial/SCANFI_landcover_cropped.tif", datatype = "INT1U", overwrite=TRUE)
SCANFI_landcover_cropped <- rast("spatial/SCANFI_landcover_cropped.tif")



cropped_SCANFI_TDN_Boundary <- crop(SCANFI_landcover_cropped, TDN_boundary, mask = TRUE)

fire_rast_cropped_TDN <- crop(fire_rast, TDN_boundary, mask = TRUE)

Camera_buffer_zones <- rasterize(vect(camera_buffer), SCANFI_landcover_cropped, field = "location") 

overlap_SCANFI_cameras <- crosstab(c(Camera_buffer_zones, SCANFI_landcover_cropped), long = TRUE) 

overlap_SCANFI_cameras_table <- overlap_SCANFI_cameras %>%
  as_tibble() %>%
  group_by(location) %>%
  mutate(landcover_prop = n/sum(n)) #n is the number of pixels for each habitat type divided by all habitat types 

#loading in esker data

esker_data <- st_read("spatial/shapefiles/Linear_Surficial_Features_of_Canada_(Canadian_Geoscience_Map_195).shp")

#loading in DEM data from erics google drive folder

TDN_DEM <- read.csv("~/Desktop/Analysis/Learning/learning/spatial/shapefiles/TDN_DEM.tif") #not sure if this worked

#load data
TDN_DEM<-raster("~/Desktop/Analysis/Learning/learning/spatial/shapefiles/TDN_DEM.tif")
 
#loading in nfdb shapefiles
#nfdb <- st_read("~/Desktop/Analysis/Learning/learning/spatial/shapefiles/NFDB_poly/NFDB_poly_20210707.shp")

#loading in nwt ecoregions
nwt_ecoregions <- st_read("spatial/shapefiles/FMD_NWT_EcoRegions.shp")

#loading in nwt boundary 
nwt_boundary <- st_read("~/Desktop/Analysis/Learning/learning/spatial/shapefiles/gpr_000a11a_e.shp")

NBAC <- st_read("~/Desktop/Analysis/Learning/learning/spatial/shapefiles/nbac_1972_2023_20240530_shp/nbac_1972_2023_20240530.shp")

#reading in national fire database fire polygon data 
#National_fire_database <- st_read("~/Desktop/Analysis/Learning/learning/spatial/shapefiles/NFDB_poly_large_fires 2/NFDB_poly_20210707_large_fires.shp")

load("~/Desktop/Analysis/Learning/learning/spatial/shapefiles/nbac_fire_TDN.RData")








