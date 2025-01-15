

#############################################
#importing and processing data


#this package has the github function
#install.packages("devtools")

#install package WildRtrax
#remotes::install_github("ABbiodiversity/wildrtrax", force = TRUE)

#load the package
library(devtools)
library(wildrtrax)
library(dplyr)
library(tidyverse)
library(sf)
library(terra)


#load species data
species_all <- read.csv("~/Desktop/Analysis/Learning/learning/raw data/NWTBM_Thaidene_Nëné_Biodiversity_Project_2021_tag_report.csv")

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
  select(2,3,4,5,28,31,41)


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

#loading in camera locations

camera_locations <- read_csv("SpeciesRawData (Oct 31)/NWTBM_Thaidene_Nëné_Biodiversity_Project_2021_location_report.csv") %>%
  drop_na("longitude") %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326) %>%
  st_transform(32612) #this number corresponds to the epsg code for utm code 12

plot(camera_locations)

#creating 300 m buffers for cameras
camera_buffer <- st_buffer(camera_locations, 300)
plot(camera_buffer)

SCANFI_landcover <- rast("raw data/SCANFI_att_nfiLandCover_SW_2020_v1.2.tif")

plot(SCANFI_landcover)

SCANFI_landcover_cropped <- crop(SCANFI_landcover, camera_buffer) #SCANFI_landcover_cropped is the scanfi data with the camera buffers
plot(SCANFI_landcover_cropped)   

Camera_buffer_zones <- rasterize(vect(camera_buffer), SCANFI_landcover_cropped, field = "location") 
plot(Camera_buffer_zones) 

overlap_SCANFI_cameras <- crosstab(c(Camera_buffer_zones, SCANFI_landcover_cropped), long = TRUE) 

overlap_SCANFI_cameras_table <- overlap_SCANFI_cameras %>%
  as_tibble() %>%
  group_by(location) %>%
  mutate(landcover_prop = n/sum(n)) #n is the number of pixels for each habitat type divided by all habitat types 

#meeting with ammaan and nick
SCANFI_proportions_camera_buffer

 
 
 
 
 
 
 
 


