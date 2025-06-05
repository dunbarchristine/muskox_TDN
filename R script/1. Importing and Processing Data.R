

#############################################
#importing and processing data


#this package has the github function
#install.packages("devtools")

install.package(WildRtrax)
remotes::install_github("ABbiodiversity/wildrtrax", force = TRUE)
devtools::install_github("ABbiodiversity/wildrtrax", force = TRUE)


#load the package
install.packages("writexl")
install.packages("ggplot2")
install.packages("ggspatial")
install.packages("sf")
install.packages("prettymapr")
install.packages("rnaturalearth")
install.packages("rnaturalearthdata")


library(DHARMa)
library(devtools)
library(wildrtrax)
library(writexl)
library(RColorBrewer)
library(terra)
library(ggspatial)
library(sf)
library(prettymapr)
library(grid)
library(tidyverse)
library(rnaturalearth)
library(rnaturalearthdata)
library(tidyterra)
library(exactextractr)
library(spatialEco)

setwd("~/Desktop/Analysis/Learning/learning/spatial")


#load species data
species_all <- read.csv("~/Desktop/Analysis/Learning/learning/Raw Data/SpeciesRawDownload/NWTBM_Thaidene_Nëné_Biodiversity_Project_2021_main_report.csv")

images <- wt_download_report(project_id = 712, sensor_id = 'CAM', reports = "main", weather_cols = F) %>%
  tibble::as_tibble()

#Authenticate in WildTrax
Sys.setenv(WT_USERNAME = 'christinedunbar3@gmail.com', WT_PASSWORD = 'Theweeknd88!')
wt_auth()

#create independent detections
species_ind_data <- wt_ind_detect(
  x = species_all,
  threshold = 30,
  units = "minutes",
  datetime_col = image_date_time,
  remove_human = TRUE,
  remove_domestic = TRUE)

#old
#Summarising the independent detections by month
summarised_month <- wt_summarise_cam(
  # Supply your detection data
  detect_data = species_ind_data,
  # Supply your raw image data
  raw_data = species_all,
  # Now specify the time interval you're interested in
  time_interval = "month",
  # What variable are you interested in?
  variable = "detections")
  # Your desired output format (wide or long)
  #output_format = "wide",
  #start_col_det = "start_time")
  #exclude_out_of_range = TRUE)

#new
# Summarising the independent detections by month
summarised_month <- wt_summarise_cam(
  detect_data = species_ind_data,
  raw_data = species_all,
  time_interval = "month",
  variable = "detections"
)


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
  variable = "detections")
  # Your desired output format (wide or long)
  # output_format = "wide",
  # start_col_det = "start_time",
  # exclude_out_of_range = TRUE)


##pulling out the camera ID, week, n effort, griz and gray wolf and muskox 
# Select the first and second columns
selected_mammals_week <- summarised_week %>%
  dplyr::select(2,3,4,5,14,27,30,40)


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


TDN_boundary <- st_read("~/Desktop/Analysis/Learning/learning/spatial/shapefiles/TDN_Boundary.shp")

tdn_raw_camera <- read.csv("~/Desktop/Analysis/Learning/learning/SpeciesRawData (Oct 31)/SPECIES_ALL_LIST.csv")

camera_locations <- read_csv("~/Desktop/Analysis/Learning/learning/Raw Data/SpeciesRawData (Oct 31)/NWTBM_Thaidene_Nëné_Biodiversity_Project_2021_location_report.csv") %>%
  drop_na("longitude") %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326) %>%
  st_transform(32612) #this number corresponds to the epsg code for utm code 12

#creating 300 m buffers for cameras
camera_buffer <- st_buffer(camera_locations, 300)

SCANFI_landcover <- rast("~/Desktop/Analysis/Learning/learning/Spatial/SCANFI/SCANFI_att_nfiLandCover_SW_2020_v1.2.tif")

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

Camera_buffer_zones <- rasterize(vect(camera_buffer), SCANFI_landcover_cropped, field = "location") 

overlap_SCANFI_cameras <- crosstab(c(Camera_buffer_zones, SCANFI_landcover_cropped), long = TRUE) 

overlap_SCANFI_cameras_table <- overlap_SCANFI_cameras %>%
  as_tibble() %>%
  group_by(location) %>%
  mutate(landcover_prop = n/sum(n)) #n is the number of pixels for each habitat type divided by all habitat types 

#loading in esker data

esker_data <- st_read("spatial/shapefiles/Linear_Surficial_Features_of_Canada_(Canadian_Geoscience_Map_195).shp")

#loading in DEM (elevation) data from erics google drive folder

TDN_DEM <- read.csv("~/Desktop/Analysis/Learning/learning/spatial/shapefiles/TDN_DEM.tif") 

#load data
TDN_DEM<- terra::rast("~/Desktop/Analysis/Learning/learning/Spatial/TDN_DEM/Copernicus_TDN_DEM.tif")

#trying a different DEM dataset because the TDN_DEM has NAs in it
ArcticDEM <- terra::rast("~/Desktop/Analysis/Learning/learning/Spatial/TDN_DEM/ArcticDEM_500m/arcticdem_mosaic_500m_v4.1_dem.tif")

#loading in nwt ecoregions
nwt_ecoregions <- st_read("~/Desktop/Analysis/Learning/learning/Spatial/shapefiles/FMD_NWT_EcoRegions.shp")

TDN_boundary <- st_transform(TDN_boundary, st_crs(nwt_ecoregions))

cropped_ecoregions_TDN_Boundary <- st_intersection(nwt_ecoregions, TDN_boundary)
# 
# tdn_ecoregions <- crop(nwt_ecoregions, TDN_boundary %>% st_transform(crs(nwt_ecoregions))) %>% 
#   project("EPSG:32612", method = "near") 

tdn_ecoregions <- st_transform(nwt_ecoregions, crs = st_crs(TDN_boundary)) %>%
  st_intersection(st_transform(TDN_boundary, crs = st_crs(nwt_ecoregions))) %>%
  st_transform(crs = 32612)  # EPSG:32612 is UTM Zone 12N

#adding tdn ecoregions to all_variables_with_tri_and_species
model_variables <- merge(all_variables_with_tri_and_species, 
                         camera_locations_df[, c("location")], 
                         by = "location", 
                         all.x = TRUE)


locs_ecoregions <- camera_locations %>%
  st_transform(crs = st_crs(cropped_ecoregions_TDN_Boundary)) #change the projection to match the raster

# Find overlaps
ecoregions_overlap <- st_join(locs_ecoregions, cropped_ecoregions_TDN_Boundary)
ecoregions_overlap <- as.data.frame(ecoregions_overlap)

# Make a new column in cameras (if needed)
model_variables <- model_variables %>% 
  left_join(ecoregions_overlap %>% select(location, ECO1_NAM_1, ECO2_NAM_1, ECO3_NAM_1, ECO4_NAM_1), by="location")

#loading in nwt boundary 
nwt_boundary <- st_read("~/Desktop/Analysis/Learning/learning/spatial/shapefiles/gpr_000a11a_e.shp")

NBAC <- st_read("~/Desktop/Analysis/Learning/learning/Spatial/shapefiles/NBAC/NBAC_1972_2024_20250506.shp")

#reading in national fire database fire polygon data 
#National_fire_database <- st_read("~/Desktop/Analysis/Learning/learning/spatial/shapefiles/NFDB_poly_large_fires 2/NFDB_poly_20210707_large_fires.shp")

load("~/Desktop/Analysis/Learning/learning/spatial/shapefiles/nbac_fire_TDN.RData")

landcover_of_canada <- rast("~/Desktop/Analysis/Learning/learning/spatial/landcover_of_canada/landcover-2020-classification.tif")

### load attribute data for land cover values
lc_atts <- read_csv("~/Desktop/Analysis/Learning/learning/ClassIndex_IndiceDeClasse.csv",
                    col_names = c("Value", "Classification", "RGB"),
                    skip = 1) %>%
  separate(RGB,c("R","G","B"),"; ", convert = TRUE) %>%
  ### remove french classification and convert rgb to hex
  mutate(Classification = str_replace_all(Classification, "[^[:alnum:]^/^-]", " "),
         Classification = str_remove_all(Classification, "/.*"),
         Classification = fct_reorder(Classification, Value),
         hex = rgb(R,G,B,maxColorValue = 255))
saveRDS(lc_atts, "~/Desktop/Analysis/Learning/learning/lc_atts.rds")

lc_cats <- data.frame(ids = lc_atts$Value, cover = lc_atts$Classification)
lc_coltab <- data.frame(ids = lc_atts$Value, cols = lc_atts$hex)

levels(landcover_of_canada) <- lc_cats
coltab(landcover_of_canada) <- lc_coltab

landcover_of_canada_cropped <- crop(landcover_of_canada, TDN_boundary %>% st_transform(crs(landcover_of_canada))) %>% 
  project("EPSG:32612", method = "near")

lcc_camera_buffers <- rasterize(vect(camera_buffer), landcover_of_canada_cropped, field = "location") 

lcc_camera_buffer_prop <- crosstab(c(lcc_camera_buffers, landcover_of_canada_cropped), long = TRUE) 

lcc_cameras_prop_table <- lcc_camera_buffer_prop %>%
  as_tibble() %>%
  group_by(location) %>%
  mutate(landcover_prop = n/sum(n)) #n is the number of pixels for each habitat type divided by all habitat types 

# Spread the data into separate columns for each land cover type
lcc_cameras_prop_columns <- lcc_cameras_prop_table %>%
  dplyr::select(-n) %>%  
  pivot_wider(
    names_from = cover,  # Create a column for each land cover type
    values_from = landcover_prop,  # Take values from the 'land_cover_prop' column
    values_fill = list(landcover_prop = 0))  # Fill missing values with 0 (no land cover)


#creating new table with just numeric variables to run correlations
lcc_cameras_prop_columns_variables_only <- lcc_cameras_prop_columns[, -1]

#uploading csv file from claudia for monthly species detections

monthly_species_detections <- read.csv("~/Desktop/Analysis/Learning/learning/712_month_detections.csv")
