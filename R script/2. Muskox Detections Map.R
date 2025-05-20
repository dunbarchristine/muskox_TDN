
library(tidyverse)
library(sf)
library(terra)

#downloads a shapefile and converts into R object
map<- read.csv('~/Desktop/Analysis/Learning/learning/Raw Data/NWTBM_muskox_month_data.csv')
tdn_boundary <- st_read("~/Desktop/Analysis/Learning/learning/Spatial/shapefiles/TDN_Boundary.shp")

#load camera sites, drop the NA sites, convert to spatial file
TDN_Cameras<-read_csv("~/Desktop/Analysis/Learning/learning/raw data/NWTBM_Thaidene_Nëné_Biodiversity_Project_2021_location_report.csv")
TDN2 <-TDN_Cameras %>% 
  filter(!is.na(latitude)) %>% 
  st_as_sf(coords=c("longitude", "latitude"), crs=4326, remove=FALSE) %>% 
  st_transform(st_crs(tdn_boundary))

#TDN_Cameras2 <- st_as_sf(TDN_Cameras, coords=c("longitude", "latitude"), crs=4326, remove=FALSE)

object_class <- class(TDN2)

#load species data
species_all <- read.csv("~/Desktop/Analysis/Learning/learning/raw data/Species Raw Data (May 2025)/NWTBM_Thaidene_Nëné_Biodiversity_Project_2021_tag_report.csv")

#QA/QC on data
#how many sites are there?
paste(length(unique(TDN_Cameras$location)))


#plot boundary (geometry column from sf) and cameras
plot(tdn_boundary$geometry)
plot(TDN2$geometry, add=TRUE)

#create column of sites, as well as location (station) so that it can be merged with the habitat file
TDN2$site <- substr(TDN2$location, 1, 11)

#QA/QC on date
#how many sites are there?
paste(length(unique(TDN2$site)))

#loading station habitat csv file
TDN_Habitat <- read_csv("Raw Data/TDN_habitat_site.csv")

#merge habitat file into camera file
TDN_Cameras_Habitat <- merge (TDN2, TDN_Habitat, by="site")

#plot boundary with cameras coloured by habitat
ggplot() +
  geom_sf(data = tdn_boundary$geometry) +
  geom_sf(data = TDN_Cameras_Habitat$geometry, aes(color = TDN_Cameras_Habitat$habitat), size = 3) +
  theme_minimal() +
  theme(
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  ) +
  labs(title = "TDN Sites by Habitat", color = "Habitat")

#separating out the data by week and month
species_all$date <- as.Date(species_all$image_date_time, format = "%Y-%m-%d")

#filtering out just muskox sightings
data <- species_all %>% mutate(year_month = floor_date(date, "month"))  %>%
  filter(species_common_name == "Muskox") %>% #if I want to filter by a species
  mutate(week = week(date), #if I want to create a column with week number
         month = month(date),
         year = year(date),
         individual_count = as.numeric(individual_count))

#averaging muskox detections by week
month_data <- data %>% 
  group_by(date, month, location) %>%
  summarise(date_count = max(individual_count)) %>%
  group_by(month, location) %>%
  summarise(month_count = mean(date_count) * 7)

#creating data frames to plot from  
data_cameras <- merge(data, TDN_Cameras, by="location")
month_data_cameras <- merge(month_data, TDN_Cameras, by="location")

#converting to sf objects
month_data_cameras_SF <-st_as_sf(month_data_cameras, coords=c("longitude", "latitude"), crs=4326, remove=FALSE)
data_cameras_SF <- st_as_sf(data_cameras, coords=c("longitude", "latitude"), crs=4326, remove=FALSE)

#Muskox count by month
ggplot() +
  geom_sf(data=tdn_boundary) +
  geom_sf(data=month_data_cameras_SF, aes(geometry = geometry, size=month_count)) +
  facet_wrap(~month, nrow = 4) +
  theme_minimal() +
  theme(
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank())


#muskox count by month with habitat layer

ggplot() +
  geom_sf(data=tdn_boundary) +
  geom_sf(data=month_data_cameras_SF, aes(geometry = geometry, size=month_count)) +
  geom_sf(data = TDN_Cameras_Habitat$geometry, color = TDN_Cameras_Habitat$habitat, size = 3) +
  facet_wrap(~month, nrow = 4) +
  theme_minimal() +
  theme(
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank())



#convert TDN habitat to an SF object so we can plot it 
class(TDN_Habitat_locations_SF) #checking if its actually an SF object
TDN_Habitat_Locations <- merge (TDN2, TDN_Habitat, by="site") #merging habitat and lat and long datasets together
TDN_Habitat_locations_SF <- st_as_sf(TDN_Habitat_Locations, coords=c("longitude", "latitude"), crs=4326, remove=FALSE) #now that its merged, turning it into an SF object

#reading in water shapefile
water_shapefile <- "~/Desktop/Analysis/Learning/learning/spatial/shapefiles/TDN_water_features.shp"
water_polygon <- st_read(water_shapefile)

#check the shapefile contents 
print(water_shapefile)
summary(water_shapefile)

#reading in treeline shapefile
shapefile_path2 <- "~/Desktop/Analysis/Learning/learning/spatial/shapefiles/Treeline_Approx.shp"
treeline_polygon <- st_read(shapefile_path2)

Habitat_types <- st_as_sf(TDN_Habitat_locations_SF, coords=c("longitude", "latitude"), crs=4326, remove=FALSE)
#Muskox count all year trying to add habitat layer 
ggplot() +
  geom_sf(data=tdn_boundary) +
  geom_sf(data=TDN_Habitat_locations_SF, fill = "lightgreen", alpha = 0.5) +  
  geom_sf(data = water_polygon, fill = 'blue', color = 'light blue') +
  geom_sf(data=data_cameras_SF, aes(geometry = geometry, size=individual_count)) +
  theme_minimal() +
  theme(
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()) 

#making maps with muskox count and habitat 
data_cameras_SF = data_cameras_SF %>% mutate(month = lubridate::month(image_date_time))

data_cameras_SF = left_join(x = data_cameras_SF, y = TDN_Habitat_locations_SF %>% st_drop_geometry() %>% dplyr::select(location, habitat), by = "location")

ggplot(data=data_cameras_SF) +
  geom_sf(data=tdn_boundary) +
  #geom_sf(aes(colour = habitat), size = 6) +
  geom_sf(data = water_polygon, fill = 'light blue') +
  geom_sf(data = treeline_polygon, color = 'red') +
  geom_sf(aes(size=individual_count, colour = habitat)) +
  facet_wrap(~month, nrow  = 4) +
  theme_minimal() +
  theme(
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid.minor = element_blank()) 


#this package has the github function
install.packages("devtools")

#install package WildRtrax
remotes::install_github("ABbiodiversity/wildrtrax", force = TRUE)

#load the package
library(devtools)
library(wildrtrax)
library(dplyr)

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


  colnames(species_ind_det)
  head(species_ind_det)
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


#let's see how the detections month data looks on a map looking at the presence of Muskox
summarised_month_cameras <- left_join(summarised_month, TDN2%>% select(location, geometry), by="location")

## 0k now delete all the months after Aug 2022
ggplot() +
  geom_sf(data=tdn_boundary) +
  geom_sf(data=summarised_month_cameras, aes(geometry = geometry,
                                                        colour = Muskox == 0, size=Muskox)) + # Color based on whether Muskox is 0
  facet_wrap(~month, nrow = 4) +
  scale_color_manual(values = c("FALSE" = "orange", "TRUE" = "lightgray")) + # Set faint color for zeros
  theme_minimal() +
  theme_minimal() +
  theme(
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank())











