


#mapping monthyl and weekly muskox detections 

library(tidyverse)
library(sf)
library(terra)

#get shapefiles 

tdn_boundary <- st_read("~/Desktop/Analysis/Learning/learning/spatial/shapefiles/TDN_Boundary.shp")
shapefile_treeline <- "~/Desktop/Analysis/Learning/learning/spatial/shapefiles/Treeline_Approx.shp"
water_shapefile <- "~/Desktop/Analysis/Learning/learning/spatial/shapefiles/TDN_water_features.shp"


#making SF objects for non sf objects
treeline_polygon <- st_read(shapefile_treeline)
water_polygon <- st_read(water_shapefile)

#independent detection files from "importing and processing data" script
#load camera sites, drop the NA sites, convert to spatial file

#adding the camera locations to the summarized monthly data
#first, reading in the camera data file, transforming it to be an SF object, and then transforming it to the same projection as tdn_boundary
TDN_Cameras<-read_csv("~/Desktop/Analysis/Learning/learning/raw data/NWTBM_Thaidene_Nëné_Biodiversity_Project_2021_location_report.csv")
TDN2 <-TDN_Cameras %>% 
  filter(!is.na(latitude)) %>% 
  st_as_sf(coords=c("longitude", "latitude"), crs=4326, remove=FALSE) %>% 
  st_transform(st_crs(tdn_boundary))

#adding the geometry from TDN2 and the summarized month data together to make the summarised_month_cameras

# Join the 'geometry' column from 'TDN2' to 'summarised_month'
summarised_month_cameras <- left_join(summarised_month, 
                                      TDN2 %>% select(location, geometry), 
                                      by = "location")

# Now join the 'habitat' column from 'TDN_Cameras_Habitat' to the existing dataset
summarised_month_cameras <- left_join(summarised_month_cameras, 
                                      TDN_Cameras_Habitat %>% select(location, habitat), 
                                      by = "location")

### plotting monthly habitat detections of muskox ###

summarised_month_cameras3<- st_as_sf(summarised_month_cameras, coords=c("longitude", "latitude"), crs=4326, remove=FALSE)

# Convert to an sf object, assuming the 'geometry' column is already correctly named
summarised_month_cameras_sf <- st_as_sf(summarised_month_cameras, wkt = "geometry")  # or just st_as_sf() if already in the correct format

# If 'geometry' is already in 'sfc' format, just do the conversion without 'wkt'
summarised_month_cameras_sf <- st_as_sf(summarised_month_cameras)

# Filter the data to include only Muskox counts greater than 0
summarised_month_cameras_musk <- summarised_month_cameras %>%
  filter(Muskox > 0)

# Ensure summarised_month_cameras is an sf object (if it isn't already)
summarised_month_cameras <- st_as_sf(summarised_month_cameras, wkt = "geometry")  # Or just st_as_sf() if already in the correct format

# Make sure 'month' is a factor and set the levels in the correct order (e.g., from January to December)
summarised_month_cameras_musk$month <- factor(summarised_month_cameras_musk$month, 
                                         levels = c("January", "February", "March", "April", 
                                                    "May", "June", "July", "August", 
                                                    "September", "October", "November", "December"))

# Create the plot
ggplot(data = summarised_month_cameras_musk) +
  geom_sf(data = tdn_boundary) +  # Boundary polygon
  geom_sf(data = water_polygon, fill = 'light blue') +  # Water area
  geom_sf(data = treeline_polygon, color = 'red') +  # Treeline
  geom_sf(aes(geometry = geometry.y, colour = habitat, size = Muskox)) +  # Plot Muskox values > 0
  facet_wrap(~month, nrow = 4) +  # Facet by month
  theme_minimal() +
  theme(
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  )

### plotting monthly habitat detections of grizzly bears using independent detections data ###

# Make sure 'month' is a factor and set the levels in the correct order (e.g., from January to December)
summarised_month_cameras$month <- factor(summarised_month_cameras$month, 
                                                  levels = c("January", "February", "March", "April", 
                                                             "May", "June", "July", "August", 
                                                             "September", "October", "November", "December"))
# Rename column 'Grizzly Bear' to 'Grizzly_Bear'
summarised_month_cameras <- summarised_month_cameras %>% rename("Grizzly_Bear" = "Grizzly Bear")

# Filter the data to include only Grizzly Bear counts greater than 0
summarised_month_cameras <- summarised_month_cameras %>%
  filter(Grizzly_Bear > 0)

# Create the plot
ggplot(data = summarised_month_cameras) +
  geom_sf(data = tdn_boundary) +  # Boundary polygon
  geom_sf(data = water_polygon, fill = 'light blue') +  # Water area
  geom_sf(data = treeline_polygon, color = 'red') +  # Treeline
  geom_sf(aes(geometry = geometry.y, colour = habitat, size = Grizzly_Bear)) +  
  facet_wrap(~month, nrow = 4) +  # Facet by month
  theme_minimal() +
  theme(
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank())

### plotting monthly habitat detections of gray wolves using independent detections data ###

# Make sure 'month' is a factor and set the levels in the correct order (e.g., from January to December)
summarised_month_cameras$month <- factor(summarised_month_cameras$month, 
                                                  levels = c("January", "February", "March", "April", 
                                                             "May", "June", "July", "August", 
                                                             "September", "October", "November", "December"))
# Rename column 'Grizzly Bear' to 'Grizzly_Bear'
summarised_month_cameras <- summarised_month_cameras %>% rename("Gray_Wolf" = "Gray Wolf")

# Filter the data to include only Grizzly Bear counts greater than 0
summarised_month_cameras <- summarised_month_cameras %>%
  filter(`Gray_Wolf` > 0)
  
# Rename column 'Grizzly Bear' to 'Grizzly_Bear'
  summarised_month_cameras <- summarised_month_cameras %>% rename("habitat" = "habitat.y")

# Create the plot
ggplot(data = summarised_month_cameras) +
  geom_sf(data = tdn_boundary) +  # Boundary polygon
  geom_sf(data = water_polygon, fill = 'light blue') +  # Water area
  geom_sf(data = treeline_polygon, color = 'red') +  # Treeline
  geom_sf(aes(geometry = geometry.y, colour = habitat, size = Gray_Wolf)) +  
  facet_wrap(~month, nrow = 4) +  # Facet by month
  theme_minimal() +
  theme(
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank())


##plotting independent detections for grizzlies

# Rename column 'Grizzly Bear' to 'Grizzly_Bear'
summarised_month_cameras <- summarised_month_cameras %>% rename("Grizzly_Bear" = "Grizzly Bear")

# Make sure 'month' is a factor and set the levels in the correct order (e.g., from January to December)
summarised_month_cameras$month <- factor(summarised_month_cameras$month, 
                                         levels = c("January", "February", "March", "April", 
                                                    "May", "June", "July", "August", 
                                                    "September", "October", "November", "December"))


#now plotting the independent detections for grizzly bear
ggplot() +
  geom_sf(data=tdn_boundary) +
  geom_sf(data=summarised_month_cameras, aes(geometry = geometry,
                                             colour = Grizzly_Bear == 0, size=Grizzly_Bear)) + # Color based on whether Muskox is 0
  facet_wrap(~month, nrow = 4) +
  scale_color_manual(values = c("FALSE" = "orange", "TRUE" = "lightgray")) + # Set faint color for zeros
  theme_minimal() +
  theme_minimal() +
  theme(
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank())

##plotting independent detections for Gray Wolf

# Rename column 'Gray wolf' to 'Gray_Wolf'
summarised_month_cameras <- summarised_month_cameras %>% rename("Gray_Wolf" = "Gray Wolf")

# Make sure 'month' is a factor and set the levels in the correct order (e.g., from January to December)
summarised_month_cameras$month <- factor(summarised_month_cameras$month, 
                                         levels = c("January", "February", "March", "April", 
                                                    "May", "June", "July", "August", 
                                                    "September", "October", "November", "December"))

#now plotting the independent detections for gray wolf
ggplot() +
  geom_sf(data=tdn_boundary) +
  geom_sf(data=summarised_month_cameras, aes(geometry = geometry,
                                             colour = Gray_Wolf == 0, size=Gray_Wolf)) + # Color based on whether Muskox is 0
  facet_wrap(~month, nrow = 4) +
  scale_color_manual(values = c("FALSE" = "orange", "TRUE" = "lightgray")) + # Set faint color for zeros
  theme_minimal() +
  theme_minimal() +
  theme(
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank())






