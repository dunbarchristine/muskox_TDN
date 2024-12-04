# Explain where I got the data from
# Study region

library(tidyverse)
library(sf)
library(terra)

#downloads a shapefile and converts into R object
TDN_Boundary<-st_read("Data/Raw/Boundary_TDN/TDN_Boundary.shp")

#make sure it looks right
plot(TDN_Boundary)

#load camera sites, drop the NA sites, convert to spatial file
TDN_Cameras<-read_csv("Data/Raw/SpeciesRawDownload/NWTBM_Thaidene_Nëné_Biodiversity_Project_2021_location_report.csv")%>% 
  drop_na(latitude)%>%
  st_as_sf(coords=c("longitude", "latitude"), crs=4326, remove=FALSE) %>% 
  st_transform(st_crs(TDN_Boundary))

#load species data
species_all <- read.csv("Data/Raw/SpeciesRawDownload/NWTBM_Thaidene_Nëné_Biodiversity_Project_2021_tag_report.csv")

#QA/QC on data
#how many sites are there?
paste(length(unique(TDN_Cameras$location)))
#how does it look?
plot(TDN_Cameras)

#plot boundary (geometry column from sf) and cameras
plot(TDN_Boundary$geometry)
plot(TDN_Cameras$geometry, add=TRUE)

#create column of sites, as well as location (station) so that it can be merged with the habitat file
TDN_Cameras$site <- substr(TDN_Cameras$location, 1, 11)

#QA/QC on date
#how many sites are there?
paste(length(unique(TDN_Cameras$site)))

#loading station habitat csv file
TDN_Habitat<-read_csv("Data/Raw/TDN_habitat_site.csv")

#merge habitat file into camera file
TDN_Cameras_Habitat <- merge (TDN_Cameras, TDN_Habitat, by="site")

#plot boundary with cameras coloured by habitat
ggplot() +
  geom_sf(data = TDN_Boundary$geometry) +
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

month_data <- data %>% 
  group_by(date, month, location) %>%
  summarise(date_count = max(individual_count)) %>%
  group_by(month, location) %>%
  summarise(month_count = mean(date_count) * 7)

data_cameras <- merge(data, TDN_Cameras, by="location")
month_data_cameras <- merge(month_data, TDN_Cameras, by="location")

#Muskox count by month
ggplot() +
  geom_sf(data=TDN_Boundary) +
  geom_sf(data=month_data_cameras, aes(geometry = geometry, size=month_count)) +
  facet_wrap(~month, nrow = 4) +
  theme_minimal() +
  theme(
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()) 

#Muskox count all year 
ggplot() +
  geom_sf(data=TDN_Boundary) +
  geom_sf(data=data_cameras, aes(geometry = geometry, size=individual_count)) +
  theme_minimal() +
  theme(
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()) 

#Muskox detections all year
ggplot() +
  geom_sf(data=TDN_Boundary) +
  geom_sf(data=data_cameras, aes(geometry = geometry, size=individual_count)) +
  theme_minimal() +
  theme(
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()) 