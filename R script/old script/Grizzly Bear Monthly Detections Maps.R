#making a map showing wolf detections each month to compare to muskox detections
library(dplyr)
library(ggplot2)
library(lubridate)

#upload TDN shapefile 
tdn_boundary <- st_read("TDN_Boundary.shp")

#reading in water shapefile
water_shapefile <- "~/Desktop/Analysis/Learning/learning/TDN_water_features.shp"
water_polygon <- st_read(water_shapefile)

#load camera sites, drop the NA sites, convert to spatial file
TDN_Cameras<-read_csv("NWTBM_Thaidene_Nëné_Biodiversity_Project_2021_location_report.csv")
TDN2 <-TDN_Cameras %>% 
  filter(!is.na(latitude)) %>% 
  st_as_sf(coords=c("longitude", "latitude"), crs=4326, remove=FALSE) %>% 
  st_transform(st_crs(tdn_boundary))

#load species data
species_all <- read.csv("NWTBM_Thaidene_Nëné_Biodiversity_Project_2021_tag_report.csv")

#create column of sites, as well as location (station) so that it can be merged with the habitat file
TDN2$site <- substr(TDN2$location, 1, 11)

#loading station habitat csv file
TDN_Habitat<-read_csv("TDN_habitat_site.csv")

#merge habitat file into camera file
TDN_Cameras_Habitat <- merge(TDN2, TDN_Habitat, by="site")

#adding month column to species_all
species_all <- species_all %>%
  mutate(month = month(ymd_hms(image_date_time)))

#convert TDN habitat to an SF object so we can plot it 
TDN_Habitat_Locations <- merge(TDN2, TDN_Habitat, by="site") #merging habitat and lat and long datasets together
TDN_Habitat_locations_SF <- st_as_sf(TDN_Habitat_Locations, coords=c("longitude", "latitude"), crs=4326, remove=FALSE) #now that its merged, turning it into an SF object
class(TDN_Habitat_locations_SF) #checking if its actually an SF object
#properly formatting date-time column

#separating out the data by week and month
species_all$date <- as.Date(species_all$image_date_time, format = "%Y-%m-%d")


species_all<- species_all %>%
  dplyr::mutate(species = fct_recode(species_common_name, Gray_Wolf="Gray Wolf"))
unique_categories2 <- unique(species_all$species_common_name)
print(unique_categories2)

#filtering out just gray wolf sightings
grizzly_data <- species_all %>% mutate(year_month = floor_date(date, "month"))  %>%
  filter(species_common_name == "Grizzly Bear") %>% #if I want to filter by a species
  mutate(week = week(date), #if I want to create a column with week number
         month = month(date),
         year = year(date),
         individual_count = as.numeric(individual_count))

#renaming column from 'habitat.x' to 'habitat'

grizzly_data_cameras_SF <- grizzly_data_cameras_SF %>%
  rename(habitat = habitat.x)

#averaging wolf detections by week
# wolf_month_data <- wolf_data %>% 
#   group_by(date, month, location) %>%
#   summarise(date_count = max(individual_count)) %>%
#   group_by(month, location) %>%
#   summarise(month_count = mean(date_count) * 7)

#creating data frames to plot from  
grizzly_data_cameras <- merge(grizzly_data, TDN_Cameras_Habitat, by="location")
# wolf_month_data_cameras <- merge(month_data, TDN_Cameras_Habitat, by="location")

#converting to sf objects
grizzly_data_cameras_SF <- st_as_sf(grizzly_data_cameras, coords=c("longitude", "latitude"), crs=4326, remove=FALSE)
# wolf_month_data_cameras_SF <-st_as_sf(wolf_month_data_cameras, coords=c("longitude", "latitude"), crs=4326, remove=FALSE)

#making maps with wolf count and habitat 
grizzly_data_cameras_SF = grizzly_data_cameras_SF %>% mutate(month = lubridate::month(image_date_time))
grizzly_data_cameras_SF = left_join(x = grizzly_data_cameras_SF, y = TDN_Habitat_locations_SF %>% st_drop_geometry() %>% dplyr::select(location, habitat), by = "location")

#plot
ggplot(data=grizzly_data_cameras_SF)  +
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
