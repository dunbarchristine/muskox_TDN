
###########
#reading and mapping shapefiles and locations in TDN 
library(sp)
library(readr)
library(ggplot2)
library(sf)

#reading in the detections file
loc<- read.csv('TDN_muskox_detections_by_location.csv')

#converting to a dataframe
df <- data.frame(loc)

# Convert df to an sf object
df_sf <- st_as_sf(df, coords = c("longitude", "latitude"), crs = 4326)
#reading in the TDN boundary 
shapefile_path <- "~/Desktop/Analysis/Learning/learning/TDN_Boundary.shp"
spatial_polygon <- st_read(shapefile_path)
#reading in the TDN treeline 
shapefile_path2 <- "~/Desktop/Analysis/Learning/learning/Treeline_Approx.shp"
spatial_polygon2 <- st_read(shapefile_path2)

# Plot the detections, TDN boundary and the treeline
ggplot() +
  # Add the spatial polygons
  geom_sf(data = spatial_polygon, fill = 'lightgrey', color = 'black') + 
  geom_sf(data = spatial_polygon2, color = 'black') +
  geom_sf(data = df_sf, color = "red", size = 3) +
  labs(title = "Thaidene Nëné Muskox Detections",
       x = "Longitude",
       y = "Latitude") +
  theme_minimal()


###subsetting months
# Load the packages
library(dplyr)
library(magrittr)  # Optional, dplyr includes magrittr


# Read in your data
map<- read.csv('NWTBM_muskox_month_data.csv')
tdn_boundary <- st_read("TDN_Boundary.shp")

#told it the format for the date: year, month, day
map$date <- as.Date(map$date, format = "%Y-%m-%d")
data <- map %>% mutate(year_month = floor_date(date, "month")) %>% 
  filter(species_common_name == "Muskox") %>%
  mutate(week = week(date),
         month = month(date),
         year = year(date),
         individual_count = as.numeric(individual_count))

#create column for date, month, location + show average muskox detections per week
month_data <- data %>% 
  group_by(date, month, location) %>% 
  summarise(muskox_count = max(individual_count)) %>%
  group_by(month, location) %>% 
  summarise(month_count = mean(muskox_count) * 7)

#plotting lat and long
locations <- data %>% 
  distinct(location, latitude, longitude) %>%
  drop_na(latitude) %>%
  st_as_sf(coords=c("longitude", "latitude"), crs=4326)
  
month_col <- month_data %>%
  left_join(locations) %>%
  st_as_sf(month_col, wkt = "geometry", crs = 4326)
  
st_geometry_type(month_col)
ggplot() +
  geom_sf(data=spatial_polygon) +
  geom_sf(data=month_col, aes(geometry = geometry, size=month_count)) +
  coord_sf(datum=st_crs(3857))
  facet_wrap(~month, nrow = 4) +
  theme_minimal() +
  theme(
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank())

#ggplot(aes(size = month_count)) +
#geom_sf(aes(geometry = geometry)) +
#st_transform(st_crs(tdn_boundary)) +
#facet_wrap(~month, nrow = 4)

#######################################################
#code from claudia

map<- read.csv('NWTBM_muskox_month_data.csv')
#separating out the data by week and month
map$date <- as.Date(map$date, format = "%Y-%m-%d")
data <- map %>% mutate(year_month = floor_date(date, "month")) %>% 
  filter(species_common_name == "Muskox") %>%
  mutate(week = week(date),
         month = month(date),
         year = year(date),
         individual_count = as.numeric(individual_count))

month_data <- data %>%
  group_by(date, month, location) %>%
  summarise(date_count = max(individual_count)) %>%
  group_by(month, location) %>%
  summarise(month_count = mean(date_count) * 7)

month_data_cameras <- merge(month_data, cameras, by="location") #cameras = location report file from wildtrax

ggplot() +
  geom_sf(data= spatial_polygon) +
  geom_sf(data=month_data_cameras, aes(geometry = geometry, size=month_count)) +
  facet_wrap(~month, nrow = 4) +
  theme_minimal() +
  theme(
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank())


ggplot() +
  geom_sf(data=month_col) +
  geom_sf(data=month_col, aes(geometry = geometry, size=month_count)) +
  facet_wrap(~month, nrow = 2) +
  theme_minimal() +
  theme(
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank())


# Plot the detections, TDN boundary and the treeline
ggplot() +
  # Add the spatial polygons
  geom_sf(data = spatial_polygon, fill = 'lightgrey', color = 'black') + 
  geom_sf(data = spatial_polygon2, color = 'black') +
  geom_sf(data = df_sf, color = "red", size = 3) +
  labs(title = "Thaidene Nëné Muskox Detections",
       x = "Longitude",
       y = "Latitude") +
  theme_minimal()



  
  
  
drop_na(latitude)%>%
  st_as_sf(coords=c("longitude", "latitude"), crs=4326) %>% 
  st_transform(st_crs(TDN_Boundary.shp))


#######working
######################################################
#taking nwtbm report and isolating only muskox data 

library(sp)
library(readr)
library(ggplot2)
library(sf)

data1 <- read.csv('NWTBM_muskox_month_data.csv', header = TRUE)

df <- read.table("NWTBM_muskox_month_data.csv.c.txt", sep = "\t", header = TRUE)
head(data1)
summary(data1)

######################################################
#making maps that corresponds to data from each month


install.packages("tidyverse")
library(tidyverse)
library(lubridate)


map<- read.csv('NWTBM_muskox_month_data.csv')
head(map)

df <- data.frame(map)

df$date <- as.Date(df$date)

# Convert date column to Date type if not already
data$date <- as.Date(data$date)

# Check the structure of the data
str(data)

# Check column names
names(data)

# Clear workspace to avoid conflicts
rm(list = ls())

# Load required packages
library(sf)
library(ggplot2)
library(lubridate)
library(dplyr)# Install dplyr and magrittr if not already installed
install.packages("dplyr")
install.packages("magrittr")



# Generate maps for each month
months <- unique(data_sf$year_month)

for (month in months) {
  monthly_data <- data_sf %>% filter(year_month == month)
  
  p <- ggplot() +
    geom_sf(data = monthly_data, aes(color = your_metric), size = 2) +
    theme_minimal() +
    labs(title = paste("Map for", format(month, "%B %Y")),
         color = "Metric") +
    coord_sf()
  
  ggsave(filename = paste0("map_", format(month, "%Y_%m"), ".png"), plot = p, width = 10, height = 8)


# Extract the `date` column from the data frame
date_only <- map_sf[ , "month"]
print(date_only)

# Check the column names of the data frame
names(map_sf)

muskox_only <- map_sf[ , "species_common_name"]
print(muskox_only)
























