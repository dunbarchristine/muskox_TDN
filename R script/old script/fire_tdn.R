
#trying to map a map showing different fires in/around TDN throughout the years

# Install the necessary packages if you haven't already
install.packages(c("sf", "ggplot2", "dplyr"))
library(sf)
library(ggplot2)
library(dplyr)


shapefile_path <- "~/Desktop/Analysis/Learning/learning/raw data/2023_Data/FireHistory.shp"

fire_data <- st_read("FireHistory.shp")

tdn_boundary <- st_read("TDN_Boundary.shp")


# Convert 'effectdate' to Date type if it's not already
fire_data$effectdate <- as.Date(fire_data$effectdate)

# Extract the year from the 'effectdate' column
fire_data$year <- format(fire_data$effectdate, "%Y")

# Optionally, you can filter to only keep specific years, if needed:
# fire_data <- fire_data %>% filter(year %in% c("1987", "1988", "1989"))

# Create the plot with boundary and fire data
ggplot() +
  # Plot the boundary
  geom_sf(data = tdn_boundary, fill = NA, color = "black", size = 1) +
  # Plot the fire data
  geom_sf(data = fire_data, aes(fill = year, size = area_ha), alpha = 0.6, color = "gray80") +
  scale_size_continuous(range = c(0.1, 10)) +  # Adjust size range
  scale_fill_viridis_d() +  # Color by year
  theme_minimal() +
  labs(
    title = "Fires Over the Years within TDN Boundary",
    subtitle = "Fire incidents and their areas, colored by year",
    fill = "Year",
    size = "Fire Size (ha)"
  ) +
  theme(legend.position = "bottom")

##################################################

ggplot(data=data_cameras_SF) +
  geom_sf(data=tdn_boundary) +
  #geom_sf(aes(colour = habitat), size = 6) +
  geom_sf(data = water_polygon, fill = 'light blue') +
  geom_sf(data = treeline_polygon, color = 'red') +
  geom_sf(data = fire_data, aes(fill = year, size = area_ha), alpha = 0.6, color = "gray80") +
  theme_minimal() +
  theme(
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid.minor = element_blank()) 

##################################################

ggplot() +
  geom_sf(data = tdn_boundary, fill=NA) +
  geom_sf(data = fire_data, aes(fill = year)) +
  theme_minimal() +
  theme(
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()) +
  labs(title = "TDN Sites by Fire", color = "Year of Fire") +
  coord_sf(xlim = st_bbox(tdn_boundary)[c("xmin", "xmax")],
           ylim = st_bbox(tdn_boundary)[c("ymin", "ymax")],
           expand = FALSE) # Focus plot on TDN_Boundary extent

#filter out certain years from fire map

data_cameras_SF_filtered <- data_cameras_SF %>%
  filter(!year %in% c(2020, 2021))








