
# Check for empty geometries
esker_data_empty <- esker_data[st_is_empty(esker_data), ]
esker_data_non_empty <- esker_data[!st_is_empty(esker_data), ]

# Display number of empty geometries
nrow(esker_data_empty)
nrow(esker_data_non_empty)

# creating camera buffer bounding box 
camera_buffer_bb <- camera_buffer %>%
  st_buffer(100000) %>%
  st_bbox() %>%
  st_as_sfc()

plot(camera_buffer_bb)
# Then add TDN_boundary
plot(TDN_boundary, add = TRUE)
#then add buffers
plot(camera_buffer$geometry, add = TRUE)

# Remove empty geometries
esker_data <- esker_data[!st_is_empty(esker_data), ]

# Remove the Z dimension
esker_data <- st_zm(esker_data) %>%
  st_transform(32612) %>%
  st_intersection(camera_buffer_bb)

plot(esker_data)

# Rasterize
# Assuming 'esker_sp' is already a SpatialLinesDataFrame or a SpatVector
# Make sure it is a SpatVector
#esker_sp <- vect(esker_sp)


#getting distance between cameras and eskers

rast <- terra::rast(esker_data, resolution = 30)
esker_dist <- distance(rast, esker_data)

plot(rast)

plot(log(esker_dist))

#trying to overlay tdn boundary on esker_data map
# Check CRS of both objects
st_crs(esker_data)
st_crs(TDN_boundary)

# If they differ, transform one to the other
TDN_boundary_eskers <- st_transform(TDN_boundary, st_crs(esker_data))

# Plot esker_data first
plot(esker_data$geometry)

# Plot TDN_boundary next, ensuring it's in the same CRS
plot(TDN_boundary, add = TRUE)

#then add buffers
plot(camera_locations$geometry, add = TRUE)

# Compute distances from each camera trap to the nearest esker
camera_locations <- camera_locations %>%
  rowwise() %>%
  mutate(distance_to_esker = min(st_distance(geometry, esker_data$geometry))) %>%
  ungroup()

# View the results
print(camera_locations)

#plot(esker_data$geometry) %>%
#plot(TDN_boundary, add = TRUE)

#trying to create elevation map using ArcticDEM (code from erics github)

#Load tdn boundary data
# Assign a CRS (replace with the correct CRS)
# Assign a CRS using EPSG code
crs(TDN_DEM) <- CRS("+init=epsg:32612")  # Example: EPSG 32633 for UTM Zone 33N

TDN_dem <- st_transform(TDN_boundary, crs = st_crs(TDN_DEM))

dem_cropped<-crop(TDN_DEM,TDN_dem)
plot(dem_cropped, axes = FALSE)
plot(st_geometry(TDN_dem), add = TRUE)
plot(st_geometry(camera_locations),add = TRUE)

#trying to make table with elevation for each camera location 

# Assuming 'dem_cropped' is the cropped DEM and 'camera_locations' is an sf object of camera locations

# Extract elevation values for each camera location
elevations <- extract(dem_cropped, camera_locations)

# Combine the camera locations data with the extracted elevation values
# Convert camera_locations to a data frame if it's an sf object
camera_locations_df <- st_as_sf(camera_locations) %>%
  st_drop_geometry()  # Remove geometry to get a regular data frame

# Combine the extracted elevations with the camera location data
camera_locations_df$elevation <- elevation

# Print the resulting table
print(camera_locations_df)


#making inset nwt boundary 

# Download landmass data for the entire world from the naturalearth and naturalearthdata packages
world_landmass <- ne_countries(scale = "medium", returnclass = "sf") %>%
  st_transform(st_crs(Canada_Boundaries))

#Download Canada boundaries
Canada_Boundaries<-st_read("~/Desktop/Analysis/Learning/learning/spatial/shapefiles/gpr_000a11a_e.shp") %>%
  st_transform(Canada_Boundaries, crs = 3347)

#Transform Canadian Boundaries to NAD83
Canada_Boundaries_NWT <- Canada_Boundaries %>% st_transform(st_crs(TDN_boundary))

NWT_Boundary <- Canada_Boundaries_NWT %>%
  filter(PREABBR == "N.W.T.")

saveRDS(NWT_Boundary, "spatial/shapefiles/TDN_Boundary.rds")

world_landmass_NWT <- world_landmass %>% 
  st_transform(st_crs(TDN_boundary))

NWT_landmass <- st_intersection(world_landmass_NWT, NWT_Boundary)

saveRDS(NWT_landmass, "spatial/shapefiles/TDN_Boundary.rds")

# Save the plot as a PNG with a transparent background
png("nwt_boundary.png", bg = "transparent", width = 800, height = 600)

# Create the plot
ggplot() +
  geom_sf(data = NWT_Boundary, fill = NA, color = "black") +  # Boundary with no fill, black border
  geom_sf(data = NWT_landmass, fill = "thistle3") +  # Landmass with light grey fill
  geom_sf(data = TDN_boundary, fill = "royalblue2", color = "black") +  # Boundary with limegreen fill, black border
  theme_minimal() +
  theme(
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    plot.title = element_text(size = 16, face = "bold"),
    plot.subtitle = element_text(size = 12),
    panel.background = element_rect(fill = "transparent", color = NA),  # Transparent panel background
    plot.background = element_rect(fill = "transparent", color = NA))   # Transparent background for the plot area
    
    