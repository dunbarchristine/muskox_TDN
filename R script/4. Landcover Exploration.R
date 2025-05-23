#cropping arcticdem to tdn boundary 
TDN_boundary_projected_ArcticDEM <- st_transform(TDN_boundary, crs = st_crs(ArcticDEM)) %>%
  st_buffer(1000)

ArcticDEM_cropped<-crop(ArcticDEM,TDN_boundary_projected_ArcticDEM)
plot(ArcticDEM_cropped, axes = FALSE)
plot(st_geometry(ArcticDEM_cropped), add = TRUE)
plot(st_geometry(camera_locations),add = TRUE)

plot(ArcticDEM_cropped)  # First plot the raster
plot(TDN_boundary_projected_ArcticDEM, add = TRUE, border = "red", lwd = 2)  # Overlay boundary

#Terrain Ruggedness Index (TRI)
#ArcticDEM data
TRI_Results_ArcticDEM <- spatialEco::tri(ArcticDEM_cropped, s = 3, exact = FALSE)
#ArcticDEM for TDI
TDN_tri_ArcticDEM <- st_transform(TDN_boundary, crs = st_crs(TRI_Results_ArcticDEM))

#Load tdn boundary data

#using arcticDEM data and cropping it to TDN boundary, similar to above
# TDN_boundary_projected <- st_transform(TDN_boundary, crs = st_crs(TDN_DEM)) %>%
#   st_buffer(1000)

TDN_boundary_projected_ArcticDEM <- st_transform(TDN_boundary, crs = st_crs(ArcticDEM)) %>%
  st_buffer(1000)

ArcticDEM_cropped<-crop(ArcticDEM,TDN_boundary_projected_ArcticDEM)
#saving model_variables as RDS
saveRDS(ArcticDEM_cropped,"~/Desktop/Analysis/Learning/learning/RDS files/ArcticDEM_cropped.rds")
ArcticDEM_cropped <- readRDS("~/Desktop/Analysis/Learning/learning/RDS files/ArcticDEM_cropped.rds")

#trying to make table with elevation for each camera location (COPERNICUS TDN_DEM)
# Extract values for each camera location
elevations <- raster::extract(ArcticDEM_cropped, camera_locations)
esker_camera_distances <- terra::extract(esker_dist, camera_locations)
TRI_extracted <- raster::extract(TRI_Results, camera_locations)


crs(ArcticDEM_cropped) <- CRS("+init=epsg:32612")  # Example: EPSG 32633 for UTM Zone 33N
camera_locations_projected <- vect(camera_locations_projected)  # converts sf to SpatVector
# Extract TRI values
TRI_extracted_arcticDEM <- terra::extract(TRI_Results_ArcticDEM, camera_locations_projected)

# TRI_extracted_arcticDEM <- raster::extract(TRI_Results_ArcticDEM, camera_locations)


# Combine the camera locations data with the extracted elevation values
# Convert camera_locations to a data frame if it's an sf object
camera_locations_df <- st_as_sf(camera_locations) %>%
  st_drop_geometry()  # Remove geometry to get a regular data frame

# Combine the extracted elevations with the camera location data
camera_locations_df$elevations <- elevations$ArcticDEM_cropped
camera_locations_df$esker_camera_distances <- esker_camera_distances$layer
camera_locations_df$TRI_extracted <- TRI_extracted$lyr.1

# Merge the datasets based on 'camera_id'
adding_variables_elevations_eskers <- merge(comb_overlap_SCANFI_and_selected_mammals_week, 
                                            camera_locations_df[, c("location", "elevations", "esker_camera_distances")], 
                                            by = "location", 
                                            all.x = TRUE)  # Keeps all rows from comb_overlap_SCANFI_and_selected_mammals_week

#adding terrain ruggedness index (TRI) to all_variables
all_variables_with_tri <- all_variables %>%
  left_join(camera_locations_df %>% select(location, TRI_extracted), by = "location")

# all_variables <- all_variables %>%
#   mutate(grizzly_per_day = grizzly_bear/n_days_effort, 
#          gray_wolf_per_day = gray_wolf/n_days_effort)

#copernicus
#made new dataset that included tri and grizz_per_day and gray_wolf_per_day
all_variables_with_tri <- all_variables %>%
  left_join(camera_locations_df %>% select(location, TRI_extracted), by = "location")

all_variables_with_tri_and_species_copernicus <- all_variables_with_tri %>%
  mutate(grizzly_per_day = grizzly_bear/n_days_effort, 
         gray_wolf_per_day = gray_wolf/n_days_effort)

all_variables <- all_variables %>%
  rename_with(~ gsub(" ", "_", .))

#arcticdem
# # Remove specific columns and rename 'elevations.y' to 'elevation'
# adding_variables_elevations_eskers <- adding_variables_elevations_eskers %>%
#   dplyr::select(-c(elevation.x, elevation.y, elevations.x)) %>%
#   rename(elevation = elevations.y)



#eskers
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

# Remove empty geometries
esker_data <- esker_data[!st_is_empty(esker_data), ]

# Remove the Z dimension
esker_data_cropped <- st_zm(esker_data) %>%
  st_transform(32612) %>%
  st_intersection(camera_buffer_bb)


#getting distance between cameras and eskers

rast <- terra::rast(esker_data_cropped, resolution = 30)
esker_rast <- rasterize(esker_data_cropped, rast)
esker_dist <- distance(esker_rast)


st_write(camera_locations, "SpeciesRawData (Oct 31)/NWTBM_Thaidene_Nëné_Biodiversity_Project_2021_location_repo.shp")

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


#trying to plot basic SCANFI landcover map for TDN 
ggplot() +
  tidyterra::geom_spatraster(data = cropped_SCANFI_TDN_Boundary) +
  theme_minimal() +
  theme(
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  ) +
  labs(title = "TDN Landcover Types", color = "Habitat")


#making map of all nwt ecoregions
nwt_ecoregions_2 <- nwt_ecoregions %>%
  group_by(ECO2_NAM_1) %>% 
  summarize(geometry = st_union(geometry)) 


#making mao for just tdn ecoregions
tdn_ecoregions_2 <- cropped_ecoregions_TDN_Boundary %>%
  group_by(ECO2_NAM_1) %>% 
  summarize(geometry = st_union(geometry)) 


#plotting map of tdn ecoregions
ggplot(data = tdn_ecoregions_2) +
  geom_sf(aes(fill = ECO2_NAM_1), color = "black", size = 0.2) +
  scale_fill_manual(
    name = "TDN Ecoregions",
    values = c("darkseagreen", "burlywood")
  ) +
  theme_minimal() +
  theme(
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank())
wd