library(raster)
library(tidyverse)
library(terra)


#cropping arcticdem to tdn boundary 
TDN_boundary_projected_ArcticDEM <- st_transform(TDN_boundary, crs = st_crs(ArcticDEM)) %>%
  st_buffer(1000)

#extract the elevation at camera site. we are using camera buffer because it has a geometry column.
arctic_DEM_500_mosaic_camera <- terra::extract(ArcticDEM, camera_buffer, fun=mean)

#add the value to the locations file if needed
camera_buffer$Arctic_DEM_500m_elevation_m<- arctic_DEM_500_mosaic_camera$arcticdem_mosaic_500m_v4.1_dem

#deleting old elevation column that gave NAs
model_variables <- model_variables %>%
  select(-Elevation)

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

#ArcticDEM data with data from Claudia (arctic_DEM_500_mosaic_camera). Not working, needs to be spatraster

# TRI_Results_ArcticDEM_mosaic <- spatialEco::tri(arctic_DEM_500_mosaic_camera, s = 3, exact = FALSE)
#  #ArcticDEM for TDI
# TDN_tri_ArcticDEM_mosaic <- st_transform(TDN_boundary, crs = st_crs(TRI_Results_ArcticDEM_mosaic))

#calculating TRI. grabbed this code from Claudias GitHub
TDN_tri_arctic<-  terra::terrain(ArcticDEM, v="TRI", unit = "degrees")

#saving model_variables as RDS
saveRDS(ArcticDEM_cropped,"~/Desktop/Analysis/Learning/learning/RDS files/ArcticDEM_cropped.rds")
ArcticDEM_cropped <- readRDS("~/Desktop/Analysis/Learning/learning/RDS files/ArcticDEM_cropped.rds")

#adding Arctic_DEM_500m_elevation_m to model_variables instead of having the previous column "Elevations". "Elevations" column was giving me NAs
model_variables <- model_variables %>%
  dplyr::left_join(
    camera_buffer %>%
      dplyr::select(location, Arctic_DEM_500m_elevation_m),
    by = "location"
  )

# Extract values for each camera location
elevations <- raster::extract(ArcticDEM_cropped, camera_locations)
esker_camera_distances <- terra::extract(esker_dist, camera_locations)
TRI_extracted <- raster::extract(TDN_tri_arctic, camera_locations)

#adding terrain ruggedness index (TRI) to all_variables
all_variables_with_tri <- all_variables %>%
  left_join(camera_locations_df %>% select(location, TRI_extracted), by = "location")


#made new dataset that included tri and grizz_per_day and gray_wolf_per_day
all_variables_with_tri <- all_variables %>%
  left_join(camera_locations_df %>% select(location, TRI_extracted), by = "location")

#adding new variables to "all_variables_with_tri_and_species". This dataset is used to make model_variables (in 7.model dataset script)
all_variables_with_tri_and_species <- all_variables_with_tri %>%
  mutate(grizzly_per_day = grizzly_bear/n_days_effort, 
         gray_wolf_per_day = gray_wolf/n_days_effort)



############### eskers ################
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


#making map for just tdn ecoregions
tdn_ecoregions_2 <- cropped_ecoregions_TDN_Boundary %>%
  group_by(ECO2_NAM_1) %>% 
  summarize(geometry = st_union(geometry)) 





library(sf)
# Set the correct CRS (replace with your known EPSG if different)
st_crs(tdn_ecoregions) <- 32612

# Ensure both datasets use the same CRS
tdn_ecoregions <- st_transform(tdn_ecoregions, crs = st_crs(model_variables))

# Perform spatial join
model_variables_joined <- st_join(model_variables, tdn_ecoregions, left = TRUE)



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


#Calculating the Ecoregion for each Camera location
locs_ecoregions <- cameras_sf %>%
  st_transform(crs = st_crs(ecoregions_cropped)) #change the projection to match the raster

# Find overlaps
ecoregions_overlap <- st_join(locs_ecoregions, ecoregions_cropped)
ecoregions_overlap <- as.data.frame(ecoregions_overlap)

# Make a new column in cameras (if needed)
cameras <- cameras %>% 
  left_join(ecoregions_overlap %>% select(location, ECO1_NAM_1, ECO2_NAM_1, ECO3_NAM_1, ECO4_NAM_1, ecoregion), by="location")



#comparing scanfi and lcc to determine which landcover dataset to use/justification for choosing lc/scanfi
lcc_scanfi_cor <- cor(lcc_cameras_prop_columns_variables_only, overlap_SCANFI_cameras_table_variables_only)

#looking at how correlated all the variables are 
model_subset <- all_variables_with_tri_and_species %>% ungroup() %>%
  select(-15:-24, -28, -29) %>%
  distinct() %>%
  select(-1)

correlation_matrix <- cor(model_subset, use = "complete.obs")  # Use complete.obs to handle NAs
corrplot(correlation_matrix, method="circle")







