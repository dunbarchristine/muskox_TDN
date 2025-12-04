library(raster)
library(tidyverse)
library(terra)
library(writexl)

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

camera_locations_df <- camera_locations_df %>%
  left_join(
    model_variables %>% dplyr::select(location, TRI_extracted),
    by = "location"
  )

all_variables <- all_variables %>%
  left_join(
    model_variables %>% dplyr::select(location, TRI_extracted),
    by = "location"
  )


camera_locations_unique <- camera_locations_df %>%
  dplyr::select(location, TRI_extracted) %>%
  distinct(location, .keep_all = TRUE)

all_variables_with_tri <- all_variables %>%
  left_join(camera_locations_unique, by = "location")

camera_locations_unique2 <- camera_locations_df %>%
  dplyr::select(location, TRI_extracted) %>%
  distinct(location, .keep_all = TRUE)

all_variables_with_tri <- all_variables %>%
  left_join(camera_locations_unique2, by = "location")


#adding terrain ruggedness index (TRI) to all_variables
all_variables_with_tri <- all_variables %>%
  left_join(camera_locations_df %>% dplyr::select(location, TRI_extracted), by = "location")


# #made new dataset that included tri and grizz_per_day and gray_wolf_per_day
# all_variables_with_tri <- all_variables %>%
#   left_join(camera_locations_df %>% dplyr::select(location, TRI_extracted), by = "location")

#adding new variables to "all_variables_with_tri_and_species". This dataset is used to make model_variables (in 7.model dataset script)
all_variables_with_tri_and_species <- all_variables_with_tri %>%
  mutate(grizzly_per_day = grizzly_bear/N_days_effort, 
         gray_wolf_per_day = gray_wolf/N_days_effort)



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

#making plots for weekly muskox detections in regard to landcover 
muskox_detections <- model_variables %>%
  st_drop_geometry() %>%
  #drop_na(N_days_effort) %>%
  group_by(location, cluster, season) %>%
  summarise(Muskox = sum(Muskox), N_days_effort = sum(N_days_effort),
            Shrub = mean(Shrub), 
            Trees = mean(Trees),
            Groundcover = mean(Groundcover),
            Water = mean(Water),
            ) %>%
  mutate(Muskox_per_week = Muskox/N_days_effort*7) %>%
  pivot_longer(cols = c("Trees", "Groundcover", "Shrub", "Water"), names_to = "landcover_class", values_to = "proportion") %>%
  group_by(location, cluster, season) %>%
  filter(proportion == max(proportion)) %>%
  group_by(landcover_class, season) %>%
  summarise(Muskox = sum(Muskox), 
            N_days_effort = sum(N_days_effort),
            Muskox_per_week_mean = mean(Muskox_per_week), 
            sd = sd(Muskox_per_week),
            num_cameras = n(),
            num_cluster = length(unique(cluster)),
            se = sd / sqrt(num_cameras)   # ✅ Standard Error
  )

#making it so no values show on the plot that go below zero
muskox_detections_plot <- muskox_detections %>%
  mutate(ymin = Muskox_per_week_mean - sd,
         ymax = Muskox_per_week_mean + sd) %>%
  filter(ymax > 0)  # only keep points where there's something above zero

#plotting
ggplot(muskox_detections_plot, aes(x = landcover_class, y = Muskox_per_week_mean)) +
  geom_point() +
  geom_pointrange(aes(ymin = pmax(ymin, 0), ymax = ymax)) +
  facet_wrap(~season) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
  labs(
    x = "Land Cover Class",
    y = "Average Muskox Detection Per Week"
  ) +
  scale_x_discrete(labels = c(
    "Shrub" = "Shrub",
    "Trees" = "Trees",
    "Groundcover" = "Groundcover",
    "Water" = "Water"
  ))

#making plots for weekly muskox detections in regard to fire
muskox_detections_fire <- model_variables %>%
  st_drop_geometry() %>%
  #drop_na(N_days_effort) %>%
  group_by(location, cluster, season) %>%
  summarise(Muskox = sum(Muskox), N_days_effort = sum(N_days_effort),
            fire_age0 = mean(fire_age0),
            fire_age1 = mean(fire_age1),
            fire_age2 = mean(fire_age2),
            fire_age3 = mean(fire_age3),
            fire_age4 = mean(fire_age4),
  ) %>%
  mutate(Muskox_per_week = Muskox/N_days_effort*7) %>%
  pivot_longer(cols = c("fire_age0", "fire_age1", "fire_age2","fire_age3","fire_age4"), names_to = "landcover_class", values_to = "proportion") %>%
  group_by(location, cluster, season) %>%
  filter(proportion == max(proportion)) %>%
  group_by(landcover_class, season) %>%
  summarise(Muskox = sum(Muskox), 
            N_days_effort = sum(N_days_effort),
            Muskox_per_week_mean = mean(Muskox_per_week), 
            sd = sd(Muskox_per_week),
            num_cameras = n(),
            num_cluster = length(unique(cluster)),
se = sd / sqrt(num_cameras)   # ✅ Standard Error
)


#making it so no values show on the plot that go below zero
muskox_detections_fire_plot <- muskox_detections_fire %>%
  mutate(ymin = Muskox_per_week_mean - sd,
         ymax = Muskox_per_week_mean + sd) %>%
  filter(ymax > 0)  # only keep points where there's something above zero

#plotting
ggplot(muskox_detections_fire_plot, aes(x = landcover_class, y = Muskox_per_week_mean)) +
  geom_point() +
  geom_pointrange(aes(
    ymin = pmax(ymin, 0),  # cap lower bound at zero
    ymax = ymax
  )) +
  facet_wrap(~season) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
  labs(
    x = "Years Post Fire",
    y = "Average Muskox Detection Per Week"
  ) +
  scale_x_discrete(labels = c(
    "fire_age0" = "0–10",
    "fire_age1" = "11–20",
    "fire_age2" = "21–30",
    "fire_age3" = "31–50",
    "fire_age4" = "Unburned"
  )) +
  coord_cartesian(ylim = c(0, NA))


write_xlsx(muskox_detections, "muskox_detections.xlsx")

#making simple bar grpahs showing average muskox detections
ggplot(muskox_detections, aes(x = landcover_class, y = Muskox_per_month_mean, fill = landcover_class)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  facet_wrap(~season) +
  labs(
    title = "Average Monthly Muskox Detections by Land Cover Class",
    x = "Dominant Land Cover Class",
    y = "Average Muskox Detections per Month",
    fill = "Land Cover"
  ) +
  scale_fill_manual(values = c(
    "Water" = "lightskyblue",
    "Trees" = "olivedrab",
    "Shrub" = "goldenrod4",          # Example brownish color
    "Groundcover" = "gray60"   # Example gray color
  )) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#looking at grizzly detections by landcover type
grizzly_detections <- model_variables %>%
  st_drop_geometry() %>%
  #drop_na(N_days_effort) %>%
  group_by(location, cluster, season) %>%
  summarise(grizzly_bear = sum(grizzly_bear), N_days_effort = sum(N_days_effort),
            Shrub = mean(Shrub), 
            Trees = mean(Trees),
            Groundcover = mean(Groundcover),
            Water = mean(Water),
  ) %>%
  mutate(grizz_per_week = grizzly_bear/N_days_effort*7) %>%
  pivot_longer(cols = c("Trees", "Groundcover", "Shrub", "Water"), names_to = "landcover_class", values_to = "proportion") %>%
  group_by(location, cluster, season) %>%
  filter(proportion == max(proportion)) %>%
  group_by(landcover_class, season) %>%
  summarise(grizzly_bear = sum(grizzly_bear), 
            N_days_effort = sum(N_days_effort),
            grizz_per_week_mean = mean(grizz_per_week), 
            sd = sd(grizz_per_week),
            num_cameras = n(),
            num_cluster = length(unique(cluster)),
se = sd / sqrt(num_cameras))   # ✅ Standard Error


#plotting
ggplot(grizzly_detections, aes(x = landcover_class, y = grizz_per_month_mean)) +
  geom_point() +
  geom_pointrange(aes(ymin = grizz_per_month_mean - sd, ymax = grizz_per_month_mean + sd)) +
  facet_wrap(~season) +
  labs(
    x = "Land Cover Class",
    y = "Average Grizzly Detection Per Month"
  )



#making simple bar grpahs showing average grizz detections
ggplot(grizzly_detections, aes(x = landcover_class, y = grizz_per_month_mean, fill = landcover_class)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  facet_wrap(~season) +
  labs(
    title = "Average Monthly Grizzly Bear Detections by Land Cover Class",
    x = "Dominant Land Cover Class",
    y = "Average Grizzly Bear Detections per Month",
    fill = "Land Cover"
  ) +
  scale_fill_manual(values = c(
    "Water" = "lightskyblue",
    "Trees" = "olivedrab",
    "Shrub" = "goldenrod4",          # Example brownish color
    "Groundcover" = "gray60"   # Example gray color
  )) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


write_xlsx(grizzly_detections, "grizzly_detections.xlsx")

#looking at wolf detections by landcover type
wolf_detections <- model_variables %>%
  st_drop_geometry() %>%
  #drop_na(N_days_effort) %>%
  group_by(location, cluster, season) %>%
  summarise(gray_wolf = sum(gray_wolf), N_days_effort = sum(N_days_effort),
            Shrub = mean(Shrub), 
            Trees = mean(Trees),
            Groundcover = mean(Groundcover),
            Water = mean(Water),
  ) %>%
  mutate(wolf_per_week = gray_wolf/N_days_effort*7) %>%
  pivot_longer(cols = c("Trees", "Groundcover", "Shrub", "Water"), names_to = "landcover_class", values_to = "proportion") %>%
  group_by(location, cluster, season) %>%
  filter(proportion == max(proportion)) %>%
  group_by(landcover_class, season) %>%
  summarise(gray_wolf = sum(gray_wolf), 
            N_days_effort = sum(N_days_effort),
            wolf_per_week_mean = mean(wolf_per_week), 
            sd = sd(wolf_per_week),
            num_cameras = n(),
            num_cluster = length(unique(cluster)),
            se = sd / sqrt(num_cameras)   # ✅ Standard Error
  )

#plotting
ggplot(wolf_detections, aes(x = landcover_class, y = wolf_per_week_mean)) +
  geom_point() +
  geom_pointrange(aes(ymin = wolf_per_week_mean - sd, ymax = wolf_per_week_mean + sd)) +
  facet_wrap(~season) +
  labs(
    x = "Land Cover Class",
    y = "Average Gray Wolf Detection Per Week"
  )


write_xlsx(wolf_detections, "wolf_detections.xlsx")

#making simple bar grpahs showing average wolf detections
ggplot(wolf_detections, aes(x = landcover_class, y = wolf_per_month_mean, fill = landcover_class)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  facet_wrap(~season) +
  labs(
    title = "Average Monthly Gray Wolf Detections by Land Cover Class",
    x = "Dominant Land Cover Class",
    y = "Average Gray Wolf Detections per Month",
    fill = "Land Cover"
  ) +
  scale_fill_manual(values = c(
    "Water" = "lightskyblue",
    "Trees" = "olivedrab",
    "Shrub" = "goldenrod4",          # Example brownish color
    "Groundcover" = "gray60"   # Example gray color
  )) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



muskox_avg_detections <- model_variables %>%
  st_drop_geometry() %>%
  #drop_na(N_days_effort) %>%
  group_by(location, cluster, season) %>%
  summarise(Muskox = sum(Muskox), N_days_effort = sum(N_days_effort)
            # Shrub = mean(Shrub), 
            # Trees = mean(Trees),
            # Groundcover = mean(Groundcover),
            # Water = mean(Water),
  ) %>%
  mutate(Muskox_per_month = Muskox/N_days_effort*30) %>%
  #pivot_longer(cols = c("Trees", "Groundcover", "Shrub", "Water"), names_to = "landcover_class", values_to = "proportion") %>%
  group_by(location, cluster, season) %>%
  filter(proportion == max(proportion)) %>%
  group_by(season) %>%
  summarise(Muskox = sum(Muskox), 
            N_days_effort = sum(N_days_effort),
            Muskox_per_month_mean = mean(Muskox_per_month), 
            sd = sd(Muskox_per_month),
            num_cameras = n(),
            num_cluster = length(unique(cluster)))

#predator detections by landcover type
predator_detections <- model_variables %>%
  st_drop_geometry() %>%
  #drop_na(N_days_effort) %>%
  group_by(location, cluster, season) %>%
  summarise(Predators = sum(Predators), N_days_effort = sum(N_days_effort),
            Shrub = mean(Shrub), 
            Trees = mean(Trees),
            Groundcover = mean(Groundcover),
            Water = mean(Water),
  ) %>%
  mutate(predator_per_week = Predators/N_days_effort*7) %>%
  pivot_longer(cols = c("Trees", "Groundcover", "Shrub", "Water"), names_to = "landcover_class", values_to = "proportion") %>%
  group_by(location, cluster, season) %>%
  filter(proportion == max(proportion)) %>%
  group_by(landcover_class, season) %>%
  summarise(Predators = sum(Predators), 
            N_days_effort = sum(N_days_effort),
            predator_per_week_mean = mean(predator_per_week), 
            sd = sd(predator_per_week),
            num_cameras = n(),
            num_cluster = length(unique(cluster)),
            se = sd / sqrt(num_cameras)   # ✅ Standard Error
  )




#predicting muskox detections with predators 
# Step 1: Convert cluster column in dataset to factor
predator_data <- model_variables %>%
  filter(season == "Summer") %>%
  ungroup() %>%
  modelr::data_grid(
    TRI_extracted = 0,
    Shrub = 0,
    Predators_new = seq(0, 5, by = 1),
    N_days_effort = 7,
    cluster = "new",
  ) %>%
  mutate(
    Predators= MuMIn::stdize(Predators_new, source = Predator)
  )

# Step 2: Predict
predators_prediction <- predator_data %>%
  mutate(predicted_muskox_week =
           predict(mod_pred_sum_1, newdata = ., allow.new.levels = TRUE, type = "response"))

ggplot(predators_prediction, aes(x = Predators_new,
                                      y = predicted_muskox_week)) +
  geom_line(size=1) + # scatter plot
  geom_rug(
    data = model_variables,
    aes(x = Predators, y = 0),
    inherit.aes = FALSE,
    sides = "b",
    alpha = 0.3,
    position = position_jitter(height = 0.02)
  ) +
  labs(
    x = "Number of Predators",
    y = "Predicted Muskox Detections per Week",
  ) +
  theme_minimal()

#predicting muskox detections for TRI
# Step 1: Convert cluster column in dataset to factor
TRI_data <- model_variables %>%
  filter(season == "Summer") %>%
  ungroup() %>%
  modelr::data_grid(
    TRI_extracted_new = seq(2, 260, by = 1),
    Shrub = 0,
    Predators = 0,
    N_days_effort = 7,
    cluster = "new",
  ) %>%
  mutate(
    TRI_extracted = MuMIn::stdize(TRI_extracted_new, source = TRI_extracted)
  )

# Step 2: Predict
TRI_prediction <- TRI_data %>%
  mutate(predicted_muskox_week =
           predict(mod_pred_sum_1, newdata = ., allow.new.levels = TRUE, type = "response"))

ggplot(TRI_prediction, aes(x = TRI_extracted_new,
                                 y = predicted_muskox_week)) +
  geom_line(size=1) + # scatter plot
  geom_rug(
    data = model_variables,
    aes(x = TRI_extracted, y = 0),
    inherit.aes = FALSE,
    sides = "b",
    alpha = 0.3,
    position = position_jitter(height = 0.02)
  ) +
  labs(
    x = "Terrain Ruggedness Index",
    y = "Predicted Muskox Detections per Week",
  ) +
  theme_minimal()

#predicting muskox detections for shrub 
# Step 1: Convert cluster column in dataset to factor
Shrub_data <- model_variables %>%
  filter(season == "Summer") %>%
  ungroup() %>%
  modelr::data_grid(
    TRI_extracted = 0,
    Shrub_new = seq(0, 1, by = 1),
    Predators = 0,
    N_days_effort = 7,
    cluster = "new",
  ) %>%
  mutate(
    Shrub = MuMIn::stdize(Shrub_new, source = Shrub)
  )

# Step 2: Predict
Shrub_prediction <- Shrub_data %>%
  mutate(predicted_muskox_week =
           predict(mod_pred_sum_1, newdata = ., allow.new.levels = TRUE, type = "response"))

ggplot(Shrub_prediction, aes(x = Shrub_new,
                           y = predicted_muskox_week)) +
  geom_line(size=1) + # scatter plot
  geom_rug(
    data = model_variables,
    aes(x = Shrub, y = 0),
    inherit.aes = FALSE,
    sides = "b",
    alpha = 0.3,
    position = position_jitter(height = 0.02)
  ) +
  labs(
    x = "Shrub Land Cover",
    y = "Predicted Muskox Detections per Week",
  ) +
  theme_minimal()


SCANFI_landcover_cropped <- rast("spatial/SCANFI_landcover_cropped.tif")

tdn_boundary_rast <- rasterize(vect(TDN_boundary), SCANFI_landcover_cropped, field = "Name") 

landcover_tdn <- crosstab(c(tdn_boundary_rast, SCANFI_landcover_cropped), long = TRUE) 

landcover_tdn_table <- landcover_tdn %>%
  as_tibble() %>%
  group_by(Name) %>%
  mutate(landcover_prop = n/sum(n))

SCANFI_landcover_cameras <- camera_locations %>%
  mutate(landcover = terra::extract(SCANFI_landcover_cropped, camera_locations))


#map of tdn boundary, scanfi landcover and camera locations
ggplot() +
  tidyterra::geom_spatraster(data = cropped_SCANFI_TDN_Boundary) + 
  geom_sf(data = camera_locations, color = "limegreen", size = 3) +  
  #geom_sf(data = comb_overlap_SCANFI_and_selected_mammals_week_df$Muskox)
  theme_minimal() +
  theme(
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  ) +
  ggtitle("Plot of cropped_SCANFI_TDN_boundary and camera_locations")


#dominate land cover classes in TDN 
cropped_SCANFI_TDN_Boundary <- crop(SCANFI_landcover_cropped, TDN_boundary, mask = TRUE)

Camera_buffer_zones <- rasterize(vect(camera_buffer), SCANFI_landcover_cropped, field = "location") 

overlap_SCANFI_cameras <- crosstab(c(Camera_buffer_zones, SCANFI_landcover_cropped), long = TRUE) 

overlap_SCANFI_cameras_table <- overlap_SCANFI_cameras %>%
  as_tibble() %>%
  group_by(location) %>%
  mutate(landcover_prop = n/sum(n)) #n is the number of pixels for each habitat type divided by all habitat types 





















