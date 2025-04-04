







#getting independent detections for muskox, grizzlies, and gray wolves PER season

# Summarize the independent detections for each species by season
independent_detections_per_season <- selected_mammals_week %>%
  # Filter rows where at least one species was detected
  filter(Muskox == 1 | `Gray Wolf` == 1 | `Grizzly Bear` == 1) %>%
  # Group by season and location (and year_week if necessary for more precision)
  group_by(season, location) %>%
  # Keep only the first detection per season-location combination for each species
  summarize(
    muskox_detection = max(Muskox),           # Only keep the first detection of muskox
    wolves_detection = max(`Gray Wolf`),      # Only keep the first detection of wolves
    grizzly_detection = max(`Grizzly Bear`),  # Only keep the first detection of grizzly bears
    caribou_detection = max(`Barren-ground Caribou`)
  ) %>%
  # Ungroup so we can summarize by season
  ungroup() %>%
  # Now, summarize by season to get the total independent detections for each species
  group_by(season) %>%
  summarize(
    independent_muskox = sum(muskox_detection),
    independent_wolves = sum(wolves_detection),
    independent_grizzly = sum(grizzly_detection),
    independent_caribou = sum(caribou_detection)
  )

# View the result
print(independent_detections_per_season)


# Create the tibble with the necessary data
wolf_grizz_musk <- tibble(
  x = rep(1:5, each = 5),
  y = rep(1:5, times = 5),
  value = c(1, 2, 3, 4, 5, 2, 3, 4, 5, 6, 3, 4, 5, 6, 7, 
            4, 5, 6, 7, 8, 5, 6, 7, 8, 9)
)

#trying to see what habitats grizzlies and gray wolves are spending the most time in

# Step 1: Reshape the data to long format
most_detected_habitat <- comb_overlap_SCANFI_and_selected_mammals_week %>%
  dplyr::select(location, Shrub, Herbs, Water, Bryoid, Rock, `Treed mixed`, `Treed broadleaf`, `Treed conifer`, `Grizzly Bear`, `Gray Wolf`, Muskox, `Barren-ground Caribou`, n_days_effort) %>%
  pivot_longer(cols = c("Shrub", "Herbs", "Water", "Treed mixed", "Treed broadleaf", "Treed conifer", "Bryoid", "Rock"), 
               names_to = "habitat_type", 
               values_to = "proportion") %>%
  group_by(location) %>%
  filter(proportion == max(proportion))

most_detected_habitat %>%
  dplyr::select(location, habitat_type) %>%
  distinct() %>%
  pull(habitat_type) %>%
  as.factor() %>%
  summary()

most_detected_habitat <- as.data.frame(most_detected_habitat)

# Step 2: Summarize the detections by species and habitat type
most_detected_habitat_summary <- most_detected_habitat %>%
  dplyr::select(`Grizzly Bear`,`Gray Wolf`, `Barren-ground Caribou`, Muskox, habitat_type, n_days_effort, location) %>%  # Explicitly use dplyr::select()
  group_by(habitat_type) %>%
  summarise(grizzly_abund = sum(`Grizzly Bear`)/sum(n_days_effort)*1000,
            wolf_abund = sum(`Gray Wolf`)/sum(n_days_effort)*1000,
            muskox_abund = sum(Muskox)/sum(n_days_effort)*1000,
            caribou_abund = sum(`Barren-ground Caribou`)/sum(n_days_effort)*1000,
            num_cameras = length(unique(location)))

write_xlsx(most_detected_habitat_summary, "most_detected_habitat_summary.xlsx")

#filtering out just muskox sightings
data <- species_all %>% mutate(image_date = as_date(image_date_time), year_month = floor_date(image_date, "month"))  %>%
  filter(species_common_name %in% c("Grizzly Bear", "Gray Wolf", "Barren-ground Caribou")) %>% #if I want to filter by a species
  mutate(week = week(image_date), #if I want to create a column with week number
         month = month(image_date),
         year = year(image_date),
         individual_count = as.numeric(individual_count))

#averaging muskox detections by week
total_data <- data %>% 
  group_by(image_date, month, location, species_common_name) %>%
  summarise(date_count = max(individual_count)) %>%
  group_by(location, species_common_name) %>%
  summarise(total_count = mean(date_count) * 7)

#creating data frames to plot from  
total_data_cameras <- merge(total_data, TDN_Cameras, by="location")
#month_data_cameras <- merge(month_data, TDN_Cameras, by="location")

#converting to sf objects
total_data_cameras_SF <-st_as_sf(total_data_cameras, coords=c("longitude", "latitude"), crs=4326, remove=FALSE)
#data_cameras_SF <- st_as_sf(data_cameras, coords=c("longitude", "latitude"), crs=4326, remove=FALSE)

#plotting species abundance by SCANFI landcover type 
ggplot() +
  geom_sf(data=TDN_boundary %>% st_transform(32612), aes(geometry = geometry), fill = NA, color = "black", size = 20) +
  tidyterra::geom_spatraster(data = cropped_SCANFI_TDN_Boundary) +
  geom_sf(data=total_data_cameras_SF %>% st_transform(32612), aes(geometry = geometry, size=total_count)) +
  facet_wrap(~species_common_name, nro = 1) +
  theme_minimal() +
  theme(
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()) +
  ggtitle("Species Frequency of Detection by Landcover Type")  # Add the title

#trying to plot gray wolf detections by month with scanfi landcover

ggplot() +
  geom_sf(data=TDN_boundary %>% st_transform(32612), aes(geometry = geometry), fill = NA, color = "black", size = 20) +
  geom_sf(data=month_data_cameras_SF, aes(geometry = geometry, size=month_count)) +
  tidyterra::geom_spatraster(data = cropped_SCANFI_TDN_Boundary) +
  geom_sf(data=total_data_cameras_SF %>% st_transform(32612), aes(geometry = geometry, size=total_count)) +
  facet_wrap(~month, nrow = 4) +
  #facet_wrap(~species_common_name, nro = 1) +
  theme_minimal() +
  theme(
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()) +
  ggtitle("Species Frequency of Detection by Landcover Type") 


ggplot() +
  geom_sf(data=TDN_boundary) +
  geom_sf(data=month_data_cameras_SF, aes(geometry = geometry, size=month_count)) +
  facet_wrap(~month, nrow = 4) +
  theme_minimal() +
  theme(
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank())













#trying to plot muskox frequency of detections by landcover type and season
ggplot() +
  geom_sf(data=TDN_boundary) +
  geom_sf(data=month_data_cameras_SF, aes(geometry = geometry, size=month_count)) +
  geom_sf(data = TDN_Cameras_Habitat$geometry, color = TDN_Cameras_Habitat$habitat, size = 3) +
  facet_wrap(~month, nrow = 4) +
  theme_minimal() +
  theme(
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank())

#trying to plot cropped_SCANFI_TDN_Boundary and camera_locations together 
# Example of filtering for each season
# Filter for each season and select the relevant columns

#creating dataset with season, location and geomtry points
muskox_season_data<- comb_overlap_SCANFI_and_selected_mammals_week %>%
  group_by(season, location) %>%
  summarize(muskox_counts = sum(Muskox), 
            n_days_effort = sum(n_days_effort),
            muskox_per_week = muskox_counts/n_days_effort*7) %>%
  left_join(camera_locations) %>%
  mutate(season = factor(season, levels = c("Spring", "Summer", "Fall", "Winter")), 
         muskox_per_week = ifelse(muskox_per_week == 0, NA, muskox_per_week))

#plotting muskox_season_data to create seasonal plots of muskox frequency of detections 
ggplot() +
  tidyterra::geom_spatraster(data = cropped_SCANFI_TDN_Boundary) +
  geom_sf(data=TDN_boundary, fill = NA) +
  geom_sf(data = camera_locations, colour = "limegreen", size = 0.5) +
  geom_sf(data=muskox_season_data, aes(geometry = geometry, size=muskox_per_week)) +
  facet_wrap(~season, nrow = 2) +
  scale_color_manual(name = "camera", values = c("Camera Location" = "limegreen")) +
  theme_minimal() +
  theme(
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()) +
  ggtitle("Muskox Frequency of Detections by Landcover Type")

ggplot() +
  tidyterra::geom_spatraster(data = cropped_SCANFI_TDN_Boundary) +
  geom_sf(data = TDN_boundary, fill = NA) +
  geom_sf(data = camera_locations, aes(colour = "Camera Location"), size = 0.5) +  # Add aes() to map to the legend
  geom_sf(data = muskox_season_data, aes(geometry = geometry, size = muskox_per_week)) +
  facet_wrap(~season, nrow = 2) +
  scale_color_manual(name = "Camera", values = c("Camera Location" = "limegreen")) +  # Custom color scale
  theme_minimal() +
  theme(
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()) +
  ggtitle("Muskox Frequency of Detections by Landcover Type")


#  
ggplot() +
  tidyterra::geom_spatraster(data = fire_rast_cropped_TDN) +
  geom_sf(data=TDN_boundary, fill = "NA") +
  geom_sf(data = camera_locations, colour = "limegreen", size = 0.5) +
  geom_sf(data=muskox_season_data, aes(geometry = geometry, size=muskox_per_week)) +
  facet_wrap(~season, nrow = 2) +
  scale_fill_manual(values = c("#fecc5c","#fd8d3c","orangered","#e31a1c"), na.value = NA) +
  theme_minimal() +
  theme(
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),  # No background in the panel
    strip.background = element_blank(),   # Removes the background around the facet labels
    panel.border = element_blank()        # Removes any border around the panel
  ) +
  ggtitle("Muskox Frequency of Detections by Fire Age")

#########
ggplot() +
  tidyterra::geom_spatraster(data = fire_rast_cropped_TDN) +
  geom_sf(data = TDN_boundary, fill = "NA") +
  geom_sf(data = camera_locations, aes(colour = "Camera Location"), size = 0.5) +  # Add to legend
  geom_sf(data = muskox_season_data, aes(geometry = geometry, size = muskox_per_week)) +
  facet_wrap(~season, nrow = 2) +
  scale_fill_manual(values = c("#fecc5c","#fd8d3c","orangered","#e31a1c"), na.value = NA) +
  scale_color_manual(name = "camera", values = c("Camera Location" = "limegreen")) +  # Custom color for camera locations
  theme_minimal() +
  theme(
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    strip.background = element_blank(),
    panel.border = element_blank()
  ) +
  ggtitle("Muskox Frequency of Detections by Fire Age")

# Step 3: Crop the TDN_Boundary using st_intersection
cropped_TDN_Boundary <- st_intersection(TDN_boundary, nwt_boundary_row7)

ggplot() +
  tidyterra::geom_spatraster(data = cropped_SCANFI_TDN_Boundary) + 
  geom_sf(data = camera_locations, color = "red", size = 3) +  
  geom_sf(data = comb_overlap_SCANFI_and_selected_mammals_week_df$Muskox)
theme_minimal() +
  theme(
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  ) +
  ggtitle("Plot of cropped_SCANFI_TDN_boundary and camera_locations")

#trying to plot most_detected_habitat_summary
ggplot() +
  geom_sf(data = TDN_boundary$geometry) +
  geom_sf(data = most_detected_habitat_summary$muskox_abund, size = 3) +
  theme_minimal() +
  theme(
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  ) +
  labs(title = "TDN Sites by Habitat", color = "Habitat")

ggplot(most_detected_habitat_summary) +
  geom_sf(aes(fill = muskox_abund), color = "black") +  # Fill by muskox abundance
  scale_fill_viridis_c(option = "C", name = "Muskox Abundance") +  # Color scale
  labs(title = "Muskox Abundance by Habitat Type within TDN Boundary") +
  theme_minimal() +
  theme(legend.position = "right")

#code not running
# pivot_longer(cols = c("grizzly_bear", "gray_wolf", "Muskox"), 
#              names_to = "species", 
#              values_to = "detections") %>%
# group_by(habitat_type, species) %>%
# summarise(total_detections = sum(detections, na.rm = TRUE), .groups = "drop")

#trying this code from chatgpt:
most_detected_habitat %>%
  pivot_longer(cols = c("grizzly_bear", "gray_wolf", "Muskox"), 
               names_to = "species", 
               values_to = "detections") %>%
  group_by(habitat_type, species) %>%
  summarise(total_detections = sum(detections, na.rm = TRUE), .groups = "drop")

# Create a bar plot for abundance by landcover type for grizzly bears
ggplot(most_detected_habitat_summary, aes(x = habitat_type, y = grizzly_abund)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.3, color = "cornflowerblue", fill = "cornflowerblue") + # Make bars smaller
  #scale_fill_manual(values = c("muskox_abund" = "skyblue")) +  # Custom colors
  labs(x = "Landcover Type", y = "Grizzly Abundance", title = "Grizzly Bear Frequency of Detection by Landcover Type") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels if needed

# Create a bar plot for abundance by landcover type for muskox
ggplot(most_detected_habitat_summary, aes(x = habitat_type, y = muskox_abund)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.3, color = "cornflowerblue", fill = "cornflowerblue") + # Make bars smaller
  #scale_fill_manual(values = c("muskox_abund" = "skyblue")) +  # Custom colors
  labs(x = "Landcover Type", y = "Muskox Abundance", title = "Muskox Frequency of Detection by Landcover Type") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels if needed

# Create a bar plot for abundance by landcover type for gray wolves
ggplot(most_detected_habitat_summary, aes(x = habitat_type, y = wolf_abund)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.3, color = "cornflowerblue", fill = "cornflowerblue") + # Make bars smaller
  #scale_fill_manual(values = c("muskox_abund" = "skyblue")) +  # Custom colors
  labs(x = "Landcover Type", y = "Wolf Abundance", title = "Gray Wolf Frequency of Detection by Landcover Type") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels if needed

# Create a bar plot for abundance by landcover type for barren-ground caribou
ggplot(most_detected_habitat_summary, aes(x = habitat_type, y = caribou_abund)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.3, color = "cornflowerblue", fill = "cornflowerblue") + # Make bars smaller
  #scale_fill_manual(values = c("muskox_abund" = "skyblue")) +  # Custom colors
  labs(x = "Landcover Type", y = "Wolf Abundance", title = "Barren-ground Caribou Frequency of Detection by Landcover Type") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels if needed


#trying to make one bar graph with all 3 species

# Reshape the data into long format
most_detected_habitat_summary_long <- most_detected_habitat_summary %>%
  pivot_longer(cols = c(grizzly_abund, muskox_abund, wolf_abund, caribou_abund),
               names_to = "species",
               values_to = "abundance")

# Create a bar plot with different colors for each species
ggplot(most_detected_habitat_summary_long, aes(x = habitat_type, y = abundance, fill = species)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7) +  # Bar width adjusted
  scale_fill_manual(values = c("grizzly_abund" = "cornflowerblue", 
                               "muskox_abund" = "skyblue", 
                               "wolf_abund" = "lightgreen")) +  # Custom colors
  labs(x = "Landcover Type", y = "Abundance", title = "Abundance by Species and Landcover Type") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))  # Rotate x-axis labels if needed


ggplot(most_detected_habitat_summary_long, aes(x = habitat_type, y = abundance, fill = species)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7) +  # Bar width adjusted
  scale_fill_manual(values = c("grizzly_abund" = "aquamarine", 
                               "muskox_abund" = "dodgerblue", 
                               "wolf_abund" = "darkkhaki",
                               "caribou_abund" = "cadetblue")) +  # Custom colors
  labs(x = "Landcover Type", y = "Detection", title = "Species Frequency of Detection by Landcover Type") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 15, hjust = 1)) %>%  # Rotate x-axis labels if needed
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),  # Rotate x-axis labels
    axis.title.x = element_text(size = 14, face = "bold"),  # Make x-axis label thicker
    axis.title.y = element_text(size = 14, face = "bold"),  # Make y-axis label thicker
    plot.title = element_text(size = 16, face = "bold")  # Make title thicker
  )


#making monthly maps of species detections by landcover type

#separating out the data by week and month
species_all$date <- as.Date(species_all$image_date_time, format = "%Y-%m-%d")

#filtering out just muskox sightings
data <- species_all %>% mutate(year_month = floor_date(date, "month"))  %>%
  filter(species_common_name == "Muskox") %>% #if I want to filter by a species
  mutate(week = week(date), #if I want to create a column with week number
         month = month(date),
         year = year(date),
         individual_count = as.numeric(individual_count))

wolf_data <- species_all %>% mutate(year_month = floor_date(date, "month"))  %>%
  filter(species_common_name == "Gray Wolf") %>% #if I want to filter by a species
  mutate(week = week(date), #if I want to create a column with week number
         month = month(date),
         year = year(date),
         individual_count = as.numeric(individual_count))

grizzly_data <- species_all %>% mutate(year_month = floor_date(date, "month"))  %>%
  filter(species_common_name == "Grizzly Bear") %>% #if I want to filter by a species
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

#averaging wolf detections by week
avg_wolf_week_data <- wolf_data %>% 
  group_by(date, month, location) %>%
  summarise(date_count = max(individual_count)) %>%
  group_by(month, location) %>%
  summarise(month_count = mean(date_count) * 7)

#averaging wolf detections by week
avg_grizzly_week_data <- grizzly_data %>% 
  group_by(date, month, location) %>%
  summarise(date_count = max(individual_count)) %>%
  group_by(month, location) %>%
  summarise(month_count = mean(date_count) * 7)

#creating data frames to plot from  
data_cameras <- merge(data, TDN_Cameras, by="location")
month_data_cameras <- merge(month_data, TDN_Cameras, by="location")

wolf_cameras <- merge(wolf_data, TDN_Cameras, by="location")
wolf_month_data_cameras <- merge(avg_wolf_week_data, TDN_Cameras, by="location")

grizzly_cameras <- merge(grizzly_data, TDN_Cameras, by="location")
grizzly_month_data_cameras <- merge(avg_grizzly_week_data, TDN_Cameras, by="location")

#converting to sf objects
month_data_cameras_SF <-st_as_sf(month_data_cameras, coords=c("longitude", "latitude"), crs=4326, remove=FALSE)
data_cameras_SF <- st_as_sf(data_cameras, coords=c("longitude.x", "latitude.x"), crs=4326, remove=FALSE)

wolf_cameras_sf <-st_as_sf(wolf_cameras, coords=c("longitude.x", "latitude.x"), crs=4326, remove=FALSE)
wolf_month_data_cameras_sf <- st_as_sf(wolf_month_data_cameras, coords=c("longitude", "latitude"), crs=4326, remove=FALSE)

grizzly_cameras_sf <-st_as_sf(grizzly_cameras, coords=c("longitude.x", "latitude.x"), crs=4326, remove=FALSE)
grizzly_month_data_cameras_sf <- st_as_sf(grizzly_month_data_cameras, coords=c("longitude", "latitude"), crs=4326, remove=FALSE)

#Muskox count by month
ggplot() +
  geom_sf(data=TDN_boundary) +
  geom_sf(data=month_data_cameras_SF, aes(geometry = geometry, size=month_count)) +
  facet_wrap(~month, nrow = 4) +
  theme_minimal() +
  theme(
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank())


#wolf count by month
ggplot() +
  geom_sf(data=TDN_boundary) +
  tidyterra::geom_spatraster(data = cropped_SCANFI_TDN_Boundary) +
  geom_sf(data=wolf_month_data_cameras_sf, aes(geometry = geometry, size=month_count)) +
  geom_sf(data = wolf_month_data_cameras_sf, # Exclude zero counts
          aes(geometry = geometry,
              colour = factor(Wolf > 0),
              size = Moose)) +
  scale_fill_discrete(name = "Landcover Type") +
  #scale_color_manual(values = c("TRUE" = "orange", "FALSE" = "black"),
                     #labels = c("TRUE" = "Yes", "FALSE" = "No"),
                     #name = "Moose Sighted") +
  scale_size_continuous(name = "Number of Gray Wolves", range = c(2, 8),
                        breaks = seq(1, max(wolf_month_data_cameras_sf, na.rm = TRUE), by = 2)) + # Adjust size range
  facet_wrap(~month, nrow = 4) +
  theme_minimal() +
  theme(
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()) +
ggtitle("Average Wolf Detections Per Month") +
  guides(
    fill = guide_legend(order = 1), # Landcover Type first
    color = guide_legend(order = 2, override.aes = list(size = 2)), # Gray Wolf Detection second
    size = guide_legend(order = 3, override.aes = list(colour = "orange")) # Ensures large dots match moose color
  )

#grizzly count by month
ggplot() +
  geom_sf(data=TDN_boundary) +
  tidyterra::geom_spatraster(data = cropped_SCANFI_TDN_Boundary) +
  geom_sf(data=grizzly_month_data_cameras_sf, aes(geometry = geometry, size=month_count)) +
  facet_wrap(~month, nrow = 4) +
  theme_minimal() +
  theme(
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()) +
  ggtitle("Average Grizzly Bear Detections Per Month") +






moose_map <- ggplot() +
  geom_sf(data = TDN_ecoregions_2, aes(fill = ECO2_NAM_1), alpha = 0.5) +
  geom_sf(data = TDN_lakes_rivers, colour = "#6699CC") +
  geom_sf(data = ind_det_moose_sf, # Exclude zero counts
          aes(geometry = geometry,
              colour = factor(Moose > 0),
              size = Moose)) +
  scale_fill_discrete(name = "Ecoregion") +
  scale_color_manual(values = c("TRUE" = "orange", "FALSE" = "black"),
                     labels = c("TRUE" = "Yes", "FALSE" = "No"),
                     name = "Moose Sighted") +
  scale_size_continuous(name = "Number of Moose", range = c(2, 8),
                        breaks = seq(1, max(ind_det_moose$Moose, na.rm = TRUE), by = 2)) + # Adjust size range
  theme_minimal() +
  theme(
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  ) +
  guides(
    fill = guide_legend(order = 1), # Ecoregion first
    color = guide_legend(order = 2, override.aes = list(size = 2)), # Moose sighting second
    size = guide_legend(order = 3, override.aes = list(colour = "orange")) # Ensures large dots match moose color
  )


