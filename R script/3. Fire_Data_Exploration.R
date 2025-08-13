
library(tidyverse)
library(sf)
library(terra)
library(readxl)

### Load fire data

fire_data_nbac <- sf::read_sf("~/Desktop/Analysis/Learning/learning/Spatial/shapefiles/NBAC/NBAC_1972to2024_20250506_shp") 



camera_locations <- read_csv("~/Desktop/Analysis/Learning/learning/raw data/Species Raw Data (May 2025)/NWTBM_Thaidene_Nëné_Biodiversity_Project_2021_location_report.csv") %>%
  drop_na("longitude") %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326) %>%
  st_transform(32612) #this number corresponds to the epsg code for utm code 12

#creating 300 m buffers for cameras
camera_buffer <- st_buffer(camera_locations, 300)

# creating camera buffer bounding box 
camera_buffer_bb <- camera_buffer %>%
  st_buffer(100000) %>%
  st_bbox() %>%
  st_as_sfc()

### crop fire data
fire_data_nbac_crop <- fire_data_nbac %>%
  sf::st_intersection(camera_buffer_bb %>% st_transform(st_crs(fire_data_nbac))) %>%
  sf::st_transform(32612)

saveRDS(fire_data_nbac_crop, saveRDS(fire_data_nbac_crop, "~/Desktop/Analysis/Learning/learning/RDS files/fire_data_nbac_crop.rds"))
fire_data_nbac_crop <- readRDS("RDS files/fire_data_nbac_crop.rds")

### combine cropped fire data
fire_data_subset <- fire_data_nbac_crop %>%
  # bind_rows(fire_data_nfdb_crop %>%
  #             filter(YEAR < 1972)) %>%
  sf::st_transform(4326) %>%
  mutate(fire_age = 
           case_when(
             YEAR >= 2022 - 10 ~ 0,  # 0-10 year old fires
             YEAR >= 2022 - 20 & YEAR < 2022 - 10 ~ 1,  # 11-20 year old fires
             YEAR >= 2022 - 30 & YEAR < 2022 - 20 ~ 2,  # 21-30 year old fires
             YEAR < 2022 - 30 ~ 3,  # > 30 year old fires
             TRUE ~ NaN))  # Assign NaN for any cases that don't match
  
temp <- fire_data_subset %>%
  st_drop_geometry()


### create a stacked raster of years since fire
temp_rast <- rast(fire_data_subset %>% st_transform(32612), resolution = 30) 

fire_rast <- fire_data_subset %>%
  st_transform(32612) %>%
  mutate(fire_age = factor(fire_age)) %>%
  rasterize(temp_rast,
            field = "fire_age",
            fun = "min")
 
Camera_buffer_zones <- rasterize(vect(camera_buffer), fire_rast, field = "location") 


# Resample fire_rast to match Camera_buffer_zones
fire_rast <- resample(fire_rast, Camera_buffer_zones, method = "near")

# Resample Camera_buffer_zones to match the resolution and extent of fire_rast
Camera_buffer_zones_resampled <- resample(Camera_buffer_zones, fire_rast, method = "near")

# Now you can run the crosstab function with the aligned rasters
fire_buffer_proportion <- crosstab(c(Camera_buffer_zones_resampled, fire_rast), long = TRUE, useNA = TRUE)

# Merge the datasets based on "location" (adding_variables_elevations_eskers) is in landcover exploration line 73
combined_variables <- adding_variables_elevations_eskers %>%
  left_join(fire_buffer_proportion %>% dplyr::select(location), by = "location")

#chatgpt code: 
fire_variables <- fire_buffer_proportion %>%
  as_tibble() %>%
  mutate(
    fire_age = as.numeric(fire_age),  # Ensure 'fire_age' is numeric
    fire_age = replace_na(fire_age, 4)  # Replace NA values with 4
  ) %>%
  drop_na(location) %>%
  group_by(location) %>%
  mutate(fire_prop = n / sum(n)) %>%
  mutate(fire_presence_or_absence = case_when(
    fire_age == 4 ~ 0,  # For values 4, assign 0 (absence)
    fire_age %in% 0:3 ~ 1  # For values 0 to 3, assign 1 (presence)
  )) %>%
  dplyr::select(fire_age, fire_prop, location) %>%
  pivot_wider(
    names_prefix = "fire_age",
    names_from = fire_age,  # Create a column for each fire age
    values_from = fire_prop,  # Take values from the 'fire_prop' column
    values_fill = list(fire_prop = 0)  # Fill missing values with 0
  )

# Reorder columns based on the numbers in their names
fire_variables <- fire_variables %>%
  dplyr::select(order(-as.numeric(sub("fire_age", "", names(.)))))  # Extract numbers and reorder columns

# Move the 'location' column to the very left
fire_variables <- fire_variables %>%
  dplyr::select(location, everything())

#adding missing variables 
comb_overlap_SCANFI_and_selected_mammals_week <- merge(model_variables, 
                                            camera_locations_df[, c("location", "Arctic_DEM_500m_elevation_m", "esker_camera_distances")], 
                                            by = "location", 
                                            all.x = TRUE)  # Keeps all rows from comb_overlap_SCANFI_and_selected_mammals_week

camera_locations_df <- camera_locations_df %>%
  left_join(
    model_variables %>% select(location, esker_camera_distances, Arctic_DEM_500m_elevation_m),
            by = "location"
  )

adding_variables_elevations_eskers <- merge(comb_overlap_SCANFI_and_selected_mammals_week, 
                                            camera_locations_df[, c("location", "Arctic_DEM_500m_elevation_m", "esker_camera_distances")], 
                                            by = "location", 
                                            all.x = TRUE)  # Keeps all rows from comb_overlap_SCANFI_and_selected_mammals_week



saveRDS(adding_variables_elevations_eskers,"~/Desktop/Analysis/Learning/learning/RDS files/adding_variables_elevations_eskers.rds") #this is where I have it saved on my desktop. This is to save the rds

adding_variables_elevations_eskers <- readRDS("~/Desktop/Analysis/Learning/learning/RDS files/adding_variables_elevations_eskers.rds") #this code is to read the rds file you saved above


# Merge the datasets by 'location'
all_variables <- fire_variables %>%
  left_join(adding_variables_elevations_eskers, by = "location") %>%
  mutate(log_esker_camera_distances = log(esker_camera_distances + 1))

#renaming columns
all_variables <- all_variables %>%
  rename(Elevation = `elevations`)

all_variables <- all_variables %>%
  select(-elevation)

#adding image_date_time" to all_variables 
all_variables_image_date_time <- all_variables %>%
  left_join(data %>% select(location, image_date_time), by = "location")

#renaming columns
all_variables_image_date_time <- all_variables_image_date_time %>%
  rename(Elevation = `elevations`)

#trying to extract month into its own column

all_variables_month <- all_variables_image_date_time %>%
  mutate(month = month(image_date_time))


#summarizing by month
monthly_summary_all_variables <- all_variables_month %>%
  group_by(month) %>%
  summarize(
    total_muskox = sum(Muskox, na.rm = TRUE),
    total_caribou = sum(`Barren-ground Caribou`, na.rm = TRUE),
    total_grizzly_bear = sum(grizzly_bear, na.rm = TRUE),
    total_gray_wolf = sum(gray_wolf, na.rm = TRUE),
    .groups = "drop"
  )



write_csv(all_variables, "all_variables.csv")



# Assuming both datasets have a 'location' column, we'll join by that column
combined_variables_and_fire <- comb_overlap_SCANFI_and_selected_mammals_week %>%
  left_join(fire_variables, by = "location") %>%
  left_join(camera_locations[, c("location", "distance_to_esker")]) %>%
  left_join(camera_locations_df[, c("location", "elevation")])

#deleting and renaming columns

comb_overlap_SCANFI_and_selected_mammals_week <- comb_overlap_SCANFI_and_selected_mammals_week %>%
  # Remove the unwanted .y columns
  dplyr::select(-fire_presence_or_absence.x, -fire_age.x, -n.x, -fire_prop.x) %>%
  # Rename the .x columns
  rename(
    fire_presence_or_absence = fire_presence_or_absence.y,
    fire_age = fire_age.y,
    n = n.y,
    fire_prop = fire_prop.y
  ) 

#trying to figure out which fire_age muskox are spending the most time in.

# Convert the data to long format
all_variables_long <- all_variables %>%
  pivot_longer(cols = starts_with("fire_age"),  # Select all columns starting with "fire_age"
               names_to = "fire_age",            # New column for the fire age category
               values_to = "proportion")    # New column for muskox detection

# Count the number of muskox detections (where detection > 0) for each fire age
muskox_fire_age_detection_summary <- all_variables_long %>%
  group_by(fire_age) %>%
  summarise(total_detections = sum(proportion > 0, na.rm = TRUE), .groups = "drop") %>%
  arrange(desc(total_detections))  # Sort by the number of detections


# View the summary
muskox_detection_summary

muskox_fire_age_detection_summary <- all_variables_long %>%
  group_by(location) %>%
  filter(proportion == max(proportion)) %>%
  group_by(fire_age) %>%
  summarize(muskox_fire_detection_sum = sum(Muskox), 
            num_days = sum(n_days_effort),
            muskox_detection_rate = muskox_fire_detection_sum/num_days*30) 


















