

#############################################
#importing and processing data


#this package has the github function
#install.packages("devtools")

#install package WildRtrax
#remotes::install_github("ABbiodiversity/wildrtrax", force = TRUE)

#load the package
library(devtools)
library(wildrtrax)
library(dplyr)
library(tidyverse)


#load species data
species_all <- read.csv("~/Desktop/Analysis/Learning/learning/raw data/NWTBM_Thaidene_Nëné_Biodiversity_Project_2021_tag_report.csv")

#create independent detections
species_ind_data <- wt_ind_detect(
  x = species_all,
  threshold = 30,
  units = "minutes",
  datetime_col = image_date_time,
  remove_human = TRUE,
  remove_domestic = TRUE)

#Summarising the independent detections by month

summarised_month <- wt_summarise_cam(
  # Supply your detection data
  detect_data = species_ind_data,
  # Supply your raw image data
  raw_data = species_all,
  # Now specify the time interval you're interested in
  time_interval = "month",
  # What variable are you interested in?
  variable = "detections",
  # Your desired output format (wide or long)
  output_format = "wide",
  start_col_det = "start_time")


#colnames(species_ind_det)
#head(species_ind_det)
summarised_week <- species_ind_data

#Summarising the independent detections by week
summarised_week <- wt_summarise_cam(
  # Supply your detection data
  detect_data = species_ind_data,
  # Supply your raw image data
  raw_data = species_all,
  # Now specify the time interval you're interested in
  time_interval = "week",
  # What variable are you interested in?
  variable = "detections",
  # Your desired output format (wide or long)
  output_format = "wide",
  start_col_det = "start_time")


##pulling out the camera ID, week, n effort, griz and gray wolf and muskox 

# Select the first and second columns
selected_mammals_week <- summarised_week %>%
  select(2,3,4,5,28,31,41)


# Extract last 2 characters from the 'location' column because it is the camera site and create a new column 'camera'
selected_mammals_week <- selected_mammals_week%>%
  mutate(camera = substr(location, nchar(location) - 1, nchar(location)))

# Extract first 11 characters from the 'location' column and create a new column 'cluster'
selected_mammals_week <- selected_mammals_week%>%
  mutate(cluster = substr(location, 1, 11))

#renaming columns 
# Rename column 'old_name' to 'new_name'
selected_mammals_week <- selected_mammals_week%>%
  rename("gray_wolf" = "Gray Wolf")

selected_mammals_week <- selected_mammals_week%>%
  rename("grizzly_bear" = "Grizzly Bear")










