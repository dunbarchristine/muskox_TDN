

#############################################
#importing and processing data


#this package has the github function
install.packages("devtools")

#install package WildRtrax
remotes::install_github("ABbiodiversity/wildrtrax", force = TRUE)

#load the package
library(devtools)
library(wildrtrax)
library(dplyr)
library(tidyverse)

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
  start_col_det = "start_date")

summarised_month <- species_ind_data
colnames(species_ind_det)
head(species_ind_det)

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














