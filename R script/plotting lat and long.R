x <- c("site")
y <- c("n_detections")

plot(x,y)
install.packages("ggplot2")
install.packages("geom_point")

library("ggplot2")

geom_sf(data = TDN_muskox_detections_by_location)
geom_point(data =TDN_muskox_detections_by_location, aes(x = lon, y = lat))


install.packages("leaflet")
library(leaflet)
leaflet() %>%
  addTiles()
install.packages("dplyr")

map1 <- ggplot2(TDN_muskox_detections_by_location, aes(x = long, y = lat,))
read.csv()

install.packages(c("sf"))

# Install and load necessary packages
install.packages(c("ggplot2", "sf", "dplyr"))
library(ggplot2)
library(sf)
library(magrittr)
library(RColorBrewer)

loc<- read.csv('TDN_muskox_detections_by_location.csv')

# Example data frame with latitude and longitude
df <- data.frame(loc)

# Convert to an sf object
df_sf <- st_as_sf(df, coords = c("longitude", "latitude"), crs = 4326)

#read in TDN boundary
TDN <- st_read("TDN_Boundary.shp", package = "sf")

TDN <- st_read("TDN_Boundary.shp", package = "sf")

# Plot the data
ggplot() +
  geom_sf(data = TDN, color = "red", size = 3) +
  labs(title = "Map of Latitude and Longitude Points",
       x = "Longitude",
       y = "Latitude") +
  theme_minimal()

library(ggplot2)
installed.packages()["ggplot2", "Package"]

install.packages("sf")
library(sf)
?st_as_sf


#Load Packages
list.of.packages <- c(
  "leaflet",       # creates interactive maps
  "plotly",        # creates interactive plots   
  "kableExtra",    # Creates interactive tables 
  "tidyr",         # A package for data manipulation
  "dplyr",         # A package for data manipulation
  "viridis",       # Generates colors for plots  
  "corrplot",      # Plots pairwise correlations
  "lubridate",     # Easy manipulation of date objects
  "taxize",        # Package to check taxonomy 
  "sf")            # Package for spatial data analysis 

# Check you have them in your library
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
# load them
if(length(new.packages)) install.packages(new.packages,repos = "http://cran.us.r-project.org")
lapply(list.of.packages, require, character.only = TRUE)

#library(lubridate)
# day-month-year
dmy("24-12-2022")

# year-month-day
ymd("2022-12-24")

# Check you have them and load them
list.of.packages <- c("iNEXT", "kableExtra", "tidyr", "ggplot2", "gridExtra", "dplyr", "viridis")

new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only = TRUE)

list.of.packages <- c("iNEXT", "kableExtra", "tidyr", "ggplot2", "gridExtra", "dplyr", "viridis")

sp_summary <- read.csv("TDN_muskox_detections_by_locations", header=T)

# Use nrow() to count the number of species
nrow(sp_summary)

# Set the working directory to the folder containing the file
setwd("~/Desktop/muskox detections")

# Now try to open the file
read.csv("TDN_muskox_detections_by_locations")
dir()

install.packages("readxl")

library(readxl)

data <- read_excel("/Users/christinedunbar/Desktop/muskox detections")

getwd()
dir()

read_excel("TDN_muskox_detections_by_locations")
data <- read_excel("TDN_muskox_detections_by_locations")

setwd()
dir()

sf

