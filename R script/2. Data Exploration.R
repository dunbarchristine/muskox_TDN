####*********
### Lets make a plot with Muskox and compare detections between season
#load in packages

list.of.packages <- c("kableExtra", "tidyr", "ggplot2", "gridExtra", "dplyr", "Hmsc", "jtools", "lubridate", "corrplot", 
                      "MuMIn","stringr","sf","raster","leaflet", "tidyverse","htmlwidgets","webshot", "purrr", "magick",
                      "forcats","multcomp", "reshape2", "lme4", "tidyr", "stats")

install.packages("writexl")

library(writexl)
library(corrplot)
library(RColorBrewer)
library(dplyr)
library(ggplot2)
library(terra)
library(tidyverse)
library(raster)

summarized_week_data <- summarised_week %>%
  group_by(year, week) %>%
  summarise(Total_Count = sum(Muskox, na.rm = TRUE),
            n_effort = sum(n_days_effort, na.rm = TRUE),.groups = "drop") %>%
  mutate(year_week = paste(year, week, sep = "_"))

# new summarized data with camera effort
ggplot(summarized_week_data, aes(x = year_week, y = n_effort)) +
  geom_bar(stat = "identity", position = "dodge", color = "pink", fill = "pink") +
  labs(title = "Total Muskox Weekly CR Across Region by Week",
       x = "Week of the Year",
       y = "Total Muskox CR") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size=8)) 

# Generate values from "2023_1" to "2023_52"
excluded_weeks_2023 <- paste0("2023_", 1:52)
# View the result
print(excluded_weeks)

#generate values from "2022_39 to 2022_52
excluded_weeks_2022 <- paste0("2022_", 39:52)

# View the result
print(excluded_weeks_2022)

#removing certain dates from summarized_week_data2 as they are not relevant 
  summarized_week_data2 <- summarized_week_data %>%
  filter(!(year_week %in% c( "2021_9",  "2021_10", "2021_11", "2021_12",
                             "2021_13", "2021_14", "2021_15", "2021_16",
                             "2021_17", "2021_18", "2021_19", "2021_20",
                             "2021_21", "2021_22", "2021_23", "2021_24",
                             "2021_25", "2021_26", "2021_27", "2021_28",
                             "2021_29","2021_31", "2021_32", "2021_30", "2022_39","2022_40",
                             "2022_34", "2022_35", "2022_36", "2022_37", "2022_38",
                             "2022_41", "2022_42", "2022_43","2022_44",
                             "2022_45", "2022_46", "2022_47","2022_48",
                             "2022_49", "2022_50", "2022_51", "2022_52", 
                             "2023_1", "2023_2","2023_3", "2023_4", "2023_5", 
                             "2023_6", "2023_7","2023_8", "2023_9","2023_10",
                             "2023_11", "2023_12", "2023_13", "2023_14",
                             "2023_15", "2023_16","2023_17","2023_18",
                             "2023_19","2023_20","2023_21","2023_22","2023_23",
                             "2023_24", "2023_25", "2023_26", "2023_27", "2023_28",
                             "2023_29", "2023_30", "2023_31", "2023_32", "2023_33",
                             "2023_34","2023_35", "2023_36", "2023_37", "2023_38",
                             "2023_39", "2023_40", "2023_41", "2023_42", "2023_43",
                             "2023_44", "2023_45", "2023_46", "2023_47", "2023_48", 
                             "2023_49", "2023_50", "2023_51", "2023_52"
  )))

  
  weekly_season_data_clean <- summarized_week_data %>%
  filter(!(year_week %in% c( "2021_9",  "2021_10", "2021_11", "2021_12",
                             "2021_13", "2021_14", "2021_15", "2021_16",
                             "2021_17", "2021_18", "2021_19", "2021_20",
                             "2021_21", "2021_22", "2021_23", "2021_24",
                             "2021_25", "2021_26", "2021_27", "2021_28",
                             "2021_29","2021_31", "2021_32", "2021_30", "2022_39","2022_40",
                             "2022_34", "2022_35", "2022_36", "2022_37", "2022_38",
                             "2022_41", "2022_42", "2022_43","2022_44",
                             "2022_45", "2022_46", "2022_47","2022_48",
                             "2022_49", "2022_50", "2022_51", "2022_52", 
                             "2023_1", "2023_2","2023_3", "2023_4", "2023_5", 
                             "2023_6", "2023_7","2023_8", "2023_9","2023_10",
                             "2023_11", "2023_12", "2023_13", "2023_14",
                             "2023_15", "2023_16","2023_17","2023_18",
                             "2023_19","2023_20","2023_21","2023_22","2023_23",
                             "2023_24", "2023_25", "2023_26", "2023_27", "2023_28",
                             "2023_29", "2023_30", "2023_31", "2023_32", "2023_33",
                             "2023_34","2023_35", "2023_36", "2023_37", "2023_38",
                             "2023_39", "2023_40", "2023_41", "2023_42", "2023_43",
                             "2023_44", "2023_45", "2023_46", "2023_47", "2023_48", 
                            "2023_49", "2023_50", "2023_51", "2023_52" )))
  
# Convert year_week to a factor with ordered levels (so it orders the x axis labels of the histogram in ggplot in the same order as the input spreadsheet)
  weekly_season_data_clean3 <- summarized_week_data2 %>%
    mutate(year_week = factor(year_week, 
                              levels = sort(unique(year_week)), 
                              ordered = TRUE)) %>%
    mutate(year_week = factor(year_week, levels = unique(year_week)))
#oct 22, 2024: for some reason code above wasn't ordering x axis in numerical order, so created weekly_season_data_clean3 (below) and it worked..
  weekly_season_data_clean4 <- weekly_season_data_clean3 %>%
    mutate(year_week = factor(year_week, 
                              levels = sort(unique(year_week)), 
                              ordered = TRUE)) %>%
    mutate(year_week = factor(year_week, levels = unique(year_week)))
  
#adding zeros to single digits in "weekly_season_data_clean2"
  weekly_season_data_clean2$week <- sprintf("%02d", weekly_season_data_clean2$week)

#plot the Capture rate
ggplot(summarized_week_data2, aes(x = year_week, y = Total_Count)) +
  geom_bar(stat = "identity", position = "dodge", color = "pink", fill = "pink") +
  labs(title = "Total Muskox Weekly CR Across Region by Week",
       x = "Week of the Year",
       y = "Total Muskox CR") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size=8)) 

# Adding a new column for seasons
weekly_season_data_clean4 <- summarized_week_data2 %>%
  mutate(season = case_when(
    week %in% 1:9 ~ "Winter",   # Weeks 1 to 9
    week %in% 10:22 ~ "Spring",   # Weeks 10 to 22
    week %in% 23:35 ~ "Summer",   # Weeks 23 to 35
    week %in% 36:45 ~ "Fall",     # Weeks 36 to 45
    week %in% 46:52 ~ "Winter", #weeks 46 to 52
    TRUE ~ "Unknown"))

#fill aesthetic to 'seasons' column:
ggplot(weekly_season_data_clean4, aes(x = year_week, y = Total_Count/n_effort, fill = season)) +
  geom_bar(stat = "identity", position = "dodge", color = "black") +
  scale_fill_manual(values = c("Winter" = "cornflowerblue", 
                               "Spring" = "darkgoldenrod1", 
                               "Summer" = "darkolivegreen4", 
                               "Fall" = "burlywood4")) +
  labs(title = "Average Daily Muskox Detections",
       x = "Week of the Year",
       y = "Weekly Muskox Per Trigger") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  theme(axis.text.x = element_text(size = 10)) # Adjust size as needed

####*********
### Lets make a plot with Caribou Moose and Muskox and compare detections between season

#Make a data set from summarized_week dataset that includes muskox, caribou and moose 

#change name of barren-ground caribou to barren_ground_caribou

summarised_week <- summarised_week%>%
  rename("Barren_Ground_Caribou" = "Barren-ground Caribou")

#first make the dataset with all 3 species
summarised_week_data_3_species <- summarised_week %>%
  group_by(year, week) %>%
  summarise(
    Total_Count_muskox = sum(Muskox, na.rm = TRUE),
    Total_Count_moose = sum(Moose, na.rm = TRUE),
    Total_Count_caribou = sum(Barren_Ground_Caribou, na.rm = TRUE),
    n_effort = sum(n_days_effort, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(year_week = paste(year, week, sep = "_"))

#filter out weeks we dont need 
#removing certain dates from summarized_week_data2 as they are not relevant 
summarised_week_data_3_species  <- summarised_week_data_3_species  %>%
  filter(!(year_week %in% c( "2021_9",  "2021_10", "2021_11", "2021_12",
                             "2021_13", "2021_14", "2021_15", "2021_16",
                             "2021_17", "2021_18", "2021_19", "2021_20",
                             "2021_21", "2021_22", "2021_23", "2021_24",
                             "2021_25", "2021_26", "2021_27", "2021_28",
                             "2021_29","2021_31", "2021_32", "2021_30", "2022_39","2022_40",
                             "2022_34", "2022_35", "2022_36", "2022_37", "2022_38",
                             "2022_41", "2022_42", "2022_43","2022_44",
                             "2022_45", "2022_46", "2022_47","2022_48",
                             "2022_49", "2022_50", "2022_51", "2022_52", 
                             "2023_1", "2023_2","2023_3", "2023_4", "2023_5", 
                             "2023_6", "2023_7","2023_8", "2023_9","2023_10",
                             "2023_11", "2023_12", "2023_13", "2023_14",
                             "2023_15", "2023_16","2023_17","2023_18",
                             "2023_19","2023_20","2023_21","2023_22","2023_23",
                             "2023_24", "2023_25", "2023_26", "2023_27", "2023_28",
                             "2023_29", "2023_30", "2023_31", "2023_32", "2023_33",
                             "2023_34","2023_35", "2023_36", "2023_37", "2023_38",
                             "2023_39", "2023_40", "2023_41", "2023_42", "2023_43",
                             "2023_44", "2023_45", "2023_46", "2023_47", "2023_48", 
                             "2023_49", "2023_50", "2023_51", "2023_52")))

#adding zeros to single digits in "weekly_season_data_clean2"
summarised_week_data_3_species <- summarised_week_data_3_species %>%
  mutate(week = sprintf("%02d",as.numeric(week)))
  
summarised_week_data_3_species <- summarised_week_data_3_species %>%
  mutate(year_week = paste(year, week, sep = "_"))

#reshape dataset from wide to long format

long_data <- summarised_week_data_3_species %>%
  pivot_longer(cols = c(Total_Count_muskox, Total_Count_moose, Total_Count_caribou),
               names_to = "species",
               values_to = "count")

#make plot for 3 species 

# Create the histogram plot
ggplot(long_data, aes(x = year_week, y = count, fill = species)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Counts of Ungulate Species Per Week",
       x = "Week",
       y = "Weekly Species Detections Per Total Trigger") +
  facet_wrap(~species, nrow  = 2) +
  theme_minimal() +
  scale_fill_manual(values = c("Total_Count_muskox" = "cornflowerblue", 
                               "Total_Count_moose" = "darkgoldenrod1", 
                               "Total_Count_caribou" = "darkolivegreen")) +
  theme(legend.title = element_blank()) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))


#fill aesthetic to 'seasons' column:
ggplot(summarised_week_data_3_species, aes(x = year_week, y = Total_Count/n_effort, fill = season)) +
  geom_bar(stat = "identity", position = "dodge", color = "black") +
  scale_fill_manual(values = c("Winter" = "cornflowerblue", 
                               "Spring" = "darkgoldenrod1", 
                               "Summer" = "darkolivegreen4", 
                               "Fall" = "burlywood4")) +
  labs(title = "Average Daily Muskox Detections",
       x = "Week of the Year",
       y = "Weekly Muskox Detections Per Total Trigger") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  theme(axis.text.x = element_text(size = 10)) # Adjust size as needed


#clean data in TDN_ungulates_only

# Example dataset
TDN_ungulates_only<- data.frame(image_date_time = c("2023-08-22 11:39:49", "2023-08-23 12:40:50"))

# Convert 'image_date_time' to POSIXct date-time format
TDN_ungulates_only$image_date_time <- ymd_hms(TDN_ungulates_only$image_date_time)

# Extract week, month, and year
TDN_ungulates_only_clean <- TDN_ungulates_only %>%
  mutate(
    week = isoweek(image_date_time),  # ISO week number
    month = month(image_date_time),    # Month number
    year = year(image_date_time)        # Year
  )

# View the updated dataset
print(TDN_ungulates_only_clean)

#combining year and week columns

# Assuming your_data already has 'year' and 'week' columns
TDN_ungulates_only_clean2 <- TDN_ungulates_only_clean %>%
  mutate(year_week = paste(year, week, sep = "_"))  # Combine year and week with an underscore

# View the updated dataset
print(your_data)

#filter out 2023 from ungulate data 
TDN_unugulates_super_clean <- TDN_ungulates_only_clean2%>%
  filter(!(year_week %in% c( "2021_9",  "2021_10", "2021_11", "2021_12",
                             "2021_13", "2021_14", "2021_15", "2021_16",
                             "2021_17", "2021_18", "2021_19", "2021_20",
                             "2021_21", "2021_22", "2021_23", "2021_24",
                             "2021_25", "2021_26", "2021_27", "2021_28",
                             "2021_29","2021_31", "2021_32", "2021_30", "2022_39","2022_40",
                             "2022_34", "2022_35", "2022_36", "2022_37", "2022_38",
                             "2022_41", "2022_42", "2022_43","2022_44",
                             "2022_45", "2022_46", "2022_47","2022_48",
                             "2022_49", "2022_50", "2022_51", "2022_52", 
                             "2023_1", "2023_2","2023_3", "2023_4", "2023_5", 
                             "2023_6", "2023_7","2023_8", "2023_9","2023_10",
                             "2023_11", "2023_12", "2023_13", "2023_14",
                             "2023_15", "2023_16","2023_17","2023_18",
                             "2023_19","2023_20","2023_21","2023_22","2023_23",
                             "2023_24", "2023_25", "2023_26", "2023_27", "2023_28",
                             "2023_29", "2023_30", "2023_31", "2023_32", "2023_33",
                             "2023_34","2023_35", "2023_36", "2023_37", "2023_38",
                             "2023_39", "2023_40", "2023_41", "2023_42", "2023_43",
                             "2023_44", "2023_45", "2023_46", "2023_47", "2023_48", 
                             "2023_49", "2023_50", "2023_51", "2023_52" )))


#creating graphs that show camera activity throughout the year (when they were triggered, breaks between triggers, etc)

tdn_raw_camera <- read.csv("~/Desktop/Analysis/Learning/learning/SpeciesRawData (Oct 31)/NWTBM_Thaidene_Nëné_Biodiversity_Project_2021_main_report.csv")


tdn_camera_active_dates <- tdn_raw_camera %>%
  ### convert date time to date
  mutate(datetime = as.POSIXct(image_date_time),
         date = as.Date(datetime)) %>%
  select(location, date) %>%
  ### identifying unique camera-date combinations
  distinct() %>%
  ### identifying start and end dates of continous camera activity periods
  group_by(location) %>%
  arrange(date) %>%
  mutate(
    ### calculate the number of days between successive camera dates to identify
    ### periods of inactivity
    day_lag = date - lag(date,1),
    ### flag starts of periods of continuous activity periods
    flag = ifelse(is.na(day_lag)|day_lag>1,1,0),
    ### give a unique id to each period of continuous activity
    period_id = cumsum(flag),
    ### combine camera id with unique period id
    location_period_id = str_c(location,"_",period_id)) %>%
  group_by(location, period_id) %>%
  ### keep only the start and end dates of each continuous period
  filter(date==min(date)|date==max(date))

# Filter the data to remove rows before August 2021
tdn_camera_active_dates <- tdn_camera_active_dates %>%
  filter(date >= as.Date("2021-08-01"))

camera_id <- unique(tdn_camera_active_dates$location)

tdn_camera_active_dates %>%
  filter(location %in% camera_id[1:60]) %>%
  ggplot(aes(x = location, y = date, 
             group = location_period_id,
             colour = factor(period_id))) +
  geom_line() +
  coord_flip() +
  scale_colour_manual(values = rep(c("black","red"),30)) +
  guides(colour = "none") +
  ylab("Date") +
  xlab("Camera ID") +
  theme_bw()

####### creating histograms for muskox detections by week and month in spring, summer,fall, and winter for 2021, 2022 (note that cameras weren't deployed until Aug 2021 so no graph for spring 2021)
#using the gov of Canada data for when seasons official start. winter doesn't officially start until December 21st in 2021 and 2022. no winter graph for 2021 as there is no data for Jan, Feb, march.
#cameras were removed by aug 2022, around week 34. So no histogram for fall 2022. 

#first, start with histogram for weekly muskox detections in the summer of 2021.

  # Filter data to include only weeks 29 to 38 of 2021 (summer)
  summarized_week_data_summer<- summarized_week_data %>%
    filter(year == 2021 & week >= 29 & week <= 38)  # Filter for 2021 and weeks 22 to 36
  
  # Now plot the histogram with the filtered data
  ggplot(summarized_week_data_summer, aes(x = year_week, y = Total_Count)) +
    geom_bar(stat = "identity", position = "dodge", color = "cornflowerblue", fill = "cornflowerblue") +
    labs(title = "Total Muskox Weekly Detections for Summer 2021",
         x = "Week of the Year",
         y = "Total Muskox Detection") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 8))
  
## now making a histogram for the fall of 2021.  

  # Filter data to include only weeks 39 to 52 of 2021 (Fall)
  summarized_week_data_fall <- summarized_week_data %>%
    filter(year == 2021 & week >= 39 & week <= 52)  # Filter for 2021 and weeks 22 to 36
  
  # Now plot the histogram with the filtered data
  ggplot(summarized_week_data_fall, aes(x = year_week, y = Total_Count)) +
    geom_bar(stat = "identity", position = "dodge", color = "cornflowerblue", fill = "cornflowerblue") +
    labs(title = "Total Muskox Weekly Detections for Fall 2021",
         x = "Week of the Year",
         y = "Total Muskox Detection") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 8))

  ## now making a histogram for the winter of 2022.   
  
  #adding zeros to single digits in year_week column
  summarized_week_data_winter_2022 <- summarized_week_data %>%
    mutate(year_week = str_pad(week, width = 2, side = "left", pad = "0"))
  
  # Filter data to include only weeks 1 to 11 of 2022 (winter)
  summarized_week_data_winter_2022 <- summarized_week_data %>%
    filter(year == 2022 & week >= 1 & week <= 11)  # Filter for 2021 and weeks 22 to 36
  
  # Adding 2022 to the week number in the 'year_week' column
  summarized_week_data_winter_2022 <- summarized_week_data_winter_2022 %>%
    mutate(year_week = paste0("2022_", str_pad(week, width = 2, side = "left", pad = "0")))
  
  # Convert year_week to a factor with ordered levels (so it orders the x axis labels of the histogram in ggplot in the same order as the input spreadsheet)
  summarized_week_data_winter_2022 <- summarized_week_data_winter_2022 %>%
    mutate(year_week = factor(year_week, 
                              levels = sort(unique(year_week)), 
                              ordered = TRUE)) %>%
    mutate(year_week = factor(year_week, levels = unique(year_week)))
  
  
  # Now plot the histogram with the filtered data
  ggplot(summarized_week_data_winter_2022, aes(x = year_week, y = Total_Count)) +
    geom_bar(stat = "identity", position = "dodge", color = "cornflowerblue", fill = "cornflowerblue") +
    labs(title = "Total Muskox Weekly Detections for Winter 2022",
         x = "Week of the Year",
         y = "Total Muskox Detection") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 8))
  
  
  ## now making a histogram for the summer of 2022. 
  
  # Filter data to include only weeks 29 to 38 of 2022 (summer)
  summarized_week_data_summer_2022 <- summarized_week_data %>%
    filter(year == 2022 & week >= 29 & week <= 38 )  # Filter for 2021 and weeks 22 to 36
  
  # Now plot the histogram with the filtered data
  ggplot(summarized_week_data_summer_2022, aes(x = year_week, y = Total_Count)) +
    geom_bar(stat = "identity", position = "dodge", color = "cornflowerblue", fill = "cornflowerblue") +
    labs(title = "Total Muskox Weekly Detections for Summer 2022 ",
         x = "Week of the Year",
         y = "Total Muskox Detection") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 8))

#### running basic correlations. using selected_mammals_week data set produced in the data processing code 
  
cor.test(muskox detections, wolf detections)

wolf_grizz_musk <- selected_mammals_week %>%
  select(5,6,7)

# Step 1: Calculate the correlation matrix
cor_matrix <- cor(wolf_grizz_musk)
corrplot(cor_matrix, type="upper", order="hclust",
         col=brewer.pal(n=8, name="RdYlBu"))
#christine and frances meeting dec 13 ------
#here is where my meeting with frances started. come back and remove zeros from 2021 and rerun 
#histogram weekly detections 
  
hist(wolf_grizz_musk$Muskox) #histogram weekly detections of muskox (there is a ton of zeros)
hist(wolf_grizz_musk$grizzly_bear)
hist(wolf_grizz_musk$gray_wolf)

#histogram monthly detections
hist(summarised_month$Muskox)
hist(summarised_month$`Grizzly Bear`)
hist(summarised_month$`Gray Wolf`)
summary(summarised_month$Muskox)

plot(summarised_month$Muskox)
plot(summarised_month$`Grizzly Bear`, summarised_month$Muskox)
plot(summarised_month$`Gray Wolf`, summarised_month$Muskox)
cor.test(summarised_month$`Grizzly Bear`, summarised_month$Muskox)

plot(summarised_month$`Grizzly Bear`, summarised_month$Muskox,
     xlab = "Grizzly Bear", ylab = "Muskox")

plot(summarised_month$`Gray Wolf`, summarised_month$Muskox,
     xlab = "Gray Wolf", ylab = "Muskox")


#not a very strong correlation 
#data:  summarised_month$`Grizzly Bear` and summarised_month$Muskox
#t = 0.7502, df = 3955, p-value = 0.4532
#alternative hypothesis: true correlation is not equal to 0
#95 percent confidence interval:
  #-0.01923838  0.04307157
#sample estimates:
  #cor 
#0.01192817 

plot(summarised_month$`Gray Wolf`, summarised_month$Muskox)
cor.test(summarised_month$`Gray Wolf`, summarised_month$Muskox)
#not strong but moreso than grizzlies 
#meeting with frances finished at 2:55 mst


#
#adding seasons to selected_mammals_week data set

# Adding a new column for seasons
selected_mammals_week <- selected_mammals_week %>%
  mutate(season = case_when(
    week %in% 1:10 ~ "Winter",   # Weeks 1 to 10
    week %in% 11:24 ~ "Spring",   # Weeks 11 to 24
    week %in% 25:37 ~ "Summer",   # Weeks 25 to 37
    week %in% 38:51 ~ "Fall",     # Weeks 36 to 45
    week %in% 52:52 ~ "Winter",   # weeks 52 to 52
    TRUE ~ "Unknown"))

#adding zeros to the column "year_week"
selected_mammals_week$year_week <- str_pad(selected_mammals_week$year_week, width = 2, pad = "0")

#exporting "selected_mammals_week" to excel 

# Filter data for summer and winter seasons
filtered_muskox_data_hist <- selected_mammals_week %>%
  filter(season %in% c("Summer", "Winter")) %>%
  select(Muskox, season, week, year)

# Assuming your data frame is called 'data'
# Filter data for the year 2021
filtered_muskox_2021_data <- selected_mammals_week %>%
  filter(year == 2021)

filtered_muskox_2022_data <- selected_mammals_week %>%
  filter(year == 2022)

#adding week and year to the same column

selected_mammals_week <- selected_mammals_week %>%
  mutate(year_week = paste(year, week, sep = "-"))

filtered_muskox_2021_data <- filtered_muskox_2021_data %>%
  mutate(year_week = paste(year, week, sep = "-"))

filtered_muskox_2022_data <- filtered_muskox_2022_data %>%
  mutate(year_week = paste(year, week, sep = "-"))

filtered_muskox_data_hist <- filtered_muskox_data_hist %>%
  mutate(year_week = paste(year, week, sep = "-"))


#####################################################
# Create the histogram for summer/winter/fall 2021

# Group by 'year_week' and sum 'count'
musk_sum_counts_2021 <- filtered_muskox_2021_data %>%
  group_by(year_week) %>%
  summarise(total_count = sum(Muskox))

# Extract last 2 characters from the 'year-week' column because it is the week and create a new column 'week'
musk_sum_counts_2021 <- musk_sum_counts_2021%>%
  mutate(week = substr(year_week, nchar(year_week) - 1, nchar(year_week)))

# Remove minus signs
musk_sum_counts_2021 <- musk_sum_counts_2021%>%
  mutate(week = gsub("-", "", week))

# Filter the 2021 data to exclude weeks 1-30. Cameras were deployed after that
musk_sum_counts_2021 <- musk_sum_counts_2021%>%
  filter(as.numeric(str_sub(year_week, 6, 7)) > 30)  # Assuming year_week format is "YYYY-WW"

# Adding a new column for seasons
musk_sum_counts_2021<- musk_sum_counts_2021 %>%
  mutate(season = case_when(
    week %in% 1:10 ~ "Winter",   # Weeks 1 to 10
    week %in% 11:24 ~ "Spring",   # Weeks 11 to 24
    week %in% 25:37 ~ "Summer",   # Weeks 25 to 37
    week %in% 38:51 ~ "Fall",     # Weeks 36 to 45
    week %in% 52:52 ~ "Winter",   # weeks 52 to 52
    TRUE ~ "Unknown"))

#Plot the histogram with ggplot2
ggplot(musk_sum_counts_2021, aes(x = year_week, y = total_count, fill = season)) +
  geom_bar(stat = "identity", position = "dodge", color = "black") +
  labs(
    title = "Weekly Independent Muskox Detections by Season (2021)",
    x = "Year-Week",
    y = "Number of Muskox Detections"
  ) +
  scale_fill_manual(values = c("Summer" = "blue", "Fall" = "red")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  theme(axis.text.x = element_text(size = 10)) # Adjust size as needed  

 ############## Create the histogram for summer/winter/fall 2022

# Filter the dataset to exclude weeks 39 to 52
filtered_muskox_2022_data <- filtered_muskox_2022_data %>%
  filter(as.numeric(str_sub(year_week, 6, 7)) < 39) 

#muskox_total_weekly_summary <- filtered_muskox_2022_data %>%
  #group_by(week, camera) %>%
  #summarize(Muskox = n(), .groups = "drop")

# Group by 'year_week' and sum 'count'
musk_sum_counts_2022 <- filtered_muskox_2022_data %>%
  group_by(year_week) %>%
  summarise(total_count = sum(Muskox))

# Extract last 2 characters from the 'year-week' column because it is the week and create a new column 'week'
musk_sum_counts_2022 <- musk_sum_counts_2022%>%
  mutate(week = substr(year_week, nchar(year_week) - 1, nchar(year_week)))

# Remove minus signs
musk_sum_counts_2022 <- musk_sum_counts_2022%>%
  mutate(week = gsub("-", "", week))


# Adding a new column for seasons
musk_sum_counts_2022 <- musk_sum_counts_2022 %>%
  mutate(season = case_when(
    week %in% 1:10 ~ "Winter",   # Weeks 1 to 10
    week %in% 11:24 ~ "Spring",   # Weeks 11 to 24
    week %in% 25:37 ~ "Summer",   # Weeks 25 to 37
    week %in% 38:51 ~ "Fall",     # Weeks 36 to 45
    week %in% 52:52 ~ "Winter",   # weeks 52 to 52
    TRUE ~ "Unknown"))

# Add leading zeros to single digits in the 'week' column
musk_sum_counts_2022<- musk_sum_counts_2022 %>%
  mutate(week = str_pad(week, width = 2, side = "left", pad = "0"))

musk_sum_counts_2022 <- musk_sum_counts_2022 %>%
  mutate(year_week = str_replace(year_week, "(\\d{4}-(\\d{1}))", "\\1-0\\2"))

# Add leading zeros to single digits in the 'year_week' column
musk_sum_counts_2022 <- musk_sum_counts_2022 %>%
  mutate(
    year_week = str_replace(year_week, "(\\d{4}-(\\d))", "\\1-0\\2")  # Add leading zero to single-digit weeks
  )

#Plot the histogram with ggplot2 (not working atm, adding extra digits to x axis labels)
ggplot(musk_sum_counts_2022, aes(x = year_week, y = total_count, fill = season)) +
geom_bar(stat = "identity", position = "dodge", color = "black") +
labs(
title = "Weekly Independent Muskox Detections by Season 2022",
x = "Year-Week",
y = "Number of Muskox Detections"
) +
scale_fill_manual(values = c("Summer" = "blue", "Fall" = "red", "Winter" = "pink", "Spring" = "yellow")) +
theme_minimal() +
theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
theme(axis.text.x = element_text(size = 10)) # Adjust size as needed  

#exporting "select_mammals_week" to an excel file
# Write to Excel
write_xlsx(selected_mammals_week, "selected_mammals_week.xlsx")

getwd()

#getting independent detections for muskox, grizzlies, and gray wolves PER season

# Summarize the independent detections for each species by season
independent_detections_per_season <- selected_mammals_week %>%
  # Filter rows where at least one species was detected
  filter(Muskox == 1 | gray_wolf == 1 | grizzly_bear == 1) %>%
  # Group by season and location (and year_week if necessary for more precision)
  group_by(season, location) %>%
  # Keep only the first detection per season-location combination for each species
  summarize(
    muskox_detection = max(Muskox),           # Only keep the first detection of muskox
    wolves_detection = max(gray_wolf),      # Only keep the first detection of wolves
    grizzly_detection = max(grizzly_bear)    # Only keep the first detection of grizzly bears
  ) %>%
  # Ungroup so we can summarize by season
  ungroup() %>%
  # Now, summarize by season to get the total independent detections for each species
  group_by(season) %>%
  summarize(
    independent_muskox = sum(muskox_detection),
    independent_wolves = sum(wolves_detection),
    independent_grizzly = sum(grizzly_detection)
  )

# View the result
print(independent_detections_per_season)

#adding the land cover names column to overlap_SCANFI_cameras_table

overlap_SCANFI_cameras_table <- overlap_SCANFI_cameras_table %>%
  mutate(land_cover_name = case_when(
    SCANFI_att_nfiLandCover_SW_2020_v1.2 == 1 ~ "Bryoid",
    SCANFI_att_nfiLandCover_SW_2020_v1.2 == 2 ~ "Herbs",
    SCANFI_att_nfiLandCover_SW_2020_v1.2 == 3 ~ "Rock",
    SCANFI_att_nfiLandCover_SW_2020_v1.2 == 4 ~ "Shrub",
    SCANFI_att_nfiLandCover_SW_2020_v1.2 == 5 ~ "Treed broadleaf",
    SCANFI_att_nfiLandCover_SW_2020_v1.2 == 6 ~ "Treed conifer",
    SCANFI_att_nfiLandCover_SW_2020_v1.2== 7 ~ "Treed mixed",
    SCANFI_att_nfiLandCover_SW_2020_v1.2 == 8 ~ "Water"
  ))

# Spread the data into separate columns for each land cover type
overlap_SCANFI_cameras_table <- overlap_SCANFI_cameras_table %>%
select(-n, -SCANFI_att_nfiLandCover_SW_2020_v1.2) %>%  
  pivot_wider(
    names_from = land_cover_name,  # Create a column for each land cover type
    values_from = landcover_prop,  # Take values from the 'land_cover_prop' column
    values_fill = list(landcover_prop = 0)  # Fill missing values with 0 (no land cover)
  )

#combining overlap_SCANFI_cameras_table and selected_mammals_week to become one dataset called

# Merge the datasets based on a common column (e.g., "location")
comb_overlap_SCANFI_and_selected_mammals_week<- merge(overlap_SCANFI_cameras_table, 
                       selected_mammals_week, 
                       by = "location",   # Column in both datasets that should be used to join
                       all.x = TRUE)      # Keep all rows from overlap_SCANFI_cameras_table (left join)

plot(comb_overlap_SCANFI_and_selected_mammals_week)

#exporting "select_mammals_week" to an excel file
# Write to Excel
write_xlsx(comb_overlap_SCANFI_and_selected_mammals_week, "comb_overlap_SCANFI_and_selected_mammals_week.xlsx")


# Create the tibble with the necessary data
wolf_grizz_musk <- tibble(
  x = rep(1:5, each = 5),
  y = rep(1:5, times = 5),
  value = c(1, 2, 3, 4, 5, 2, 3, 4, 5, 6, 3, 4, 5, 6, 7, 
            4, 5, 6, 7, 8, 5, 6, 7, 8, 9)
)

################################################elevation 
#playing with esker data. want to create map that shows eskers and camera sites

# Check for empty geometries
esker_data_empty <- esker_data[st_is_empty(esker_data), ]
esker_data_non_empty <- esker_data[!st_is_empty(esker_data), ]

# Display number of empty geometries
nrow(esker_data_empty)
nrow(esker_data_non_empty)

# Remove empty geometries
esker_data <- esker_data[!st_is_empty(esker_data), ]

# Remove the Z dimension
esker_data <- st_zm(esker_data)

# Convert the sf object to a Spatial object
esker_sp <- as(esker_data, "Spatial")

# Rasterize
# Assuming 'esker_sp' is already a SpatialLinesDataFrame or a SpatVector
# Make sure it is a SpatVector
esker_sp <- vect(esker_sp)

# Rasterize using terra::rasterize
esker_rast <- rasterize(esker_sp, rast)


#getting distance between cameras and eskers

rast <- terra::rast(esker_data, resolution = 30)
esker_rast <- rasterize(esker_data, rast)
esker_dist <- distance(esker_data) %>%
(Camera_buffer_zones,
    crs_epsg = 32609,
    buffer_dist = 10000
  )

#erics code for elevation

# Reshape data for plotting and calculate average mean elevation and standard deviation for each buffer size
dem_extract_plot <- dem_extract_join %>%
  pivot_longer(cols = starts_with("mean_elev"), names_to = "Buffer_Size", values_to = "Mean_Elevation") %>%
  mutate(Buffer_Size = factor(Buffer_Size, levels = c("mean_elev_300", "mean_elev_550", "mean_elev_1622", "mean_elev_3245"))) %>%
  group_by(Buffer_Size) %>%
  summarise(Avg_Mean_Elevation = mean(Mean_Elevation),
            SD = sd(Mean_Elevation))

# Fit a linear regression model
lm_model <- lm(Avg_Mean_Elevation ~ as.numeric(Buffer_Size), data = dem_extract_plot)

# Create the plot with error bars and manually add the regression line
ggplot(dem_extract_plot, aes(x = Buffer_Size, y = Avg_Mean_Elevation)) +
  geom_point() +
  geom_errorbar(aes(ymin = Avg_Mean_Elevation - SD, ymax = Avg_Mean_Elevation + SD), width = 0.2) +
  geom_abline(intercept = coef(lm_model)[1], slope = coef(lm_model)[2], color = "blue") +  # Add a manual linear trend line
  labs(title = "Average Mean Elevation Across Buffer Sizes with Error Bars and Trend Line",
       x = "Buffer Size",
       y = "Average Mean Elevation") 

################
#trying to create elevation map using ArcticDEM (code from erics github)

#Load tdn boundary data

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
camera_locations_df$elevation <- elevations

# Print the resulting table
print(camera_locations_df)





























##############################################################
#trying to see what habitats grizzlies and gray wolves are spending the most time in
most_detected_habitat <- as.data.frame(most_detected_habitat)

# Step 1: Reshape the data to long format
most_detected_habitat <- comb_overlap_SCANFI_and_selected_mammals_week %>%
  dplyr::select(location, Shrub, Herbs, Water, `Treed mixed`, `Treed broadleaf`, `Treed conifer`, gray_wolf, grizzly_bear, n_days_effort) %>%
  pivot_longer(cols = c("Shrub", "Herbs", "Water", "Treed mixed", "Treed broadleaf", "Treed conifer"), 
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

# Step 2: Summarize the detections by species and habitat type
most_detected_habitat_summary <- most_detected_habitat %>%
  dplyr::select(grizzly_bear, gray_wolf, habitat_type, n_days_effort) %>%  # Explicitly use dplyr::select()
  group_by(habitat_type) %>%
  summarise(grizzly_abund = sum(grizzly_bear)/sum(n_days_effort)*1000,
            wolf_abund = sum(gray_wolf)/sum(n_days_effort)*1000,
            num_cameras = length(unique(location)))

  pivot_longer(cols = c("grizzly_bear", "gray_wolf"), 
               names_to = "species", 
               values_to = "detections") %>%
  group_by(habitat_type, species) %>%
  summarise(total_detections = sum(detections, na.rm = TRUE), .groups = "drop")

# Step 3: Reshape data back to wide format (one column per species)
most_detected_habitat_summary <- most_detected_habitat_summary %>%
  pivot_wider(names_from = species, values_from = total_detections)

# View the result
print(wide_data)
































  