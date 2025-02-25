####*********
### Lets make a plot with Muskox and compare detections between season
#load in packages
install.packages("writexl")
install.packages("ggplot2")
install.packages("ggspatial")
install.packages("sf")
install.packages("prettymapr")
install.packages("rnaturalearth")
install.packages("rnaturalearthdata")



library(writexl)
library(RColorBrewer)
library(terra)
library(raster)
library(ggspatial)
library(sf)
library(prettymapr)
library(grid)
library(tidyverse)
library(rnaturalearth)
library(rnaturalearthdata)
library(tidyterra)

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
  dplyr::select(5,6,7)

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
  dplyr::select(Muskox, season, week, year)

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
dplyr::select(-n, -SCANFI_att_nfiLandCover_SW_2020_v1.2) %>%  
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

# Plot with ggplot2 and basemap
ggplot() +
  geom_sf(data = esker_data %>% st_transform(st_crs(32612)), color = "blue", fill = NA) +  # Plot esker_data with transparent fill
  geom_sf(data = TDN_boundary %>% st_transform(st_crs(32612)), color = "black", linewidth = 1.5, fill = NA) +  # Plot TDN_boundary with transparent fill
  geom_sf(data = camera_buffer %>% st_transform(st_crs(32612)), color = "red", linewidth = 2, fill = NA) +
  coord_sf(datum = st_crs(32612))  # Plot camera_buffer with transparent fill
  #annotation_scale(location = "br", width_hint = 0.1)   # Add scale bar to the bottom-right corner
  #annotation_north_arrow(location = "tr", 
                         #width = unit(1, "in"), height = unit(1, "in") 

                          

# Compute distances from each camera trap to the nearest esker
camera_locations <- camera_locations %>%
  rowwise() %>%
  mutate(distance_to_esker = min(st_distance(geometry, esker_data$geometry))) %>%
  ungroup()

# Remove the 'm' and convert to numeric
comb_overlap_SCANFI_and_selected_mammals_week$distance_to_esker <- 
  as.numeric(gsub("m", "", comb_overlap_SCANFI_and_selected_mammals_week$distance_to_esker))

# Rename the column to include 'm'
colnames(comb_overlap_SCANFI_and_selected_mammals_week)[
  which(names(comb_overlap_SCANFI_and_selected_mammals_week) == "distance_to_esker")] <- "distance_to_esker_m"



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
camera_locations_df$elevation <- elevations

# Print the resulting table
print(camera_locations_df)

#combining distance to eskers column to comb_overlap_SCANFI_and_selected_mammals_week

# Merge the datasets based on 'camera_id'
comb_overlap_SCANFI_and_selected_mammals_week <- merge(comb_overlap_SCANFI_and_selected_mammals_week, 
                     camera_locations_df[, c("location", "elevation")], 
                     by = "location", 
                     all.x = TRUE)  # Keeps all rows from comb_overlap_SCANFI_and_selected_mammals_week

# Convert tibble to a regular data.frame (if necessary)
camera_locations_df <- as.data.frame(camera_locations_df)

# Now perform the left join
comb_overlap_SCANFI_and_selected_mammals_week <- comb_overlap_SCANFI_and_selected_mammals_week %>%
  dplyr::left_join(camera_locations_df %>% dplyr::select(location, distance_to_esker), 
                   by = "location")

st_write(camera_locations, "SpeciesRawData (Oct 31)/NWTBM_Thaidene_Nëné_Biodiversity_Project_2021_location_repo.shp")

# View the merged dataset
head(merged_data)

######### for some reason R was not adding distance_to_esker from my other code to the "comb_overlap_SCANFI_and_selected_mammals_week" dataset, so I redid it again here with code from chatgpt and it worked:

# Perform a left join to add the "distance to eskers" column from camera_locations to comb_overlap_SCANFI_and_selected_mammals_week
#comb_overlap_SCANFI_and_selected_mammals_week <- comb_overlap_SCANFI_and_selected_mammals_week %>%
  #left_join(camera_locations %>%
              #dplyr::select(location, distance_to_esker),  # select only relevant columns
            #by = "location") 

#playing around with the nwt ecoregions data. trying to make maps with it.

# Ensure both datasets are in the same CRS (if they are not already)
if (st_crs(TDN_boundary) != st_crs(nwt_ecoregions)) {
  TDN_boundary <- st_transform(TDN_boundary, st_crs(nwt_ecoregions))
}

# Crop TDN_Boundary to nwt_ecoregions using st_intersection()
cropped_TDN_Boundary <- st_intersection(TDN_boundary, nwt_ecoregions)

# Plot to check the result
plot(cropped_TDN_Boundary$geometry)

plot(nwt_boundary$geometry)

# Subset the data to get the 7th row
nwt_boundary_row7 <- nwt_boundary[7, ]

# Plot the 7th row using plot()
plot(nwt_boundary_row7$geometry)

# Or use ggplot2 to plot
library(ggplot2)
ggplot() +
  geom_sf(data = nwt_boundary_row7, color = "blue", fill = "lightblue")  # Adjust colors as needed

# Step 2: If the CRS do not match, transform one of them to the CRS of the other
# Assuming nwt_boundary_row7 is the reference CRS:
if (st_crs(TDN_boundary) != st_crs(nwt_boundary_row7))
  TDN_boundary <- st_transform(TDN_boundary, st_crs(nwt_boundary_row7))

# Step 3: Crop the TDN_Boundary using st_intersection
cropped_TDN_Boundary <- st_intersection(TDN_boundary, nwt_boundary_row7)

# Plot with ggplot, adding basemap using ggspatial
ggplot() +
  ggspatial::annotation_map_tile(zoom = 2) +  # Add OpenStreetMap basemap (adjust zoom for resolution)
  geom_sf(data = nwt_boundary_row7, color = "blue", fill = "lightblue", alpha = 0.5) +  # Plot row 7 of nwt_boundary
  geom_sf(data = cropped_TDN_Boundary, color = "red", fill = "lightcoral", alpha = 0.5) +  # Plot cropped TDN_Boundary
  theme_minimal() +
  labs(title = "Cropped TDN_Boundary within nwt_boundary Row 7")


##############################################################
#trying to see what habitats grizzlies and gray wolves are spending the most time in

# Step 1: Reshape the data to long format
most_detected_habitat <- comb_overlap_SCANFI_and_selected_mammals_week %>%
  dplyr::select(location, Shrub, Herbs, Water, `Treed mixed`, `Treed broadleaf`, `Treed conifer`, gray_wolf, grizzly_bear, Muskox, n_days_effort) %>%
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

most_detected_habitat <- as.data.frame(most_detected_habitat)

# Step 2: Summarize the detections by species and habitat type
most_detected_habitat_summary <- most_detected_habitat %>%
  dplyr::select(grizzly_bear, gray_wolf, Muskox, habitat_type, n_days_effort) %>%  # Explicitly use dplyr::select()
  group_by(habitat_type) %>%
  summarise(grizzly_abund = sum(grizzly_bear)/sum(n_days_effort)*1000,
            wolf_abund = sum(gray_wolf)/sum(n_days_effort)*1000,
            muskox_abund = sum(Muskox)/sum(n_days_effort)*1000,
            num_cameras = length(unique(location)))

write_xlsx(most_detected_habitat_summary, "most_detected_habitat_summary.xlsx")

#filtering out just muskox sightings
data <- species_all %>% mutate(year_month = floor_date(date, "month"))  %>%
  filter(species_common_name %in% c("Muskox", "Grizzly Bear", "Gray Wolf")) %>% #if I want to filter by a species
  mutate(week = week(date), #if I want to create a column with week number
         month = month(date),
         year = year(date),
         individual_count = as.numeric(individual_count))

#averaging muskox detections by week
total_data <- data %>% 
  group_by(date, month, location, species_common_name) %>%
  summarise(date_count = max(individual_count)) %>%
  group_by(location, species_common_name) %>%
  summarise(total_count = mean(date_count) * 7)

#creating data frames to plot from  
total_data_cameras <- merge(total_data, TDN_Cameras, by="location")
#month_data_cameras <- merge(month_data, TDN_Cameras, by="location")

#converting to sf objects
total_data_cameras_SF <-st_as_sf(total_data_cameras, coords=c("longitude", "latitude"), crs=4326, remove=FALSE)
#data_cameras_SF <- st_as_sf(data_cameras, coords=c("longitude", "latitude"), crs=4326, remove=FALSE)

ggplot() +
  geom_sf(data=TDN_boundary %>% st_transform(32612), aes(geometry = geometry), fill = NA, color = "black", size = 20) +
  tidyterra::geom_spatraster(data = SCANFI_landcover_cropped) +
  geom_sf(data=total_data_cameras_SF %>% st_transform(32612), aes(geometry = geometry, size=total_count)) +
  facet_wrap(~species_common_name, nrow = 1) +
  theme_minimal() +
  theme(
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank())

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
  pivot_longer(cols = c("grizzly_bear", "gray_wolf", "Muskox"), 
               names_to = "species", 
               values_to = "detections") %>%
  group_by(habitat_type, species) %>%
  summarise(total_detections = sum(detections, na.rm = TRUE), .groups = "drop")
  
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
    labs(x = "Landcover Type", y = "Grizzly Abundance", title = "Grizzly Abundance by Landcover Type") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels if needed
  
  # Create a bar plot for abundance by landcover type for muskox
  ggplot(most_detected_habitat_summary, aes(x = habitat_type, y = muskox_abund)) +
    geom_bar(stat = "identity", position = "dodge", width = 0.3, color = "cornflowerblue", fill = "cornflowerblue") + # Make bars smaller
    #scale_fill_manual(values = c("muskox_abund" = "skyblue")) +  # Custom colors
    labs(x = "Landcover Type", y = "Muskox Abundance", title = "Muskox Abundance by Landcover Type") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels if needed
  
  # Create a bar plot for abundance by landcover type for gray wolves
  ggplot(most_detected_habitat_summary, aes(x = habitat_type, y = wolf_abund)) +
    geom_bar(stat = "identity", position = "dodge", width = 0.3, color = "cornflowerblue", fill = "cornflowerblue") + # Make bars smaller
    #scale_fill_manual(values = c("muskox_abund" = "skyblue")) +  # Custom colors
    labs(x = "Landcover Type", y = "Wolf Abundance", title = "Wolf Abundance by Landcover Type") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels if needed
  
  
  
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
      plot.background = element_rect(fill = "transparent", color = NA)   # Transparent background for the plot area
    )






 
  





















  