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

#old fire code that I am not using but do not want to delete just yet
#combining fire ages
mod_3data <- model_variables_only %>%
  mutate(fire_age2_3 = fire_age2 + fire_age3, 
         fire_age0_1 = fire_age0 + fire_age1)

#fire model for food hypothesis 
mod_nb3 <- glmer.nb(Muskox ~ scale(fire_age2_3) + scale(fire_age0_1) +
                      offset(log(n_days_effort)) +
                      (1|cluster), #grouping by five cameras 
                    data = mod_3data)
# ss <- getME(mod_nb3,c("theta","fixef"))
# m2 <- update(mod_nb3,start=ss,control=glmerControl(optCtrl=list(maxfun=2e4)))


# ss <- getME(mod_nb3.1,c("theta","fixef"))
# m2 <- update(mod_nb3.1,start=ss,control=glmerControl(optCtrl=list(maxfun=2e4)))


#combining fire ages
# mod_4data <- model_variables_only %>%
#   mutate(fire_age2_3 = fire_age2 + fire_age3, 
#          fire_age0_1 = fire_age0 + fire_age1)


# fmList<-model.sel(mod_nb1=mod_nb1, mod_nb2=mod_nb2, mod_nb3=mod_nb3, mod_nb4=mod_nb4)
# fmList

  
# # Now perform the left join
# comb_overlap_SCANFI_and_selected_mammals_week <- comb_overlap_SCANFI_and_selected_mammals_week %>%
#   dplyr::left_join(camera_locations_df %>% dplyr::select(location, distance_to_esker), 
#                    by = "location")

# # Remove the 'm' and convert to numeric
# comb_overlap_SCANFI_and_selected_mammals_week$distance_to_esker <- 
#   as.numeric(gsub("m", "", comb_overlap_SCANFI_and_selected_mammals_week$distance_to_esker))
# 
# # Rename the column to include 'm'
# colnames(comb_overlap_SCANFI_and_selected_mammals_week)[
#   which(names(comb_overlap_SCANFI_and_selected_mammals_week) == "distance_to_esker")] <- "distance_to_esker_m"

#moving the column "cluster" to the dataset combined_variables_fire

#ecoregions 
locs_ecoregions <- camera_locations %>%
  st_transform(crs = st_crs(cropped_ecoregions_TDN_Boundary)) #change the projection to match the raster

# Find overlaps
ecoregions_overlap <- st_join(locs_ecoregions, cropped_ecoregions_TDN_Boundary)
ecoregions_overlap <- as.data.frame(ecoregions_overlap)

# Make a new column in cameras 
model_variables <- model_variables %>% 
  left_join(ecoregions_overlap %>% select(location, ECO1_NAM_1, ECO2_NAM_1, ECO3_NAM_1, ECO4_NAM_1), by="location")

variables_only <- all_variables %>%
  ungroup() %>%  # Ungroup the dataset to prevent warnings
  select(`Treed_broadleaf`, `Treed_conifer`, `Treed_mixed`, Bryoid, Shrub, Water, Herbs,
         gray_wolf, grizzly_bear, elevations, esker_camera_distances,
         fire_age0, fire_age1, fire_age2, fire_age3, fire_age4, log_esker_camera_distances,
         grizzly_per_day, gray_wolf_per_day) %>%
  select(-matches("^location$"))

#all variables with seasons

all_variables_season <- all_variables %>%
  group_by(location, season, year, cluster) %>%
  summarize(
    Muskox = sum(Muskox),
    grizzly_bear = sum(grizzly_bear),
    gray_wolf = sum(gray_wolf),
    n_days_effort = sum(n_days_effort),
    `Treed broadleaf` = mean(`Treed broadleaf`),
    `Treed conifer` = mean(`Treed conifer`),
    `Treed mixed` = mean(`Treed mixed`),
    Bryoid = mean(Bryoid),
    Shrub = mean(Shrub),
    Water = mean(Water),
    Herbs = mean(Herbs),
    fire_age0 = mean(fire_age0),
    fire_age1 = mean(fire_age1),
    fire_age2 = mean(fire_age2),
    fire_age3 = mean(fire_age3),
    fire_age4 = mean(fire_age4),
    elevations = mean(elevations),
    esker_camera_distances = mean(esker_camera_distances),
    log_esker_camera_distances = mean(log_esker_camera_distances)
  )

# Assuming both datasets have a common key for joining (e.g., an ID column like "id")
combined_variables_and_fire <- combined_variables_and_fire %>%
  left_join(dplyr::select(comb_overlap_SCANFI_and_selected_mammals_week_df, location, cluster), by = "location") 
  

#adding seasons to selected_mammals_week data set

# # Adding a new column for seasons
# selected_mammals_week <- selected_mammals_week %>%
#   mutate(season = case_when(
#     week %in% 1:10 ~ "Winter",   # Weeks 1 to 10
#     week %in% 11:24 ~ "Spring",   # Weeks 11 to 24
#     week %in% 25:37 ~ "Summer",   # Weeks 25 to 37
#     week %in% 38:51 ~ "Fall",     # Weeks 36 to 45
#     week %in% 52:52 ~ "Winter",   # weeks 52 to 52
#     TRUE ~ "Unknown"))


#adding the land cover names column to overlap_SCANFI_cameras_table
# 
# overlap_SCANFI_cameras_table <- overlap_SCANFI_cameras_table %>%
#   mutate(land_cover_name = case_when(
#     SCANFI_att_nfiLandCover_SW_2020_v1.2 == 1 ~ "Bryoid",
#     SCANFI_att_nfiLandCover_SW_2020_v1.2 == 2 ~ "Herbs",
#     SCANFI_att_nfiLandCover_SW_2020_v1.2 == 3 ~ "Rock",
#     SCANFI_att_nfiLandCover_SW_2020_v1.2 == 4 ~ "Shrub",
#     SCANFI_att_nfiLandCover_SW_2020_v1.2 == 5 ~ "Treed broadleaf",
#     SCANFI_att_nfiLandCover_SW_2020_v1.2 == 6 ~ "Treed conifer",
#     SCANFI_att_nfiLandCover_SW_2020_v1.2 == 7 ~ "Treed mixed",
#     SCANFI_att_nfiLandCover_SW_2020_v1.2 == 8 ~ "Water"
#   ))


#making an un-summarized data set with a different row for each camera for each week for each year
#added two columns for number of grizzlies and gray wolves detected per day in a week
all_variables <- all_variables %>%
  mutate(grizzly_per_day = grizzly_bear/n_days_effort, 
         gray_wolf_per_day = gray_wolf/n_days_effort)

#made new dataset that included tri and grizz_per_day and gray_wolf_per_day
all_variables_with_tri <- all_variables %>%
  left_join(camera_locations_df %>% select(location, TRI_extracted), by = "location")

all_variables_with_tri_and_species <- all_variables_with_tri %>%
  mutate(grizzly_per_day = grizzly_bear/n_days_effort, 
         gray_wolf_per_day = gray_wolf/n_days_effort)

all_variables <- all_variables %>%
  rename_with(~ gsub(" ", "_", .))




#erics code for TRI
#copernicus dem data (from eric)
TRI_results <- spatialEco::tri(TDN_DEM, s = 3, exact = FALSE)
TDN_tri <- st_transform(TDN_boundary, crs = st_crs(TRI_results))

# Assign a CRS (replace with the correct CRS)
# Assign a CRS using EPSG code
crs(TDN_DEM) <- CRS("+init=epsg:32612")  # Example: EPSG 32633 for UTM Zone 33N

TDN_boundary_projected <- st_transform(TDN_boundary, crs = st_crs(TDN_DEM)) %>%
  st_buffer(1000)

dem_cropped<-crop(TDN_DEM,TDN_boundary_projected)
plot(dem_cropped, axes = FALSE)
plot(st_geometry(TDN_dem), add = TRUE)
plot(st_geometry(camera_locations),add = TRUE)



#using arcticDEM data and cropping it to TDN boundary, similar to above
# TDN_boundary_projected <- st_transform(TDN_boundary, crs = st_crs(TDN_DEM)) %>%
#   st_buffer(1000)

# #making new dataset with just variables I will be testing
# model_variables_only <- model_variables %>%
#   ungroup() %>%  # Ungroup the dataset to prevent warnings
#   select(`Treed broadleaf`, `Treed conifer`, `Treed mixed`, Bryoid, Shrub, Water, Herbs,
#          gray_wolf, grizzly_bear, Muskox, Elevation, season, esker_camera_distances,
#          fire_age0, fire_age1, fire_age2, fire_age3, fire_age4, n_days_effort, cluster, log_esker_camera_distances,
#          grizzly_per_day, gray_wolf_per_day, TRI_extracted, ECO2_NAM_1) %>%
#   select(-matches("^location$"))


# crs(ArcticDEM_cropped) <- CRS("+init=epsg:32612")  # Example: EPSG 32633 for UTM Zone 33N
# camera_locations_projected <- vect(camera_locations_projected)  # converts sf to SpatVector
# # Extract TRI values
# TRI_extracted_arcticDEM <- terra::extract(TRI_Results_ArcticDEM, camera_locations_projected)

# TRI_extracted_arcticDEM <- raster::extract(TRI_Results_ArcticDEM, camera_locations)


# Combine the camera locations data with the extracted elevation values
# Convert camera_locations to a data frame if it's an sf object
# camera_locations_df <- st_as_sf(camera_locations) %>%
#   st_drop_geometry()  # Remove geometry to get a regular data frame
# 
# # Combine the extracted elevations with the camera location data
# camera_locations_df$elevations <- elevations$ArcticDEM_cropped
# camera_locations_df$esker_camera_distances <- esker_camera_distances$layer
# camera_locations_df$TRI_extracted <- TRI_extracted$lyr.1

# all_variables <- all_variables %>%
#   rename_with(~ gsub(" ", "_", .))

#arcticdem
# # Remove specific columns and rename 'elevations.y' to 'elevation'
# adding_variables_elevations_eskers <- adding_variables_elevations_eskers %>%
#   dplyr::select(-c(elevation.x, elevation.y, elevations.x)) %>%
#   rename(elevation = elevations.y)


# all_variables <- all_variables %>%
#   mutate(grizzly_per_day = grizzly_bear/n_days_effort, 
#          gray_wolf_per_day = gray_wolf/n_days_effort)

# Merge the datasets based on 'camera_id'
adding_variables_elevations_eskers <- merge(comb_overlap_SCANFI_and_selected_mammals_week, 
                                            camera_locations_df[, c("location", "elevations", "esker_camera_distances")], 
                                            by = "location", 
                                            all.x = TRUE)  # Keeps all rows from comb_overlap_SCANFI_and_selected_mammals_week

# crs(ArcticDEM_cropped) <- "EPSG:32612"
# Example: EPSG 32633 for UTM Zone 33N
#trying to make table with elevation for each camera location (COPERNICUS TDN_DEM)

#making a data set summarized across clusters, weeks and years
TDN_camera_clusters <- comb_overlap_SCANFI_and_selected_mammals_week %>%
  group_by(cluster, week, year) %>%
  summarise(grizzly_count_sum = sum(grizzly_bear),
            muskox_count_sum = sum(Muskox),
            gray_wolf_sum = sum(gray_wolf),
            n_days_effort_sum = sum(n_days_effort),
            Treed_conifer_average = mean(`Treed conifer`),
            Bryoid_average = mean(Bryoid)) %>%
  mutate(grizzly_per_day = grizzly_count_sum/n_days_effort_sum, 
         gray_wolf_per_day = gray_wolf_sum/n_days_effort_sum)


#making a model set 
#using a negative binomal mixed effects model with camera cluster and camera ID as fixed effects

#making null model
mod_nb1 <- glmmTMB(Muskox ~ 1+
                     offset(log(n_days_effort)) +
                     (1|cluster/location),
                   data = model_variables)

#predation model 
#summer model for predation
mod_pre_sum <- glmmTMB(Muskox ~ grizzly_bear + gray_wolf + 
                         offset(log(n_days_effort)) +
                         (1|cluster/location),
                       family="nbinom2",
                       data = model_variables %>% filter(season == "Summer"))


#winter model for predation 
mod_pre_win <- glmmTMB(Muskox ~ grizzly_bear + gray_wolf + 
                         offset(log(n_days_effort)) +
                         (1|cluster/location),
                       family="nbinom2",
                       data = model_variables %>% filter(season == "Winter"))


#food model
#summer model for landcover (food hypothesis)
mod_food_sum <- glmmTMB(Muskox ~ scale(Shrub) + scale(`Treed broadleaf`) + scale(Bryoid) + scale(Herbs) + scale(fire_age1) + scale(fire_age2) + scale(fire_age3) + scale(fire_age0) +
                          offset(log(n_days_effort)) +
                          (1|cluster/location),
                        family="nbinom2",
                        data = model_variables %>% filter(season == "Summer")) 


#winter model for landcover (food hypothesis)
mod_food_win <- glmmTMB(Muskox ~ scale(Shrub) + scale(`Treed broadleaf`) + scale(Herbs) + scale(Bryoid) + scale(fire_age1) + scale(fire_age2) + scale(fire_age3) + scale(fire_age0) +
                          offset(log(n_days_effort)) +
                          (1|cluster/location),
                        family="nbinom2",
                        data = model_variables %>% filter(season == "Winter")) 


#thermoregulation model
#summer model for thermoregulation 
mod_therm_summer <- glmmTMB(Muskox ~ `Treed mixed`+`Treed broadleaf`+`Treed conifer`+ Water + log_esker_camera_distances + fire_age1 + fire_age2 + fire_age3 + fire_age0 + TRI_extracted +
                              offset(log(n_days_effort)) +
                              (1|cluster/location),
                            family="nbinom2",
                            data = model_variables  %>% filter(season == "Summer"))

#winter model for thermoregulation 
mod_therm_win <- glmmTMB(Muskox ~ `Treed mixed`+`Treed broadleaf`+`Treed conifer`+ Water + log_esker_camera_distances + fire_age1 + fire_age2 + fire_age3 + fire_age0 + TRI_extracted +
                           offset(log(n_days_effort)) +
                           (1|cluster/location),
                         family="nbinom2",
                         data = model_variables %>% filter(season == "Winter"))

#model with all variables
model_all <- glmmTMB(Muskox ~ `Treed mixed` + log_esker_camera_distances + fire_age1 + fire_age2 + fire_age3 + fire_age0 + TRI_extracted + Shrub + Bryoid + Herbs + `Treed broadleaf` + `Treed conifer` + gray_wolf + grizzly_bear + season + Water +   
                       offset(log(n_days_effort)) +
                       (1|cluster/location),
                     family="nbinom2",
                     data = model_variables)














#trying more simple model because some of the previous ones did not run
#making null model
mod_1 <- glmer(Muskox ~ 1+
                 offset(log(n_days_effort)) +
                 (1|cluster/location),
               data = all_variables)

#predation model
mod_2 <- glmer(Muskox ~ grizzly_per_day + gray_wolf_per_day + Treed_conifer + Treed_broadleaf + treed_mixed + elevations + log_esker_camera_distances + TRI_extracted +
                 offset(log(n_days_effort)) +
                 (1|cluster/location),
               data = all_variables_with_tri)

#food model
mod_3 <- glmer(Muskox ~ Bryoid + Shrub + Herbs + Treed_broadleaf + fire_age0 + fire_age1 + fire_age2 + fire_age3 + fire_age4 +
                 offset(log(n_days_effort)) +
                 (1|cluster/location),
               data = all_variables)

#thermoregulation model
mod_4 <- glmer(Muskox ~ Treed_conifer + Treed_broadleaf + Treed_mixed + elevations + log_esker_camera_distances + fire_age0 + fire_age1 + fire_age2 + fire_age3 + fire_age4 +
                 offset(log(n_days_effort)) +
                 (1|cluster/location),
               data = all_variables)

#comparing model fit of all models using AIC 
fmList<-model.sel(mod_1=mod_1, mod_2=mod_2, mod_3=mod_3, mod_4=mod_4)
fmList

#in depth summary of best fitting model which is mod_4
summary(mod_4)

correlation_matrix <- cor(all_variables, use = "complete.obs")  # Use complete.obs to handle NAs
corrplot(correlation_matrix, method="circle")

###### testing out models, trying out different structures 
mod <- glm(muskox_count_sum ~ grizzly_per_day +
             gray_wolf_per_day + 
             Treed_conifer_average +
             Bryoid_average + 
             offset(log(n_days_effort_sum)),
           data = TDN_camera_clusters,
           family = "poisson")

summary(mod)


mod2 <- glmer(muskox_count_sum ~ grizzly_per_day +
                gray_wolf_per_day + 
                Treed_conifer_average +
                Bryoid_average + 
                offset(log(n_days_effort_sum)) +
                (1|cluster),
              data = TDN_camera_clusters,
              family = "poisson")

summary(mod)

#using raw data. trying a mixed effect model
mod3 <- glmer(Muskox ~ grizzly_per_day +
                gray_wolf_per_day + 
                `Treed conifer` +
                Bryoid + #random effects
                offset(log(n_days_effort)) + 
                (1|cluster/location), #fixed effect 
              data = TDN_camera_clusters,
              family = "poisson") #type of mixed effect 


fit <- goodfit(TDN_camera_clusters$Muskox)
summary(fit)
rootogram(fit)
Ord_plot(TDN_camera_clusters$Muskox)
distplot(TDN_camera_clusters$Muskox, type = "poisson")
distplot(TDN_camera_clusters$Muskox, type = "nbinom")

anova(mod, test = "Chisq")

library(AER)
deviance(mod)/mod$df.residual
dispersiontest(mod) #this is telling us there is over dispersion, cant use poisson model

library(pscl)
model1 <- zeroinfl(muskox_count_sum ~ grizzly_per_day +
                     gray_wolf_per_day + 
                     Treed_conifer_average +
                     Bryoid_average + 
                     offset(log(n_days_effort_sum)),
                   data = TDN_camera_clusters,
                   dist = "poisson")
AIC(mod, model1)

model2 <- MASS::glm.nb(muskox_count_sum ~ grizzly_per_day +
                         gray_wolf_per_day + 
                         Treed_conifer_average +
                         Bryoid_average + 
                         offset(log(n_days_effort_sum)),
                       data = TDN_camera_clusters
)

model3 <- zeroinfl(muskox_count_sum ~ grizzly_per_day +
                     gray_wolf_per_day + 
                     Treed_conifer_average +
                     Bryoid_average + 
                     offset(log(n_days_effort_sum)),
                   data = TDN_camera_clusters,
                   dist = "negbin")

model4 <- zeroinfl(muskox_count_sum ~ 1+
                     offset(log(n_days_effort_sum)),
                   data = TDN_camera_clusters,
                   dist = "negbin")


AIC(mod, model1, model2, model3)

mod_nb <- glmer.nb(Muskox ~ grizzly_per_day +
                     gray_wolf_per_day + 
                     `Treed conifer` +
                     Bryoid + 
                     offset(log(n_days_effort)) +
                     (1|cluster/location),
                   data = TDN_camera_clus)

summary(mod_nb)                  

mod_nb <- glmer.nb(Muskox ~ grizzly_per_day +
                     gray_wolf_per_day + 
                     `Treed conifer` +
                     Bryoid + 
                     offset(log(n_days_effort)) +
                     (1|cluster/location),
                   data = TDN_camera_clus)





glmmTMB::fixef()




