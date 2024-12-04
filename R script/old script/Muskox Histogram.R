#load in packages

list.of.packages <- c("kableExtra", "tidyr", "ggplot2", "gridExtra", "dplyr", "Hmsc", "jtools", "lubridate", "corrplot", 
                      "MuMIn","stringr","sf","raster","leaflet", "tidyverse","htmlwidgets","webshot", "purrr", "magick",
                      "forcats","multcomp", "reshape2", "lme4", "tidyr", "stats")


library(dplyr)
install.packages("ggplot2")
library(ggplot2)
  
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
  weekly_season_data_clean2 <- summarized_week_data2 %>%
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

#first make the dataset with all 3 species
summarised_week_data_3_species <- summarised_week %>%
  group_by(year, week) %>%
  summarise(
    Total_Count_muskox = sum(Muskox, na.rm = TRUE),
    Total_Count_moose = sum(Moose, na.rm = TRUE),
    Total_Count_caribou = sum(Barren_ground_Caribou, na.rm = TRUE),
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

# Remove the 'wolf date' column
species_all <- species_all %>%
  select(-`wolf_date`)





















  