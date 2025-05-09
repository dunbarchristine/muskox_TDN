#adding season column

model_variables <- model_variables %>%
  mutate(season = case_when(
    as.numeric(.data$week) %in% 1:20 ~ "Winter",   #Weeks 1 to 19
    as.numeric(.data$week) %in% 21:46 ~ "Summer",  #Weeks 20 to 40              
    as.numeric(.data$week) %in% 47:52 ~ "Winter",  #Weeks 47 to 52
    TRUE ~ "Unknown"
  ))

