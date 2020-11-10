
library(gridExtra)

#Start by writing functions for each type of flag.
#Each function should create a new column (1/0) for the flag.

#Each function will need a data input and fields named 
# 1) Temperature - numeric
# 2) sampleDate - date
# 3) sampleTime - hms

#All flags find errors in the time series by date and apply
# a flag (1) for all measurements on that date.


flag_high_temps <- function(data, high_temp_value = 25) {
  dates_to_flag <- data %>% 
    filter(Temperature > high_temp_value) %>% 
    distinct(sampleDate) %>% 
    pull(sampleDate)
  data %>% 
    mutate(flag_highTemps = ifelse(sampleDate %in% dates_to_flag, 1, 0))
}

flag_low_temps <- function(data, low_temp_value = -1) {
  dates_to_flag <- data %>% 
    filter(Temperature < low_temp_value) %>% 
    distinct(sampleDate) %>% 
    pull(sampleDate)
  data %>% 
    mutate(flag_lowTemps = ifelse(sampleDate %in% dates_to_flag, 1, 0))
}

flag_hourly_change <- function(data, hourly_change_value = 3) {
  dates_to_flag <- data %>% 
    group_by(sampleDate, as.POSIXct(round(dateTime, units = "hours"))) %>% 
    summarize(hourlyMeanTemp = mean(Temperature, na.rm = TRUE)) %>% 
    mutate(hourlyDiff = c(0, abs(diff(hourlyMeanTemp)))) %>% 
    filter(hourlyDiff > hourly_change_value) %>% 
    distinct(sampleDate) %>% 
    pull(sampleDate)
  data %>% 
    mutate(flag_hourlyChange = ifelse(sampleDate %in% dates_to_flag, 1, 0))
}

flag_daily_change <- function(data, daily_change_value = 3) {
  dates_to_flag <- data %>% 
    group_by(sampleDate) %>% 
    summarize(dailyMeanTemp = mean(Temperature, na.rm = TRUE)) %>% 
    mutate(dailyDiff = c(0, abs(diff(dailyMeanTemp)))) %>% 
    filter(dailyDiff > daily_change_value) %>% 
    distinct(sampleDate) %>% 
    pull(sampleDate)
  data %>% 
    mutate(flag_dailyChange = ifelse(sampleDate %in% dates_to_flag, 1, 0))
}

add_temp_flags <- function(data) {
  data %>% 
    flag_high_temps() %>% 
    flag_low_temps() %>% 
    flag_hourly_change() %>% 
    flag_daily_change()
} 

plot_flags <- function(data, subtitle) {
  p1 <- data %>%
    ggplot(aes(x = dateTime, y = Temperature)) +
    geom_line(aes(color = flag_highTemps)) + 
    theme(legend.position = "none") +
    labs(title = "Temperature measurements > 25 C",
         subtitle = subtitle)
  
  p2 <- data %>% 
    ggplot(aes(x = dateTime, y = Temperature)) +
    geom_line(aes(color = flag_lowTemps)) + 
    theme(legend.position = "none") +
    labs(title = "Temperature measurements < -1 C")
  
  p3 <- data %>% 
    ggplot(aes(x = dateTime, y = Temperature)) +
    geom_line(aes(color = flag_hourlyChange)) + 
    theme(legend.position = "none") +
    labs(title = "Rate of change in hourly mean temps. > 3 C")
  
  p4 <- data %>% 
    ggplot(aes(x = dateTime, y = Temperature)) +
    geom_line(aes(color = flag_dailyChange)) + 
    theme(legend.position = "none") +
    labs(title = "Rate of change in daily mean temps. > 3 C")
  
  grid.arrange(p1, p2, p3, p4, nrow = 2)
  
}



# APUdat %>% 
#   filter(Site == "APU9") %>% 
#   add_temp_flags() %>% 
#   plot_flags()
# 
# APUdat %>% 
#   filter(Site == "APU9") %>% 
#   add_temp_flags() %>% 
#   filter(sampleDate > "2015-08-01",
#          sampleDate < "2015-09-15") %>% 
#   ggplot(aes(x = dateTime, y = Temperature)) +
#   geom_line(aes(color = flag_hourlyChange)) + 
#   theme(legend.position = "bottom") 

#data arguments don't take a default, but detail args do.
#... ellipsis allows passing argument to other functions.
#checks on user inputs should be at start of function body
#assertive package for errors on argument inputs

#library(zeallot)
# multi-assignment operator %<-%
#c(ab, bc, de) %<-% session()