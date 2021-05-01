# IMPORTING PACKAGES AND LIBRARIES

# install.packages('ggplot2')
library(ggplot2)
# install.packages('ggthemes')
library(ggthemes)
# install.packages('lubridate')
library(lubridate)
# install.packages('dplyr')
library(dplyr)
# install.packages('tidyr')
library(tidyr)
# install.packages('DT')
library(DT)
# install.packages('scale')
library(scales)

#colors = c(""#CC1011", "#665555", "#05a399", "#cfcaca", "#f5e840", "#0683c9", "#e075b0"")

# IMPORTING DATA

apr_data <- read.csv("uber-raw-data-apr14.csv")
may_data <- read.csv("uber-raw-data-may14.csv")
jun_data <- read.csv("uber-raw-data-jun14.csv")
jul_data <- read.csv("uber-raw-data-jul14.csv")
aug_data <- read.csv("uber-raw-data-aug14.csv")
sep_data <- read.csv("uber-raw-data-sep14.csv")

# Row Binding
data_2014 <- rbind(apr_data,may_data, jun_data, jul_data, aug_data, sep_data)

# Data Time conversion function
data_2014$Date.Time <- as.POSIXct(data_2014$Date.Time, format = "%d/%m/%Y %H:%M:%S")

# Formatting Time in Hours-Min-Sec
data_2014$Time <- format(as.POSIXct(data_2014$Date.Time, format = "%d/%m/%Y %H:%M:%S"), format="%H:%M:%S")

# Formatting Data in Year-Month-Day
data_2014$Date.Time <- ymd_hms(data_2014$Date.Time)

# Specifying Day, Month, Year, Day_of_Week, Hour, Minute and Seconds.
data_2014$day <- factor(day(data_2014$Date.Time))
data_2014$month <- factor(month(data_2014$Date.Time, label = TRUE))
data_2014$year <- factor(year(data_2014$Date.Time))
data_2014$dayofweek <- factor(wday(data_2014$Date.Time, label = TRUE))
data_2014$hour <- factor(hour(hms(data_2014$Time)))
data_2014$minute <- factor(minute(hms(data_2014$Time)))
data_2014$second <- factor(second(hms(data_2014$Time)))

# PLOTTING OF NUMBER OF CARS TRIPED IN PARTICULAR HOURS.

# Table
hour_data <- data_2014 %>%
  group_by(hour) %>%
  dplyr::summarize(Total = n()) 
datatable(hour_data)

# Plotting
ggplot(hour_data, aes(hour, Total)) + 
  geom_bar( stat = "identity", fill = "steelblue", color = "red") +
  ggtitle("Trips Every Hour") +
  theme(legend.position = "none") +
  xlab('Hours') +
  scale_y_continuous(labels = comma)

# PLOTTING OF NUMBER OF CARS TRIPED IN PARTICULAR MONTHS.

# Table
month_hour <- data_2014 %>%
  group_by(month, hour) %>%
  dplyr::summarize(Total = n())
datatable(month_hour)

# Plotting
ggplot(month_hour, aes(hour, Total, fill = month)) + 
  geom_bar( stat = "identity") +
  ggtitle("Trips by Hour and Month") +
  xlab('Hours') +
  scale_y_continuous(labels = comma)

# PLOTTING OF NUMBER OF CARS TRIPED IN PARTICULAR DAY.   

# Table
day_group <- data_2014 %>%
  group_by(day) %>%
  dplyr::summarize(Total = n()) 
datatable(day_group)

# Plotting
ggplot(day_group, aes(day, Total)) + 
  geom_bar( stat = "identity", fill = "steelblue") +
  ggtitle("Trips Every Day") +
  theme(legend.position = "none") +
  xlab('Days') + 
  scale_y_continuous(labels = comma)

# PLOTTING THE HEAT MAP.   

# Table of Number of Car Triped in Particular Days and Hours.
day_and_hour <- data_2014 %>%
  group_by(day, hour) %>%
  dplyr::summarize(Total = n())
datatable(day_and_hour)

# Plotting the Number of Car Triped in Particular Days and Hours.
ggplot(day_and_hour, aes(day, hour, fill = Total)) +
  geom_tile(color = "white") +
  xlab('Days') + 
  ylab('Hours') +
  ggtitle("Heat Map by Days and Hours")

# Plotting the Number of Car Triped in Particular Days and Months.
ggplot(day_month_group, aes(day, month, fill = Total)) +
  geom_tile(color = "white") +
  xlab('Days') + 
  ylab('Months') +
  ggtitle("Heat Map by Days and Months")

# Plotting the Number of Car Triped in Particular Days of Week and Months.
ggplot(month_weekday, aes(dayofweek, month, fill = Total)) +
  geom_tile(color = "white") +
  xlab('Days of Week') + 
  ylab('Months') +
  ggtitle("Heat Map by Month and Day of Week")

# MAP VISUALIZATION OF RIDES IN NEW YORK.

min_lat <- 40.5774
max_lat <- 40.9176
min_long <- -74.15
max_long <- -73.7004

ggplot(data_2014, aes(x=Lon, y=Lat)) +
  geom_point(size=1, color = "blue") +
  scale_x_continuous(limits=c(min_long, max_long)) +
  scale_y_continuous(limits=c(min_lat, max_lat)) +
  theme_map() +
  ggtitle("NYC MAP BASED ON UBER RIDES DURING 2014 (APR-SEP)")
