getwd()
setwd("C:/Users/Public/Documents/Divvy_data/2022-2023divvy-tripdata")



#Install required packages
install.packages("tidyverse")
install.packages("lubridate")
install.packages("ggplot2")


#Load installed packages
library(tidyverse)  #helps wrangle data
library(lubridate)  #helps wrangle date attributes
library(ggplot2)  #helps visualize data

# Upload Divvy datasets (csv files) here
m3_2022 <- read_csv("202203-divvy-tripdata.csv")
m4_2022 <- read_csv("202204-divvy-tripdata.csv")
m5_2022 <- read_csv("202205-divvy-tripdata.csv")
m6_2022 <- read_csv("202206-divvy-tripdata.csv")
m7_2022 <- read_csv("202207-divvy-tripdata.csv")
m8_2022 <- read_csv("202208-divvy-tripdata.csv")
m9_2022 <- read_csv("202209-divvy-publictripdata.csv")
m10_2022 <- read_csv("202210-divvy-tripdata.csv")
m11_2022 <- read_csv("202211-divvy-tripdata.csv")
m12_2022 <- read_csv("202212-divvy-tripdata.csv")
m1_2023 <- read_csv("202301-divvy-tripdata.csv")
m2_2023 <- read_csv("202302-divvy-tripdata.csv")

#Check for consistency in the column names
colnames(m3_2022)
colnames(m4_2022)
colnames(m5_2022)
colnames(m6_2022)
colnames(m7_2022)
colnames(m8_2022)
colnames(m9_2022)
colnames(m10_2022)
colnames(m11_2022)
colnames(m12_2022)
colnames(m1_2023)
colnames(m2_2023)

#Checking the data types of columns
str(m3_2022)
str(m4_2022)
str(m5_2022)
str(m6_2022)
str(m7_2022)
str(m8_2022)
str(m9_2022)
str(m10_2022)
str(m11_2022)
str(m12_2022)
str(m1_2023)
str(m2_2023)

#Concatenate data frames into one 
bike_trips <- bind_rows(m3_2022,m4_2022,m5_2022,m6_2022,m7_2022,m8_2022,m9_2022,
                        m10_2022,m11_2022,m12_2022,m1_2023,m2_2023 )

#Deleting previous data frames to free up space 
rm(m3_2022)
rm(m4_2022)
rm(m5_2022)
rm(m6_2022)
rm(m7_2022)
rm(m8_2022)
rm(m9_2022)
rm(m10_2022)
rm(m11_2022)
rm(m12_2022)
rm(m1_2023)
rm(m2_2023)

#Details about the merged data frame

colnames(bike_trips) #List of column names
nrow(bike_trips) #How many rows are in data frame?
dim(bike_trips) #Dimensions of the data frame?
head(bike_trips$rideable_type) #See the first 6 rows of data frame/column
str(bike_trips)  #See list of columns and data types (numeric, character, etc)
summary(bike_trips)  #Statistical summary of data.
colSums(is.na(bike_trips)) #Checking for missing data

#subset of data that don't have start station names
bike_na_start <-  
  bike_trips[is.na(bike_trips$start_station_name), ]

#subset of data that don't have end station names
bike_na_end <-  
  bike_trips[is.na(bike_trips$end_station_name),] 

table(bike_trips$member_casual) #Check sum of unique values
table(bike_trips$rideable_type) #Check sum of unique values
nrow(table(bike_trips$start_station_id)) #Check total no of unique values
nrow(table(bike_trips$end_station_id)) #Check total no of unique values 

#Create some date related columns to have options to group the data by  
bike_trips$date <- as.Date(bike_trips$started_at) #yyyy-mm-dd
bike_trips$month <- format(as.Date(bike_trips$date), "%m")
bike_trips$day <- format(as.Date(bike_trips$date), "%d")
bike_trips$year <- format(as.Date(bike_trips$date), "%Y")
bike_trips$day_of_week <- format(as.Date(bike_trips$date), "%A")

#Creating a column for ride length in seconds
bike_trips$ride_length <- difftime(bike_trips$ended_at,bike_trips$started_at)
#head(bike_trips$ride_length) 
str(bike_trips)

#Convert ride length to numeric to enable calculations
#is.factor(bike_trips$ride_length)
bike_trips$ride_length <- as.numeric(as.character(bike_trips$ride_length))
is.numeric(bike_trips$ride_length)

#Removing negative ride lenghts assumed to be invalid  
bike_trips_c <- bike_trips[!(bike_trips$ride_length<0),]

#Dropping columns that will not be used for the analysis
# Remove  Columns in List
bike_trips_clean <- bike_trips_c[,!names(bike_trips_c) %in%
                                   c("start_station_id", "end_station_id",
                                     "start_lat","start_lng","end_lat",
                                     "end_lng")]

str(bike_trips_clean)

#Saving cleaned file to a csv 
write.csv(bike_trips_clean, file = 'bike_trips_clean.csv')

#Loading from csv
clean_trips <- read_csv('bike_trips_clean.csv')

str(clean_trips)
head(clean_trips)
#Dropping the index column 
clean_trips <- clean_trips[,-1]

#Descriptive summary of the ride lengths in secs
summary(clean_trips$ride_length)

# Compare members and casual users by the ride lenghts statistics
aggregate(clean_trips$ride_length ~ clean_trips$member_casual, FUN = mean)
aggregate(clean_trips$ride_length ~ clean_trips$member_casual, FUN = median)
aggregate(clean_trips$ride_length ~ clean_trips$member_casual, FUN = max)
aggregate(clean_trips$ride_length ~ clean_trips$member_casual, FUN = min)

# Ordering the days of the week 
clean_trips$day_of_week <- ordered(clean_trips$day_of_week, levels=c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))


# See the average ride time by each day for members vs casual users
aggregate(clean_trips$ride_length ~ clean_trips$member_casual + clean_trips$day_of_week, FUN = mean)

# See the total no of rides by each day for members vs casual users
aggregate(clean_trips$ride_id ~ clean_trips$member_casual + clean_trips$day_of_week, FUN = length)

# See the average ride time by bike type for members vs casual users
aggregate(clean_trips$ride_length ~ clean_trips$member_casual + clean_trips$rideable_type, FUN = mean)

# See the total no of rides by bike type for members vs casual users
aggregate(clean_trips$ride_id ~ clean_trips$member_casual + clean_trips$rideable_type, FUN = length)


# see the total no of rides and the mean duration by member type and day
clean_trips %>% 
  group_by(member_casual, day_of_week) %>%  #groups by usertype and day
  summarise(number_of_rides = n()							#calculates the number of rides and average duration 
            ,average_duration = mean(ride_length)) %>% 		# calculates the average duration
  arrange(member_casual, day_of_week)								# sorts

#Visualizing the number of rides by rider type and day of the week
clean_trips %>% 
  group_by(member_casual,day_of_week ) %>% 
  summarise(number_of_rides = n()
            ,average_duration = mean(ride_length)) %>% 
  arrange(member_casual, day_of_week)  %>% 
  ggplot(aes(x = day_of_week, y = number_of_rides, fill = member_casual)) +
  geom_col(position = "dodge") 

#Visualizing the length of rides by rider type and day of the week
clean_trips %>% 
  group_by(member_casual,day_of_week ) %>% 
  summarise(number_of_rides = n()
            ,average_duration = mean(ride_length)) %>% 
  arrange(member_casual, day_of_week)  %>% 
  ggplot(aes(x = day_of_week, y = average_duration, fill = member_casual)) +
  geom_col(position = "dodge") 

#Dropping rows with missing values in the start and end stations
clean_t <- na.omit(clean_trips)

#See the start location with the highest number of rides for member
clean_t %>% 
  group_by(day_of_week, start_station_name,member_casual = "member"  ) %>%  #groups by usertype and day
  summarise(number_of_rides = n()) %>% 	
  arrange(-number_of_rides,member_casual,start_station_name) %>% 
  head(20) %>%
  print(n=20)

#See the end location with the highest number of rides for member
clean_t %>% 
  group_by(day_of_week, end_station_name,member_casual = "member"  ) %>%  #groups by usertype and day
  summarise(number_of_rides = n()) %>% 	
  arrange(-number_of_rides,member_casual,start_station_name) %>% 
  head(20) %>%
  print(n=20)

# saving to variable and exporting to CSV for further visualizations
length_number_rides <- clean_trips %>% 
  group_by(member_casual, day_of_week) %>%  #groups by usertype and day
  summarise(number_of_rides = n()							#calculates the number of rides and average duration 
            ,average_duration = mean(ride_length)) %>% 		# calculates the average duration
  arrange(member_casual, day_of_week)	

write_csv(length_number_rides, file='length_number_rides.csv')








