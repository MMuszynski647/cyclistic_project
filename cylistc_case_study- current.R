#install needed packages
##tidyverse - to wrangle data
##lubridate - for date related functions
##ggplot - for visualization

library(tidyverse)
library(lubridate)
library(ggplot2)

###Collect data 
q2_2019 <- read.csv("Divvy_Trips_2019_Q2.csv")
q3_2019 <- read.csv("Divvy_Trips_2019_Q3.csv")
q4_2019 <- read.csv("Divvy_Trips_2019_Q4.csv")
q1_2020 <- read.csv("Divvy_Trips_2020_Q1.csv")

#Make sure all columns match, but they don't have to be in the same order

colnames(q2_2019)
colnames(q3_2019)
colnames(q4_2019)
colnames(q1_2020)

#Rename columns to make them consistent with q1_2020 so they can be combined later

(q2_2019 <- rename(q2_2019
                   ,ride_id = "X01...Rental.Details.Rental.ID"
                   ,rideable_type = "X01...Rental.Details.Bike.ID"
                   ,started_at = "X01...Rental.Details.Local.Start.Time" 
                   ,ended_at = "X01...Rental.Details.Local.End.Time" 
                   ,start_station_name = "X03...Rental.Start.Station.Name"
                   ,start_station_id = "X03...Rental.Start.Station.ID"
                   ,end_station_name = "X02...Rental.End.Station.Name"
                   ,end_station_id = "X02...Rental.End.Station.ID" 
                   ,member_casual = "User.Type"))

(q3_2019 <- rename(q3_2019
                   ,ride_id = trip_id
                   ,rideable_type = bikeid
                   ,started_at = start_time
                   ,ended_at = end_time
                   ,start_station_name = from_station_name
                   ,start_station_id = from_station_id
                   ,end_station_name = to_station_name
                   ,end_station_id = to_station_id
                   ,member_casual = usertype))

(q4_2019 <- rename(q4_2019
                   ,ride_id = trip_id
                   ,rideable_type = bikeid
                   ,started_at = start_time
                   ,ended_at = end_time
                   ,start_station_name = from_station_name
                   ,start_station_id = from_station_id
                   ,end_station_name = to_station_name
                   ,end_station_id = to_station_id
                   ,member_casual = usertype))

#validate dataframes

str(q2_2019)
str(q3_2019)
str(q4_2019)
str(q1_2020)

#Convert ride_id and rideable_type to character so they can stack correctly

q2_2019 <- mutate(q2_2019, ride_id = as.character(ride_id)
                  ,rideable_type = as.character(rideable_type))
q3_2019 <- mutate(q3_2019, ride_id = as.character(ride_id)
                  ,rideable_type = as.character(rideable_type))
q4_2019 <- mutate(q4_2019, ride_id = as.character(ride_id)
                  ,rideable_type = as.character(rideable_type))

#combine dataframes into one
all_trips <- bind_rows(q2_2019, q3_2019, q4_2019, q1_2020)

#remove columns you don't need: lat, long, birthyear, and gender

all_trips <- all_trips %>%
  select(-c(start_lat, start_lng, end_lat, end_lng, birthyear, gender, "X01...Rental.Details.Duration.In.Seconds.Uncapped", "X05...Member.Details.Member.Birthday.Year",
            "Member.Gender", "tripduration"))

###CLean up and add data for analysis
#inspect columns amd rows
colnames(all_trips)
nrow(all_trips)
dim(all_trips)
head(all_trips)
str(all_trips)
summary(all_trips)

#1. We need to consolidate the types of riders from 4 to 2
#2. Need to add colmuns day, month, year to aggregate the data
#3. add a ride_length column to the data frame
#4. delete  rides with negative duration or bikes  taken out of circulation

table(all_trips$member_casual)

#reassign member_casual values to follow the 2020 labels
all_trips <- all_trips %>%
  mutate(member_casual = recode(member_casual,
                              "Subscriber" = "member"
                                ,"Customer"= "casual"))

#make sure  values were reassigned properly
table(all_trips$member_casual)

#Add new columns for date, month, day, and year of each ride, this will help aggregate the data
all_trips$date <- as.Date(all_trips$started_at)
all_trips$month <- format(as.Date(all_trips$date),"%m")
all_trips$day <- format(as.Date(all_trips$date), "%d")
all_trips$year <- format(as.Date(all_trips$date), "%Y")
all_trips$day_of_week <- format(as.Date(all_trips$date), "%A")

#add a ride_length column 
all_trips$ride_length <- difftime(all_trips$ended_at,all_trips$started_at)

#validate
str(all_trips)

#Convert "ride_length" from factor to numeric 
is.factor(all_trips$ride_length)
all_trips$ride_length <- as.numeric(as.character(all_trips$ride_length))
is.numeric(all_trips$ride_length)

#remove bad data that can't be used in analysis

all_trips_v2 <- all_trips[!(all_trips$start_station_name == "HQ QR" | all_trips$ride_length<0),]
###==========================================================================
##Descriptive analysis
#ride_length
mean(all_trips_v2$ride_length) 
#midpoint
median(all_trips_v2$ride_length) 
#longest ride
max(all_trips_v2$ride_length)
#shortest ride
min(all_trips_v2$ride_length)

#condense
summary(all_trips_v2$ride_length)

#compare members and casual riders
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = mean)
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = median)
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = max)
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = min)

#see average ride time for each day for members vs casual users
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual + all_trips_v2$day_of_week, FUN = mean)

#put days in order
aggregate(all_trips_v2$day_of_week <- ordered(all_trips_v2$day_of_week, levels=c("Sunday", "Monday", "Tuesday", "Wednesday",
                                                                                 "Thursday", "Friday", "Saturday")))

#run average ride time by each day for members vs casual users
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual + all_trips_v2$day_of_week, FUN = mean)

#analyze ridership data by type and weekday
all_trips_v2 %>%
  mutate(weekday = wday(started_at, label = TRUE)) %>%
  wday()
group_by(member_casual, weekday) %>%
  summarise(number_of_rides = n(),
            average_duration = mean(ride_length)) %>%
  arrange(member_casual, weekday)

# Let's visualize the number of rides by rider type
all_trips_v2 %>%
  mutate(weekday = wday(started_at, label = TRUE)) %>%
  group_by(member_casual, weekday) %>%
  summarise(number_of_rides = n(),average_duration = mean(ride_length)) %>%
  arrange(member_casual, weekday) %>%
  ggplot(aes(x = weekday, y = number_of_rides, fill = member_casual)) +
  geom_col(position = "dodge")

#create a visualization for average duration
all_trips_v2 %>%
  mutate(weekday = wday(started_at, label = TRUE)) %>%
  group_by(member_casual, weekday) %>%
  summarise(number_of_rides = n()
            ,average_duration = mean(ride_length)) %>%
  arrange(member_casual, weekday) %>%
  ggplot(aes(x = weekday, y = average_duration, fill = member_casual)) +
  geom_col(position = "dodge")

#export summary file
counts <- aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual + 
                      all_trips_v2$day_of_week, FUN = mean)
write.csv(counts, file = '~/cyclistic_case_study/avg_ride_length.csv')

write.csv(all_trips_v2, file = '~/cyclistic_case_study/all_trips_v2_tableau.csv', row.names = FALSE)
