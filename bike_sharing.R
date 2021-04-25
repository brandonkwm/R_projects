library(tidyverse)
library(dplyr)
library(lubridate)
library(tibble)
library(ggplot2)

setwd('/Users/brandonwong/Desktop/Google_cert_DA/bike_sharing_case_study/divvy_trip_data')
file_names_new_format <- dir()
divvy_trip_df_new_format <- do.call(rbind,lapply(file_names_new_format,read.csv))
divvy_trip_df_new_format_2 <- mutate(divvy_trip_df_new_format, start_time_new = as.POSIXct(started_at, format = '%Y-%m-%d %H:%M')) 
divvy_trip_df_new_format_2$start_time_new2 =  divvy_trip_df_new_format_2$start_time_new
divvy_trip_df_new_format_2 <- mutate(divvy_trip_df_new_format_2, end_time_new = as.POSIXct(ended_at, format = '%Y-%m-%d %H:%M'))
divvy_trip_df_new_format_2 <-  mutate(divvy_trip_df_new_format_2, trip_duration = (end_time_new - start_time_new)) %>% 
  relocate(trip_duration, .after=last_col())
divvy_trip_df_new_format_2 <- separate(divvy_trip_df_new_format_2, start_time_new2, c('year', 'month', 'day'))
divvy_trip_df_new_format_2 <- divvy_trip_df_new_format_2 %>% 
  relocate(year, .after=last_col()) %>% 
  relocate(month, .after=last_col()) %>% 
  relocate(day, .after=last_col())

divvy_trip_df_new_format_2 <-  select(divvy_trip_df_new_format_2,-c(rideable_type,start_lat,start_lng,end_lat,end_lng,started_at,ended_at))

file_names_old_format <- grep(list.files('/Users/brandonwong/Desktop/Google_cert_DA/bike_sharing_case_study/divvy_trip_data_old',
                                        recursive = TRUE, full.names=TRUE),pattern='README',invert=TRUE,value= TRUE)

divvy_trip_read_csv_old_format <- map(file_names_old_format, read_csv)
new_col_names <- c('trip_id','start_time','end_time','bike_id','trip_duration','from_station_id','from_station_name','to_station_id','to_station_name','user_type','gender','birth_year')
divvy_trip_read_csv_old_format <- map(divvy_trip_read_csv_old_format, ~rename_all(.x, ~new_col_names))


i<-1
for(i in i:24){
  divvy_trip_read_csv_old_format[[i]] <- mutate(divvy_trip_read_csv_old_format[[i]], start_time_new = as.POSIXct(start_time, format = "%m/%d/%Y %H:%M"))
  divvy_trip_read_csv_old_format[[i]] <- mutate(divvy_trip_read_csv_old_format[[i]], end_time_new = as.POSIXct(end_time, format = "%m/%d/%Y %H:%M"))
  divvy_trip_read_csv_old_format[[i]] <- mutate(divvy_trip_read_csv_old_format[[i]], trip_duration = end_time_new - start_time_new)
  divvy_trip_read_csv_old_format[[i]] <- select(divvy_trip_read_csv_old_format[[i]],-c(bike_id, start_time, end_time, gender, birth_year))
}

multi_join <- function(list_of_loaded_data, join_func, ...){
  require("dplyr")
  output <- Reduce(function(x, y) {join_func(x, y, ...)}, list_of_loaded_data)
  return(output)
}

new_col_names_old_format <- c('ride_id','start_station_name','start_station_id','end_station_name','end_station_id','member_casual','start_time_new','end_time_new','trip_duration')
merged_data_old_format <- multi_join(divvy_trip_read_csv_old_format, full_join)
merged_data_old_format <- merged_data_old_format %>%  
  relocate(trip_duration, .after=last_col()) %>%
  relocate(from_station_name, .after=trip_id) %>% 
  relocate(to_station_name, .after=from_station_id)
colnames(merged_data_old_format) <- new_col_names_old_format

merged_data_old_format$start_time_new2 =  merged_data_old_format$start_time_new
merged_data_old_format <- separate(merged_data_old_format, start_time_new2, c('year', 'month', 'day'))
merged_data_old_format <- merged_data_old_format %>%  
  relocate(year, .after=last_col()) %>%
  relocate(month, .after=last_col()) %>%
  relocate(day, .after=last_col())

merged_data_new_old <- rbind(divvy_trip_df_new_format_2, merged_data_old_format)
merged_data_new_old$member_casual[merged_data_new_old$member_casual == 'Subscriber'] <- 'member'
merged_data_new_old$member_casual[merged_data_new_old$member_casual == 'Customer'] <- 'casual'
merged_data_new_old$member_casual[merged_data_new_old$member_casual == 'Dependent'] <- 'casual'


#start of analysis
merged_data_new_old_year_rider_trend <- rename(count(merged_data_new_old, member_casual, year), member_type_rider_count = n)
merged_data_new_old_year_trip_duration <- merged_data_new_old %>% group_by(member_casual,year) %>%  summarise(total_trip_duration_hours = sum(trip_duration, na.rm=TRUE)/(60*60))


ggplot(merged_data_new_old_year_rider_trend, aes(x = year, fill=factor(member_type_rider_count))) +
  geom_bar() +
  xlab('member_type_rider_count') +
  labs(
    x = 'Year',
    y = 'Membership type count',
    title = paste('Rider type trend'),
    subtitle = paste('2014- 2021'),
    fill = 'member_type_rider_count'
  )

#end of analysis