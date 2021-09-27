# Install and load the "Tidyverse" package for data reading, cleaning and analysis
install.packages("tidyverse")
install.packages("skimr")
install.packages("ggpubr")
library(ggplot2)
library(tidyverse)
library(dplyr)
library(skimr)
library(data.table) 
#this is for fifelse(), which can keep date type after ifelse function
library(scales) 
# this is for scale_y_continuous(labels=comma), adjustment of x/y axis scale between scientific to comma format
library(ggpubr) 
# this is for ggarrange(), which is use to merge plots/graphs into a single graphs

# Upload the bike historical raw data
data_202009=read_csv("C:/Users/2015002-1/Desktop/Personal/證書 Google Data Analysts/Course 8/Case 1/Original Data/202009-divvy-tripdata.csv")
data_202010=read_csv("C:/Users/2015002-1/Desktop/Personal/證書 Google Data Analysts/Course 8/Case 1/Original Data/202010-divvy-tripdata.csv")
data_202011=read_csv("C:/Users/2015002-1/Desktop/Personal/證書 Google Data Analysts/Course 8/Case 1/Original Data/202011-divvy-tripdata.csv")
data_202012=read_csv("C:/Users/2015002-1/Desktop/Personal/證書 Google Data Analysts/Course 8/Case 1/Original Data/202012-divvy-tripdata.csv")
data_202101=read_csv("C:/Users/2015002-1/Desktop/Personal/證書 Google Data Analysts/Course 8/Case 1/Original Data/202101-divvy-tripdata.csv")
data_202102=read_csv("C:/Users/2015002-1/Desktop/Personal/證書 Google Data Analysts/Course 8/Case 1/Original Data/202102-divvy-tripdata.csv")
data_202103=read_csv("C:/Users/2015002-1/Desktop/Personal/證書 Google Data Analysts/Course 8/Case 1/Original Data/202103-divvy-tripdata.csv")
data_202104=read_csv("C:/Users/2015002-1/Desktop/Personal/證書 Google Data Analysts/Course 8/Case 1/Original Data/202104-divvy-tripdata.csv")
data_202105=read_csv("C:/Users/2015002-1/Desktop/Personal/證書 Google Data Analysts/Course 8/Case 1/Original Data/202105-divvy-tripdata.csv")
data_202106=read_csv("C:/Users/2015002-1/Desktop/Personal/證書 Google Data Analysts/Course 8/Case 1/Original Data/202106-divvy-tripdata.csv")
data_202107=read_csv("C:/Users/2015002-1/Desktop/Personal/證書 Google Data Analysts/Course 8/Case 1/Original Data/202107-divvy-tripdata.csv")
data_202108=read_csv("C:/Users/2015002-1/Desktop/Personal/證書 Google Data Analysts/Course 8/Case 1/Original Data/202108-divvy-tripdata.csv")


# take a first look of all 12 files
glimpse(data_202009)
glimpse(data_202010)
glimpse(data_202011)
glimpse(data_202012)
glimpse(data_202101)
glimpse(data_202102)
glimpse(data_202103)
glimpse(data_202104)
glimpse(data_202105)
glimpse(data_202106)
glimpse(data_202107)
glimpse(data_202108)

# By reading the example of each column data, I can easily interpret their meaning:
# ride_id: an unique ride order identification number 
# rideable_type: type of bike 
# started_at: time of start biking
# start_station_name: start biking location
# start_station_id: id of start biking location
# ended_at: time of start biking
# end_station_name: end biking location
# end_station_id: id of end biking location
# start_lat: lagitude of start biking location
# start_lng: longitude of start biking location
# end_lat: lagitude of end biking location
# end_lng: longitude of end biking location
# member_casual: causal or annual membership

# 1) Each file contains 13 columns and they seem to be in consistent format.
# Therefore I can merge all 12 files into 1 single file by rbind() function. 
bike_merged=rbind(data_202009,data_202010,data_202011,data_202012,data_202101,data_202102,data_202103,data_202104,data_202105,data_202106,data_202107,data_202108)
glimpse(bike_merged)

# The merged data frame contain a total of 4,913,072 rows of data and 13 columns.
# skim_without_charts() function is a good way to quick scan the data type, structure and distribution, as well as countering any error.


# 2) remove the unused columns: ride_id, start_station_id, end_station_id, start_lag, start_lng, end_lag, end_lng
# Those columns either have little use in analysis or can be compensated by other columns 
bike_merged_cleaning1=select(bike_merged,rideable_type,started_at,ended_at,start_station_name,end_station_name,member_casual)


# 3) remove those rows with any missing or blank (NA) cells. 
# Data missing: There are around 450000 missing data in start_station_name and 490000 missing in end_station_name.
# Each counted as around 10% of total rows numbers. So removing them should not affect the credibility of analysis.

bike_merged_cleaning2=drop_na(bike_merged_cleaning1)
skim_without_charts(bike_merged_cleaning2)

# There are 4,234,022 rows left. It is (4234022/4913072)=86.18% of valid data after these blank cells cleaning.


# 4) Create a column "ride_length(min)" to show the duration of each ride
bike_merged_cleaning3=mutate(bike_merged_cleaning2,ride_length=round(difftime(ended_at,started_at,unit="mins"),2))

# 5) It is figured out that there are negative value in ride_length. 
# By deep looking at the data, it may be an error of mixing started_time and ended_time.
# Therefore, I have to switch those error data back to correct date column
# and then, calculate the revised_ride_length(min) and convert it into numeric format.
bike_merged_cleaning3_1=mutate(bike_merged_cleaning3,revised_started_at=fifelse(started_at<ended_at,started_at,ended_at),
  revised_ended_at=fifelse(started_at<ended_at,ended_at,started_at))
bike_merged_cleaning3_2=mutate(bike_merged_cleaning3_1,"revised_ride_length(min)"=as.numeric(round(difftime(revised_ended_at,revised_started_at,unit="mins"),2)))
bike_merged_cleaning3_2$revised_ride=as.numeric(bike_merged_cleaning3_2$'revised_ride_length(min)')


# And then Drop those wrong columns
bike_merged_cleaning4=bike_merged_cleaning3_2 %>% 
  select(-c(started_at,ended_at,ride_length,"revised_ride_length(min)"))

# 6) Add 1 new column "day_of_week" to show the weekday of each start ride.
# Here I have to set local time back to US, in order to present weekday in English.
Sys.setlocale("LC_TIME", "English")
bike_merged_cleaning5=mutate(bike_merged_cleaning4,day_of_week=weekdays(revised_started_at, abbreviate = FALSE))

# 7) Add 1 new column "hour_start" to show when (hour) of each ride start.
bike_merged_cleaning6=mutate(bike_merged_cleaning5,hour_start=(hour(revised_started_at)))
bike_merged_cleaning6$revised_ride=as.numeric(bike_merged_cleaning6$'revised_ride_length(min)')

# 8) ggplot(bike_merged_cleaning6, aes(x=revised_ride)) +geom_histogram(binwidth=10)
# The box plot is not so clear. It is because the data is highly skewed to the right and the ride_length range is too large.
# In order to eliminate the outliers (too big and too small). I will select 2.5 th and 97.5 th percentile as cutting points of data selection.
# 95% of data will be fallen within this range.
# This mean only the middle 95% of data will be picked up for analysis. The first 2.5% and last 2.5% will be removed for preventing outliers (e.g. operation error). 
# These 95% of data (i.e. +/- 2 sigma) should be confident enough to represent our real business operation condition.  

quar_025=quantile(bike_merged_cleaning6$revised_ride, c(.025)) 
quar_975=quantile(bike_merged_cleaning6$revised_ride, c(.975)) 

# The result is 2.02 min and 90 min respectively, for the middle 95% data.
# Then I filter the ride length data within this range.

bike_merged_cleaning7=bike_merged_cleaning6 %>% 
  filter(revised_ride>=2.02 & revised_ride<=90)
skim_without_charts(bike_merged_cleaning7)


# 9) Create 1 column "day_of_month" to show which day of month of each ride start.
bike_merged_cleaning8=mutate(bike_merged_cleaning7,day=format(bike_merged_cleaning7$revised_started_at, format = "%d"))

# 10) Create 1 column "month" to show which month of each ride start.
bike_merged_cleaned=mutate(bike_merged_cleaning8,month=format(bike_merged_cleaning8$revised_started_at, format = "%m"))



# Key Points of Processing Steps
# 1. Merge all 12 raw data csv files into 1 data frame
# 2. Remove the unused or duplicated information columns
# 3. Remove those rows with N/A blank data
# 4. Create a new column "Ride_Length(mins)" from the difference between start time and end time
# 5. Figure our some data have a end time early than start time. This may be due to wrong data entry in columns. 
# For those end time is earlier than start time, I made a switched and swap them back.And further investigation is needed.
# A new column "revised_ride" are generated to replace "Ride_Length(mins)"
# 6. Create a new column "day_of_week" which is calculated as the weekday of week.
# 7. Create a new column "hour_start", which represent the hour-time of start biking.
# 8. Ride length with significantly extreme large (beyond 97.5th percentile) or small (less than 2.5th percentile).
# These extreme data may be consider as data operation error and would be removed. 
# Only middle 95% (~+/-2 sigma) will be picked up and represent our real data.
# 9. Create 1 column "day_of_month" to show which day of month of each ride start.
# 10. Create 1 column "month" to show which month of each ride start.

View(bike_merged_cleaned)                               
skim_without_charts(bike_merged_cleaned)
# save(bike_merged_cleaned, file='D:/One Drive/OneDrive - HKUST Connect/Google Data Analytics/Course 8/Case 1/Proceed Data/bike_merged_cleaned.rda')



# Data Analysis
# From this part, it is better to load the previous saved rda file "bike_merged_clean.rda" for graphical visualization
# 1) Overall Information 

information_overall=bike_merged_cleaned %>% 
  group_by(member_casual) %>% 
  summarise(number_of_ride=n(),total_ride_min=sum(revised_ride),mean_ride_min=round(mean(revised_ride),1),min_ride_min=round(min(revised_ride),1),max_ride_min=round(max(revised_ride),1))

information_overall_table=ggtexttable(information_overall, rows = NULL, theme = ttheme("mOrange"))

ggplot(information_overall)+geom_col(aes(x=member_casual,y=total_ride_min,fill=member_casual))+
  geom_text(aes(x=member_casual,y=total_ride_min,label=number_of_ride),vjust=5)+
  geom_text(aes(x=member_casual,y=total_ride_min,label=paste0(round(number_of_ride/sum(number_of_ride)*100,2),"%")  ),vjust=7)+
  labs(title="Number of ride for each user type")+
  theme_classic()+theme(legend.title=element_blank())+
  scale_y_continuous(labels=comma)

#2) Information by Month

information_by_month=bike_merged_cleaned %>% 
  group_by(member_casual,month) %>% 
  summarise(number_of_ride=n(),total_ride_min=sum(revised_ride),mean_ride_min=round(mean(revised_ride),1))
information_by_month_table=ggtexttable(information_by_month, rows = NULL, theme = ttheme("mBlue"))


# 2a. Total ride numbers
month_3=ggplot(information_by_month)+geom_col(position="dodge",aes(month,number_of_ride,fill=member_casual))+
  theme_classic()+
  labs(title="Total ride numbers in different month")+
  theme(legend.title=element_blank())+ 
  scale_y_continuous(labels=comma)

month_1=ggplot(information_by_month)+geom_line(aes(x=month,y=number_of_ride,group=member_casual,color=member_casual))+
  geom_point(aes(x=month,y=number_of_ride,group=member_casual,color=member_casual))+
  labs(title="Rides number in each month")+
  theme_classic()+
  theme(legend.title=element_blank())+ 
  scale_y_continuous(labels=comma)


# 2b. Average minutes per ride 

month_2=ggplot(information_by_month)+geom_line(aes(x=month,y=mean_ride_min,group=member_casual,color=member_casual))+
  geom_point(aes(x=month,y=mean_ride_min,group=member_casual,color=member_casual))+
  labs(title="Average mintue per ride in each month")+
  theme_classic()+
  theme(legend.title=element_blank())+ 
  scale_y_continuous(labels=comma)

month_figure1=ggarrange(month_1,month_2,ncol=1,nrow =2,common.legend=TRUE)




#3) Information by Day

information_by_day=bike_merged_cleaned %>% 
  group_by(member_casual,day) %>% 
  summarise(number_of_ride=n(),total_ride_min=sum(revised_ride),mean_ride_min=round(mean(revised_ride),1))


# 3a. Average ride numbers (By day)

day_1=ggplot(information_by_day)+geom_line(aes(x=day,y=number_of_ride/12,group=member_casual,color=member_casual))+
  geom_point(aes(x=day,y=number_of_ride/12,group=member_casual,color=member_casual))+
  labs(title="Average ride numbers in each day")+
  theme_classic()+
  theme(legend.title=element_blank())+ 
  scale_y_continuous(labels=comma)

# 3b. Average minutes per ride (By day)

day_2=ggplot(information_by_day)+geom_line(aes(x=day,y=mean_ride_min,group=member_casual,color=member_casual))+
  geom_point(aes(x=day,y=mean_ride_min,group=member_casual,color=member_casual))+
  labs(title="Average mintue per ride in each day")+
  theme_classic()+
  theme(legend.title=element_blank())+ 
  scale_y_continuous(labels=comma)

#4) Information by Hour

information_by_hour=bike_merged_cleaned %>% 
  group_by(member_casual,hour_start) %>% 
  summarise(number_of_ride=n(),total_ride_min=sum(revised_ride),mean_ride_min=round(mean(revised_ride),1))

# 4a. Average ride numbers (By hour)

hour_1=ggplot(information_by_hour)+geom_line(aes(x=hour_start,y=number_of_ride,group=member_casual,color=member_casual))+
  geom_point(aes(x=hour_start,y=number_of_ride,group=member_casual,color=member_casual))+
  labs(title="Average ride numbers in each hour")+
  theme_classic()+
  theme(legend.title=element_blank())+ 
  scale_y_continuous(labels=comma)

# 4b. Average minutes per ride (By hour)

hour_2=ggplot(information_by_hour)+geom_line(aes(x=hour_start,y=mean_ride_min,group=member_casual,color=member_casual))+
  geom_point(aes(x=hour_start,y=mean_ride_min,group=member_casual,color=member_casual))+
  labs(title="Average mintue per ride in each hour")+
  theme_classic()+
  theme(legend.title=element_blank())+ 
  scale_y_continuous(labels=comma)



#5) Information by Weekday

information_by_weekday=bike_merged_cleaned %>% 
  group_by(member_casual,day_of_week) %>% 
  summarise(number_of_ride=n(),total_ride_min=sum(revised_ride),mean_ride_min=round(mean(revised_ride),1))
weekday_order=c("Sunday","Monday","Tuesday","Wednesday","Thursday","Friday","Saturday")
# 5a. Average ride numbers (By weekday)

weekday_1=ggplot(information_by_weekday)+geom_line(aes(x=day_of_week,y=number_of_ride,group=member_casual,color=member_casual))+
  geom_point(aes(x=day_of_week,y=number_of_ride,group=member_casual,color=member_casual))+
  labs(title="Average ride numbers in each weekday")+
  theme_classic()+
  theme(legend.title=element_blank(),axis.text.x=element_text(angle=45,vjust=0.5))+ 
  scale_y_continuous(labels=comma)+
  scale_x_discrete(limits =weekday_order)

# 5b. Average minutes per ride (By weekday)

weekday_2=ggplot(information_by_weekday)+geom_line(aes(x=day_of_week,y=mean_ride_min,group=member_casual,color=member_casual))+
  geom_point(aes(x=day_of_week,y=mean_ride_min,group=member_casual,color=member_casual))+
  labs(title="Average mintue per ride in each weekday")+
  theme_classic()+
  theme(legend.title=element_blank(),axis.text.x=element_text(angle=45,vjust=0.5))+ 
  scale_y_continuous(labels=comma)+
  scale_x_discrete(limits =weekday_order)



#6) Information by Bike type

information_by_bike=bike_merged_cleaned %>% 
  group_by(member_casual,rideable_type) %>% 
  summarise(number_of_ride=n(),total_ride_min=sum(revised_ride),mean_ride_min=round(mean(revised_ride),1))
information_by_bike_table=ggtexttable(information_by_bike, rows = NULL, theme = ttheme("mRed"))


# 6a. Ride numbers (By bike type)

bike_1=ggplot(information_by_bike)+geom_col(position="dodge",aes(rideable_type,number_of_ride,fill=member_casual))+
  theme_classic()+
  labs(title="Total ride numbers in bike type")+
  theme(legend.title=element_blank())+ 
  scale_y_continuous(labels=comma)


# 6b. Average minutes per ride (By bike type)

bike_2=ggplot(information_by_bike)+geom_col(position="dodge",aes(rideable_type,mean_ride_min,fill=member_casual))+
  theme_classic()+
  labs(title="Average ride minutes in bike type")+
  theme(legend.title=element_blank())+ 
  scale_y_continuous(labels=comma)

#7) Information by starting location

information_member_top10=bike_merged_cleaned %>% 
  filter(member_casual=="member") %>% 
  group_by(start_station_name) %>% 
  summarise(number_of_ride=n(),total_ride_min=sum(revised_ride),mean_ride_min=round(mean(revised_ride),1)) %>% 
  slice_max(n=10,order_by=number_of_ride) # select top 10 station

information_casual_top10=bike_merged_cleaned %>% 
  filter(member_casual=="casual") %>% 
  group_by(start_station_name) %>% 
  summarise(number_of_ride=n(),total_ride_min=sum(revised_ride),mean_ride_min=round(mean(revised_ride),1)) %>% 
  slice_max(n=10,order_by=number_of_ride) # select top 10 station


View(information_casual_top10)

# 7a. Top 10 Member ride starting location
member_start_1=ggplot(information_member_top10)+geom_col(aes(x=start_station_name,y=number_of_ride,fill=start_station_name))+
  theme_classic()+
  labs(title="Total 10 starting station for members")+
  theme(legend.title=element_blank(),axis.text.x=element_blank())+ 
  scale_y_continuous(labels=comma,limits = c(0,50000))+
  scale_x_discrete(limits =information_member_top10$start_station_name)+
  scale_fill_discrete(limits =information_member_top10$start_station_name)


# 7b. Top 10 Casual ride starting location
casual_start_1=ggplot(information_casual_top10)+geom_col(aes(x=start_station_name,y=number_of_ride,fill=start_station_name))+
  theme_classic()+
  labs(title="Total 10 starting station for casuals")+
  theme(legend.title=element_blank(),axis.text.x=element_blank())+ 
  scale_y_continuous(labels=comma)+
  scale_x_discrete(limits =information_casual_top10$start_station_name)+
  scale_fill_discrete(limits =information_casual_top10$start_station_name)

ggarrange(member_start_1,casual_start_1,nrow=1)

# 8 Route
route_1=bike_merged_cleaned %>% 
  select(member_casual,start_station_name,end_station_name )
route_2=route_1 %>% 
  mutate("route_range"=fifelse(start_station_name>=end_station_name,paste0(start_station_name," <-> ",end_station_name),paste0(end_station_name," <-> ",start_station_name)))

# 8a Member top 10 route
route_member_top10=route_2 %>% 
  filter(member_casual=="member") %>% 
  group_by(route_range) %>% 
  summarise(route_count=n()) %>% 
  slice_max(n=10,order_by=route_count) # select top 10 route trip by member

route_member_graph=ggplot(route_member_top10)+geom_col(aes(x=route_range,y=route_count,fill=route_range))+
  theme_classic()+
  labs(title="Total 10 routes for members")+
  theme(legend.title=element_blank(),axis.text.x=element_blank())+ 
  scale_y_continuous(labels=comma,limits = c(0,10000))+
  scale_x_discrete(limits =route_member_top10$route_range)+
  scale_fill_discrete(limits =route_member_top10$route_range)


# 8b Casual top 10 route
route_casual_top10=route_2 %>% 
  filter(member_casual=="casual") %>% 
  group_by(route_range) %>% 
  summarise(route_count=n()) %>% 
  slice_max(n=10,order_by=route_count) # select top 10 route trip by casual

route_casual_graph=ggplot(route_casual_top10)+geom_col(aes(x=route_range,y=route_count,fill=route_range))+
  theme_classic()+
  labs(title="Total 10 routes for casual")+
  theme(legend.title=element_blank(),axis.text.x=element_blank())+ 
  scale_y_continuous(labels=comma,limits = c(0,10000))+
  scale_x_discrete(limits =route_casual_top10$route_range)+
  scale_fill_discrete(limits =route_casual_top10$route_range)

route_member_graph
