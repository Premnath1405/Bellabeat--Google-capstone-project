# Bellabeat--Google-capstone-project
INTRODUCTION

Welcome to the case study developed as part of the Google Data Analytics Capstone activities of the Google Data Analytics Professional Certificate course. This course promoted knowledge and skills development about the analysis processes (ask, prepare, process, analyze, share and act) that were useful in this case study. This study made it possible to put this knowledge into practice to answer key business questions from real scenarios, such as the high-tech manufacturer of health-focused products for women, Bellabeat. To develop this project I chose to use R Studio

About Bellabeat

Here at Bellabeat, women’s health is our passion. Bellabeat is a high-tech company that manufactures health-focused smart products worldwide. Urška Sršen and Sando Mur founded Bellabeat in 2013, with the intent to develop beautifully designed technology that informs and inspires women around the world.

Analysis Objectives

What are some trends in smart device usage?

How could these trends apply to Bellabeat customers?

How could these trends help influence Bellabeat marketing strategy?

Business Task

Utilize the Fitbit Fitness Tracker dataset to derive potential growth opportunities and make analysis based recommendations to our marketing operations team.

ASK

A clear statement of the business task

The purpose of the business task is to analyze data from non-Bellabeat smart device users to gain insights that can be applied to Bellabeat smart devices, such as the smart bracelet, "Leaf", or the Bellabeat subscription. In addition, trends applicable to users and influencing marketing strategies are welcome.

PREPARE

A description of all data sources used

This dataset is located on Kaggle and is in the public domain - FitBit Fitness Tracker Data (https://www.kaggle.com/datasets/arashnic/fitbit). They are presented in spreadsheets, with the data organized into 18 files and titled by category. The archives include information such as: Id numbers, date, daily activity, heart rate, calories, steps, and Intensities por horas, as well METs (metabolic equivalent of task) by minute, sleep day and information about weight. Data were obtained between April 12 to May 12, 2016. In addition, the survey was developed by Amazon Mechanical Turk, that is, a reputable company that contributes to the integrity of the data but needs to be confirmed.

PROCESS

I downloaded data from "FitBit Fitness Tracker Data" to my computer.

The data was all in the form of a .csv file, a total of 18 files. I opened some files in Excel and realized that they were very large. Therefore, it would be efficient to perform the cleaning and analysis by SQL or R Studio. I preferred to go with R sudio

Environment Setup
install.packages(“tidyverse”) install.packages(“lubridate”) install.packages(“dplyr”) install.packages(“ggplot2”) install.packages(“tidyr”) install.packages(“viridisLite”) install.packages(“scales”) install.packages(“devtools”) devtools::install_github(“hadley/devtools”) remotes::install_github(“gadenbuie/cleanrmd”)

IMPORTING DATASETS

activity <- read_csv(file = "E:/Data/Case study/Bellabeat/archive/Fitabase Data 4.12.16-5.12.16/dailyActivity_merged.csv")
hourly_calories <- read_csv(file = "E:/Data/Case study/Bellabeat/archive/Fitabase Data 4.12.16-5.12.16/hourlyCalories_merged.csv")
hourly_intensities <- read_csv(file = "E:/Data/Case study/Bellabeat/archive/Fitabase Data 4.12.16-5.12.16/hourlyIntensities_merged.csv")
sleep <- read_csv(file = "E:/Data/Case study/Bellabeat/archive/Fitabase Data 4.12.16-5.12.16/sleepDay_merged.csv")
weightlog <- read_csv(file = "E:/Data/Case study/Bellabeat/archive/Fitabase Data 4.12.16-5.12.16/weightLogInfo_merged.csv")


DATA CLEAN AND PREPARATION

Now that we have our data loaded in, we will check our population per dataset.

n_distinct(activity$Id)
n_distinct(hourly_calories$Id)
n_distinct(hourly_intensities$Id)
n_distinct(sleep$Id)
n_distinct(weightlog$Id)


Based on our total population of 33 users, the weightlog dataset will have an insufficient sample size to be used in this analysis.

Now we want to check for duplicates.

sum(duplicated(sleep))
sum(duplicated(activity))
sum(duplicated(hourly_calories))
sum(duplicated(hourly_intensities))

We can see that the sleep data contains duplicates and needs to be cleaned.

sleep <- unique(sleep)
sum(duplicated(sleep))

we have cleaned our data, Now we will standardize the data’s column names.

activity <- rename_with(activity, tolower)
sleep <- rename_with(sleep, tolower)
hourly_calories <- rename_with(hourly_calories, tolower)
hourly_intensities <- rename_with(hourly_intensities, tolower)

standardize our date and time format throughout our datasets.

activity <- activity %>% 
  rename(date= activitydate) %>% 
  mutate(date= as_date(date, format= "%m/%d/%Y"))

sleep <- sleep %>%
  rename(date= sleepday) %>%
  mutate(date= as_date(date, format= "%m/%d/%Y  %I:%M:%S %p", tz= Sys.timezone()))

hourly_intensities <- hourly_intensities %>% 
  rename(date_time= activityhour) %>% 
  mutate(date_time= as.POSIXct(date_time, format="%m/%d/%Y %I:%M:%S %p", tz= Sys.timezone()))

hourly_calories <- hourly_calories %>% 
  rename(date_time= activityhour) %>% 
  mutate(date_time= as.POSIXct(date_time, format="%m/%d/%Y %I:%M:%S %p", tz= Sys.timezone()))

Now that our data is consistent, we will merge our data.

hourly_calories_intensities <- merge(x = hourly_calories, y = hourly_intensities, by = c("id","date_time"))
activity_sleep <- merge( x = activity, y = sleep, by = c("id", "date"))

activity %>% 
  select(totalsteps, totaldistance,calories) %>% 
  summary()


activity %>% 
  select(veryactiveminutes, fairlyactiveminutes, lightlyactiveminutes, sedentaryminutes) %>% 
  summary()

sleep %>% 
  select(totalminutesasleep) %>% 
  summary()

hourly_calories_intensities %>% 
  select(totalintensity, averageintensity, calories) %>% 
  summary()

KEY FINDINGS

1.The population has an average daily step count of 7638. This is low compared to the CDC recommended step count of 10,000.

2.The average daily distance traveled was 5.49 miles.

3.The total user very active and fairly active minutes was 34.72 or 0.59 hours while user’s lightly active and sedentary minutes was 1184 minutes or 19.73 hours.

4.The average sleep minutes for users was 419.2 or 6.99 hours, which is just under the CDC recommended 7 hours for adults 18-60 years old.


It will be necessary for us to associate the day of the week with our data so we will add a weekday column to both of our data sets.
For our hourly data, we will also separate time from the date column.

hourly_calories_intensities <- hourly_calories_intensities %>% 
  separate(date_time, into= c('date', 'time'), sep= c(' ')) %>% 
  mutate(date= ymd (date))
  
hourly_calories_intensities$weekday <- weekdays(hourly_calories_intensities$date) 
Group weekday by time.
hourly_calories_intensities_day_time <- (hourly_calories_intensities) %>% 
  group_by(weekday, time)%>% 
  summarize(mean_avg_intensity= mean(averageintensity, na.rm = TRUE))
  
`summarise()` has grouped output by 'weekday'. You can override using the`.groups` argument.
Organize with Monday as starting weekday.

hourly_calories_intensities_day_time$weekday <- factor(hourly_calories_intensities_day_time$weekday, levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))

DATA VISUALIZATION


Now that our data is cleaned and prepared, we will begin visualizing our data in order to derive correlations and important findings.
We will begin by examining the relationship between the day of the week, time and user intensity output.

ggplot(hourly_calories_intensities_day_time, aes(time, weekday))+
  theme(axis.text.x= element_text(angle = 90))+
  labs(title= "Daily Intensity Output", x = " ", y = " ", fill = "Average Intensity Output", caption = 'Data Source: Fitabase Data 4.1.2.16-5.12.16')+
  geom_tile(color = "black", aes(fill = mean_avg_intensity))+
  scale_fill_gradient(low= "grey", high= "deeppink4")+
  theme(plot.title = element_text(hjust = 0.5, size = 16))


![Daily intensity output](https://github.com/Premnath1405/Bellabeat--Google-capstone-project/assets/128468794/2d90d2d9-3807-4d62-9f4f-b7f85570b01d)

KEY FINDINGS

The data shows us that our population is more active earlier on week days than weekends. We also see that, on average, higher levels of intensity were out later in the day versus early in the day. We see that users are most active on Saturday, between 11:00am - 2:00pm, and Wednesday, between 5:00pm and 6:00pm.
Searching for Correlations
Next we will analyze user activity level and search for correlations.
We will be categorizing user’s activity level according to the NIH sponsored, peer-reviewed article, “Physical activity for campus employees: a university worksite wellness program”. The article uses average step count to categorize activity level as such: sedentary (< 5000 steps/day), low active (5000–7499), somewhat active (7500–9999), active (10,000–12,499), or highly active (≥ 12,500).

Note: Based on our sample size we will reference highly active and active users as one group.


activity_sleep$user_steps <- " "

activity_sleep_grouped <- activity_sleep %>% 
  group_by (id) %>% 
  summarize(average_totalsteps = mean(totalsteps),
            average_totalcalories = mean(calories),
            average_totaldistance = mean(totaldistance),
            average_minutesasleep = mean(totalminutesasleep, na.rm = TRUE)) %>% 
  mutate(user_steps = case_when(
            average_totalsteps >= 10000 ~ "Highly Active/Active",
            average_totalsteps >= 7500 & average_totalsteps < 10000 ~ "Somewhat Active",
            average_totalsteps >= 5000 & average_totalsteps < 7500 ~ "Low Active",
            average_totalsteps < 5000 ~ "Sedentary"))

activity_sleep <- subset(activity_sleep, select = -user_steps)

activity_sleep_grouped <- merge(activity_sleep, activity_sleep_grouped, by= c("id"))

activity_sleep_grouped$user_steps <- factor(activity_sleep_grouped$user_steps, levels = c("Sedentary", "Low Active", "Somewhat Active", "Highly Active/Active"))


Activity level vs daily sleep minutes


ggplot(activity_sleep_grouped, aes(user_steps, totalminutesasleep))+
  geom_boxplot(aes(fill= user_steps))+
  geom_point(alpha = 0.5, aes(size = calories, color = calories))+
  labs(title = "Activity Level vs Daily Sleep Minutes", x = "Activity Level", y = "Daily Sleep Minutes", fill= "Activity Level", color= "Daily Calories Burned", caption= "Data Source: 
Physical activity for campus employees: a university worksite wellness program")+
  coord_flip()+
  scale_fill_brewer(palette="PiYG")+
  scale_color_gradient(low= "grey2", high= "red")+
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5, size = 16))+
  theme(plot.caption = element_text(hjust = 1.75))+
  guides(size = "none",fill ="none")


![activity level vs daily sleep minutes](https://github.com/Premnath1405/Bellabeat--Google-capstone-project/assets/128468794/646002a8-0e5d-41fa-a013-f682bd780e0c)


Key Insights:

We can see that there is no significant correlation between activity level and daily sleep minutes.

Activity level vs total daily steps.

ggplot(activity_sleep_grouped, aes(user_steps, totalsteps))+
  geom_boxplot(aes(fill= user_steps))+
  geom_point(alpha = 0.5, aes(size = calories, color = calories))+
  labs(title = "Activity Level vs Daily Steps", x = "Activity Level", y = "Daily Steps", fill= "Activity Level", size= "", color= "Daily Calories Burned", caption= "Data Source: Physical activity for campus employees: a university worksite wellness program")+
  coord_flip()+
  scale_fill_brewer(palette="PiYG")+
  scale_color_gradient(low= "grey2", high= "red")+
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5, size = 16))+
  theme(plot.caption = element_text(hjust = 1.75))+
  guides(size = "none",fill ="none")


![activity level vs daily steps](https://github.com/Premnath1405/Bellabeat--Google-capstone-project/assets/128468794/c85ff013-9d1c-4d4c-9854-85910e5b99f8)


Key Insights

1.There is a high correlation between activity level and the max amount of daily steps users take.

2.We also see that users in the more active groups have a more spread out range of user steps.

3.Users in the more active groups burn more calories per step, with somewhat active users burning the highest caloric burn.

Activity level vs total calories Lost.

ggplot(activity_sleep_grouped, aes(user_steps, average_totalcalories))+
  geom_boxplot(aes(fill= user_steps))+
  geom_point(alpha = 0.5, aes(size = average_totalcalories, color = average_totalcalories))+
  labs(title = "Activity Level vs Total Calories Lost", x = "Activity Level", y = "Total Calories Lost", fill= "Activity Level", size= "", color= "Total Calories", caption= "Data Source: Physical activity for campus employees: a university worksite wellness program")+
  coord_flip()+
  scale_fill_brewer(palette="PiYG")+
  scale_color_gradient(low= "grey2", high= "red")+
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5, size = 16))+
  theme(plot.caption = element_text(hjust = 1.75))+
  guides(size = "none",fill ="none")


![activity level vs total calorie lost](https://github.com/Premnath1405/Bellabeat--Google-capstone-project/assets/128468794/9c776224-ffae-4e41-8056-b1293032d52e)


Key Insights:

1. There is a high correlation between activity level and the max amount of daily steps users take.

2. We also see that users in the more active groups have a more spread out range of user steps.

3. Users in the more active groups burn more calories per step, with somewhat active users burning the highest caloric burn.

Activity level vs total distance traveled.
ggplot(activity_sleep_grouped, aes(x= user_steps, y= average_totaldistance))+
  geom_point(alpha = 0.5, aes(size = average_totalcalories, color = average_totalcalories))+
  geom_segment(aes(x= user_steps,
                    xend= user_steps,
                    y= min(average_totaldistance),
                    yend= max(average_totaldistance)),
                linetype= "dashed",
                size= 0.1)+ 
  labs(title = "Activity Level vs Distance traveled", x= "Activity Level", y= "Miles", size= "", color= "Total Calories", caption= 'Data Source: Fitabase Data 4.1.2.16-5.12.16')+
  coord_flip()+
  scale_color_gradient(low= "grey2", high= "red")+
  theme_set(theme_classic())+
  theme(plot.title = element_text(hjust = 0.5, size = 16))+
  theme(plot.caption = element_text(hjust = 1.75))+
  guides(size = "none")


![Activity level vs total distance traveled](https://github.com/Premnath1405/Bellabeat--Google-capstone-project/assets/128468794/cf7e3d5d-30f4-4ce8-b3ca-de88ffdb4301)

Key Insights:

For the most part, distance traveled progresses similarly as activity level increases.

Based on the total calories burned, we can determine that traveling a minimum of 5 miles puts users, on average, at a higher caloric burn rate.

SUMMARY OF KEY FINDINGS


User’s average daily step count is 7638. This is low compared to the CDC recommended step count of 10,000.

The total user very active and fairly active minutes was 34.72 or 0.59 hours while user’s lightly active and sedentary minutes was 1184 minutes or 19.73 hours.

Users are more active earlier on week days verse weekends.

We see that users are most active on Saturday, between 11:00am - 2:00pm, and Wednesday, between 5:00pm and 6:00pm.

There is a high correlation between activity level and the max amount of daily steps users take.

More active user burn more calories per step versus lower active users.


RECOMMENDATIONS


In order to encourage reaching the CDC recommendation of 10,000 steps per day, our app should have preset milestones for users every 2000 steps with a notification alerting them of how many more steps they need to reach 10,000.

Our app should have a process of alerting users if they have been sedentary for an extended period of time in one day.

For users who are detected being sedentary an extended period of time for multiple days, our app should prompt them to our Bellabeat membership subscription.

To enforce being active, our app should have a feature that lets users allow for notifications pushed to their phone, as well as their wellness watch device. This can be done every hour to encourage them to be active.

Our app should provide users with the ability to setup a sleep schedule, that can then create notifications for when it is time to sleep.

We should give our users the ability to setup a plan or schedule to be active and also have preset active schedules users can select from, based on their needs.

