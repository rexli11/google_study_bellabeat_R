library(tidyverse)
library(dplyr)
library(tidyr)
library(here)
library(skimr)
library(janitor)
library(lubridate)


activity <- read.csv('dailyActivity_merged.csv')# 活動合併數據
mets <- read.csv('minuteMETsNarrow_merged.csv')# 分鐘代謝當量
rate <- read.csv('heartrate_seconds_merged.csv')# 心跳頻率
bmi <- read.csv('weightLogInfo_merged.csv')# 權重日誌訊息
sleep <- read.csv('sleepDay_merged.csv')# 每日睡眠


unique(activity$Id)# 33個
unique(mets$Id)# 33個
unique(rate$Id)# 14個
unique(bmi$Id)# 8個
unique(sleep$Id)# 24個


str(activity)
str(mets)
str(rate)
str(bmi)
str(sleep)


sum(is.na(mets))
sum(is.na(activity))
sum(is.na(rate))
sum(is.na(bmi)) #TURE >> 65na
sum(is.na(sleep))


bmi <- 
  mutate(bmi, height_m = sqrt(WeightKg/BMI)) %>% 
  mutate(bmi, height_m = round(height_m, digits = 2)) %>% 
  mutate(bmi, BMI = round(BMI, digits = 1))

bmi <-  
  rename(bmi, date = Date) %>% 
  separate(date, into = c("date", "Time"), sep = " ") %>% 
  clean_names()

bmi <- 
  select(bmi, id, date, weight_kg, bmi, height_m)


activity <- 
  rename(activity, date = ActivityDate) %>% 
  separate(date, into = c("date"), sep = " ") %>% 
  clean_names()

activity <- 
  select(activity, 
         -sedentary_active_distance, 
         -logged_activities_distance)

mets <- 
  rename(mets, date = ActivityMinute) %>% 
  separate(date, into = c("date", "time"), sep = " ") %>% 
  clean_names()

mets <- 
  mutate(mets, mets = (me_ts / 10))

mets <- 
  select(mets, -time, -me_ts) %>% 
  group_by(id, date) %>% 
  summarise(mets = sum(mets))

rate <- 
  rename(rate, date = Time) %>% 
  separate(date, into = c("date", "time"), sep = " ") %>% 
  clean_names()

rate <- 
  select(rate, -time)

rate <- 
  group_by(rate, id, date) %>% 
  summarise_all(list(sum, mean))

sleep <- 
  rename(sleep, date = SleepDay, 
         total_sleep_hour = TotalMinutesAsleep) %>% 
  separate(date, into = c("date", "time"), sep = " ") %>% 
  clean_names()

sleep <- select(sleep, -time)

sleep <- 
  mutate(sleep, sleep_hour = (total_sleep_hour / 60), 
         time_in_bed = (total_time_in_bed / 60)) %>% 
  mutate(sleep, sleep_hour = round(sleep_hour, digits = 2), 
         time_in_bed = round(time_in_bed, digits =2))



activity %>% 
  summary()
mets %>% 
  summary()
rate %>% 
  summary()
bmi %>% 
  summary()
sleep %>% 
  summary()



library(ggplot2)
bmi_sleep <- merge(bmi, sleep, by = "id")

ggplot(data = sleep, mapping = aes(x = date, y = sleep_hour )) +
  geom_point(aes(color = date)) +
  theme(axis.text.x = element_text(angle = 60)) + 
  xlab("Date") + 
  ylab("Sleep Hour") + 
  labs(title = "Sleep & Day", 
       subtitle = "Daily Sleep Time Distribution", 
       fill = "Date")


ggplot(data = activity) + 
  geom_point(mapping = aes(x = total_steps, 
                           y = calories, 
                           color = date)) + 
  geom_smooth(aes(x = total_steps, 
                  y = calories)) +
  xlab("Total Steps") + 
  ylab("Calories") + 
  labs(title = "Step & Calories", 
       subtitle = "Do More Steps Burn More Calories?", 
       fill = "Date")


ggplot(data = activity) + 
  geom_point(mapping = aes(x = lightly_active_minutes, 
                           y = very_active_minutes, 
                           color = calories,
                           size = calories)) +
  geom_smooth((aes(x = lightly_active_minutes, 
                   y = very_active_minutes))) + 
  xlab("Lightly Active Min") + 
  ylab("Hightly Active Min") + 
  labs(title = "Lightly & Heightly Active", 
       subtitle = "Differences In Calorie Consumption")


ggplot(data = bmi_sleep) + 
  geom_boxplot(mapping = aes(x = as.factor(bmi), 
                             y = sleep_hour, 
                             fill = as.factor(bmi))) + 
  theme(axis.text.x = element_text(angle = 0)) + 
  xlab("BMI") + 
  ylab("Sleep Hour") + 
  labs(title = "sleep time", 
       subtitle = "sleep time", 
       fill = "BMI")










