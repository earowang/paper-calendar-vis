library(lubridate)
library(tidyverse)
library(sugrrants)

ped_full <- read_csv("data/Pedestrian_traffic_-_hourly_count.csv")

pedestrian_2014 <- ped_full %>% 
  filter(Year >= 2014) %>% 
  select(-ID) %>% 
  mutate(
    Date_Time = dmy_hm(Date_Time, tz = "Australia/Brisbane"),
    Date = as_date(Date_Time),
    Month = month(Date_Time, abbr = FALSE, label = TRUE),
    Day = wday2(Date_Time, abbr = FALSE, label = TRUE)
  ) %>% 
  arrange(Sensor_ID, Date_Time)
# remove duplicates
pedestrian_2014 <- pedestrian_2014[!duplicated(pedestrian_2014), ]

write_rds(pedestrian_2014, path = "data/pedestrian-2014.rds")
