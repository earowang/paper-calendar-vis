## ---- load
library(ggcal)
library(ggTimeSeries)
library(lubridate)
library(tidyverse)
library(sugrrants)

pedestrian_2014 <- read_rds("data/pedestrian-2014.rds")

## ---- multi-years
p <- pedestrian_2014 %>% 
  filter(Sensor_Name == "Flagstaff Station") %>% 
  frame_calendar(Time, Hourly_Counts, Date, ncol = 12) %>% 
  ggplot(aes(.Time, .Hourly_Counts, group = Date)) +
  geom_line()
prettify(p)
