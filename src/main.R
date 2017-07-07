## ---- load
library(lubridate)
library(tidyverse)
library(sugrrants)

pedestrian_2014 <- read_rds("data/pedestrian-2014.rds")

## ---- time-series-plot
two_sensors <- c(
  "Flagstaff Station",
  "Flinders Street Station Underpass"
)
subset_df <- pedestrian_2014 %>% 
  filter(
    Sensor_Name %in% two_sensors,
    Year < 2017
)

subset_df_2016 <- subset_df %>% 
  filter(Year == 2016)

subset_df_2016 %>% 
  ggplot(aes(x = Date_Time, y = Hourly_Counts, colour = Sensor_Name)) +
  geom_line(size = 0.3) +
  facet_grid(
    Sensor_Name ~ ., 
    labeller = labeller(Sensor_Name = label_wrap_gen(20))
  ) +
  scale_colour_brewer(
    palette = "Dark2", 
    guide = guide_legend(title = "Sensor")
  ) +
  theme(legend.position = "bottom") +
  xlab("Date Time") +
  ylab("Hourly Counts")

## ---- facet-time
subset_df_2016 %>% 
  ggplot(aes(x = Time, y = Hourly_Counts, group = Date, colour = Sensor_Name)) +
  geom_line(size = 0.3) +
  facet_grid(
    Sensor_Name ~ Day, 
    labeller = labeller(Sensor_Name = label_wrap_gen(20))
  ) +
  scale_x_continuous(breaks = seq(6, 23, by = 6)) +
  scale_colour_brewer(
    palette = "Dark2", 
    guide = guide_legend(title = "Sensor")
  ) +
  theme(legend.position = "bottom") +
  xlab("Time") +
  ylab("Hourly Counts")

## ---- flinders-2016
flinders_2016 <- subset_df_2016 %>% 
  filter(Sensor_Name == "Flinders Street Station Underpass")

flinders_cal_2016 <- flinders_2016 %>% 
  frame_calendar(x = Time, y = Hourly_Counts, date = Date)

p_flinders <- flinders_cal_2016 %>% 
  ggplot(aes(x = .Time, y = .Hourly_Counts, group = Date)) +
  geom_line(colour = "#d95f02")
prettify(p_flinders, size = 3, label.padding = unit(0.15, "lines"))

## ---- flinders-free
flinders_cal_free <- flinders_2016 %>% 
  frame_calendar(x = Time, y = Hourly_Counts, date = Date, scale = "free")

p_flinders_free <- flinders_cal_free %>% 
  ggplot(aes(x = .Time, y = .Hourly_Counts, group = Date)) +
  geom_line(colour = "#d95f02")
prettify(p_flinders_free, size = 3, label.padding = unit(0.15, "lines"))

## ---- scatterplot
flinders_cal_day <- flinders_2016 %>% 
  mutate(Lagged_Counts = dplyr::lag(Hourly_Counts)) %>% 
  frame_calendar(x = Hourly_Counts, y = Lagged_Counts, date = Date, 
    calendar = "daily")

p_flinders_day <- flinders_cal_day %>% 
  ggplot(aes(x = .Hourly_Counts, y = .Lagged_Counts, group = Date)) +
  geom_point(colour = "#d95f02", size = 0.5)
prettify(p_flinders_day, size = 3, label.padding = unit(0.15, "lines"))

## ---- overlay
subset_cal <- subset_df_2016 %>% 
  frame_calendar(Time, Hourly_Counts, Date)

p_two <- subset_cal %>% 
  ggplot() +
  geom_line(
    data = filter(subset_cal, Sensor_Name == two_sensors[1]),
    aes(.Time, .Hourly_Counts, group = Date),
    colour = "#1b9e77", alpha = 0.8
  ) +
  geom_line(
    data = filter(subset_cal, Sensor_Name == two_sensors[2]),
    aes(.Time, .Hourly_Counts, group = Date),
    colour = "#d95f02", alpha = 0.8
  )
prettify(p_two, size = 3, label.padding = unit(0.15, "lines"))

## ---- boxplot
pedestrian_2016 <- pedestrian_2014 %>% 
  filter(Year == 2016, Month == "December") %>% 
  frame_calendar(x = Time, y = Hourly_Counts, date = Date)
dat <- mutate(group_by(pedestrian_2016, Date_Time), Mean = mean(.Hourly_Counts))
p_boxplot <- pedestrian_2016 %>% 
  ggplot() +
  geom_boxplot(
    aes(x = .Time, y = .Hourly_Counts, group = Date_Time),
    outlier.size = 0.3, width = 0.004, position = "identity",
    colour = "grey50"
  ) +
  geom_line(
    data = dat,
    aes(x = .Time, y = Mean, group = Date),
    colour = "#d95f02", size = 1,
  )
prettify(p_boxplot)

## END

## ---- rect
# two_sensors_wide <- subset_df_2016 %>% 
#   select(-Sensor_ID) %>% 
#   spread(key = Sensor_Name, value = Hourly_Counts) %>% 
#   rename(
#     Flinders = `Flinders Street Station Underpass`,
#     Flagstaff = `Flagstaff Station`
#   ) %>% 
#   mutate(
#     Diff = Flinders - Flagstaff,
#     More = if_else(Diff > 0, "Flinders Street Station Underpass", 
#       "Flagstaff Station")
#   )
# sensors_wide_calendar <- two_sensors_wide %>% 
#   frame_calendar(x = Time, y = vars(Flinders, Flagstaff), date = Date)
# p_rect <- sensors_wide_calendar %>%
#   ggplot() +
#   geom_rect(aes(xmin = .Time, xmax = .Time + 0.003,
#     ymin = .Flagstaff, ymax = .Flinders, fill = More
#   )) +
#   scale_fill_brewer(palette = "Dark2") +
#   theme(legend.position = "bottom")
# prettify(p_rect)

## ---- flinders-years
# flinders_1416 <- pedestrian_2014 %>% 
#   filter(
#     Year < 2017,
#     Sensor_Name == "Flinders Street Station Underpass"
#   ) %>%  
#   frame_calendar(x = Time, y = Hourly_Counts, date = Date, nrow = 3)
# p_full <- flinders_1416 %>% 
#   ggplot(aes(x = .Time, y = .Hourly_Counts, group = Date)) +
#   geom_line(colour = "#d95f02")
# prettify(p_full, size = 3, label.padding = unit(0.15, "lines"))
#
