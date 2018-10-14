## ---- load
library(ggmap)
library(forcats)
library(lubridate)
library(tidyverse)
library(sugrrants)
library(showtext)
library(tsibble)

# loading pedestrian data
pedestrian_2016 <- read_rds("data/pedestrian-2016.rds")

# selected sensors
sensors <- c(
  "State Library",
  "Flagstaff Station",
  "Flinders Street Station Underpass"
)

## ---- ped-map
# plotting the sensor locations using ggmap
ped_loc <- pedestrian_2016 %>% 
  distinct(Longitude, Latitude, Sensor_Name) %>% 
  mutate(
    Highlight = if_else(Sensor_Name %in% sensors, Sensor_Name, "Other"),
    Highlight = factor(Highlight, levels = c(sensors, "Other")),
    Selected = if_else(Sensor_Name %in% sensors, TRUE, FALSE)
  )
melb_map <- get_map(
  location = c(
    min(ped_loc$Longitude), min(ped_loc$Latitude),
    max(ped_loc$Longitude), max(ped_loc$Latitude)
  ),
  zoom = 14
)

selected <- ped_loc %>% 
  filter(Selected)
nonselected <- ped_loc %>% 
  filter(!Selected)
sensor_cols <- c(
  "State Library" = "#1b9e77", 
  "Flagstaff Station" = "#d95f02", 
  "Flinders Street Station Underpass" = "#7570b3",
  "Other" = "grey70"
) # Dark2
ggmap(melb_map) +
  geom_point(
    data = nonselected, aes(x = Longitude, y = Latitude, colour = Highlight),
    alpha = 0.8, size = 3
  ) +
  geom_point(
    data = selected, aes(x = Longitude, y = Latitude, colour = Highlight),
    size = 6
  ) +
  xlab("Longitude") +
  ylab("Latitude") +
  scale_colour_manual(
    name = "Sensor",
    breaks = names(sensor_cols),
    values = sensor_cols,
    guide = "legend"
  ) +
  theme(legend.position = "bottom")

## ---- time-series-plot
# subsetting the data
subdat <- pedestrian_2016 %>% 
  filter(Sensor_Name %in% sensors) %>% 
  mutate(Sensor_Name = fct_reorder(Sensor_Name, -Latitude))
# conventional time series plot
subdat %>% 
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
# time series plot faceted by sensors and day of week
subdat %>% 
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
# calendar plot for flinders street station
flinders <- subdat %>% 
  filter(Sensor_Name == "Flinders Street Station Underpass") %>% 
  mutate(
    Weekend = if_else(Day %in% c("Saturday", "Sunday"), "Weekend", "Weekday")
  )

flinders_cal <- flinders %>%
  frame_calendar(x = Time, y = Hourly_Counts, date = Date)

p_flinders <- flinders_cal %>% 
  ggplot(aes(x = .Time, y = .Hourly_Counts, group = Date, colour = Weekend)) +
  geom_line() +
  theme(legend.position = "bottom")
prettify(p_flinders)

## ---- flinders-free
# calendar plot for flinders street station using local scale
flinders_cal_free <- flinders %>% 
  frame_calendar(x = Time, y = Hourly_Counts, date = Date, scale = "free")

p_flinders_free <- flinders_cal_free %>% 
  ggplot(aes(x = .Time, y = .Hourly_Counts, group = Date, colour = Weekend)) +
  geom_line() +
  theme(legend.position = "bottom")
prettify(p_flinders_free)

## ---- flinders-polar
# calendar plot for flinders street station in polar coordinates
flinders_polar <- flinders %>% 
  frame_calendar(x = Time, y = Hourly_Counts, date = Date, polar = TRUE)

p_flinders_polar <- flinders_polar %>% 
  ggplot(aes(x = .Time, y = .Hourly_Counts, group = Date, colour = Weekend)) +
  geom_path() +
  theme(legend.position = "bottom")
prettify(p_flinders_polar)

## ---- overlay
# overlaying calendar plots 
subset_cal <- subdat %>% 
  frame_calendar(Time, Hourly_Counts, Date)

sensor_cols <- c(
  "#1b9e77" = "#1b9e77", 
  "#d95f02" = "#d95f02", 
  "#7570b3" = "#7570b3"
) # Dark2
p_three <- subset_cal %>% 
  ggplot() +
  geom_line(
    data = filter(subset_cal, Sensor_Name == sensors[1]),
    aes(.Time, .Hourly_Counts, group = Date, colour = sensor_cols[1])
  ) +
  geom_line(
    data = filter(subset_cal, Sensor_Name == sensors[2]),
    aes(.Time, .Hourly_Counts, group = Date, colour = sensor_cols[2])
  ) +
  geom_line(
    data = filter(subset_cal, Sensor_Name == sensors[3]),
    aes(.Time, .Hourly_Counts, group = Date, colour = sensor_cols[3])
  ) +
  scale_colour_identity(
    name = "Sensor",
    breaks = names(sensor_cols),
    labels = c(
      "State Library", 
      "Flagstaff Station",
      "Flinders Street Station Underpass"
    ),
    guide = "legend"
  ) +
  theme(legend.position = "bottom")
prettify(p_three)

## ---- facet
# calendar plots faceted by the sensors
facet_cal <- subdat %>% 
  group_by(Sensor_Name) %>% 
  frame_calendar(x = Time, y = Hourly_Counts, date = Date, nrow = 2)

p_facet <- facet_cal %>% 
  ggplot(aes(x = .Time, y = .Hourly_Counts, group = Date)) +
  geom_line(aes(colour = Sensor_Name)) +
  facet_grid(
    Sensor_Name ~ ., 
    labeller = labeller(Sensor_Name = label_wrap_gen(20))
  ) +
  scale_colour_brewer(
    palette = "Dark2", 
    guide = guide_legend(title = "Sensor")
  ) +
  theme(legend.position = "bottom")
prettify(p_facet, size = 3, label.padding = unit(0.18, "lines"))

## ---- scatterplot
# lagged scatterplot for flinders street station in the daily calendar format
flinders_cal_day <- flinders %>% 
  mutate(Lagged_Counts = dplyr::lag(Hourly_Counts)) %>% 
  frame_calendar(x = Lagged_Counts, y = Hourly_Counts, date = Date, 
    calendar = "daily", width = 0.95, height = 0.8)

p_flinders_day <- flinders_cal_day %>% 
  ggplot(
    aes(x = .Lagged_Counts, y = .Hourly_Counts, group = Date, colour = Weekend)
  ) +
  geom_point(size = 0.5, alpha = 0.6) +
  theme(legend.position = "bottom")
prettify(p_flinders_day, size = 3, label.padding = unit(0.15, "lines"))

## ---- boxplot
# boxplots for hourly counts across all the sensors in 2016 December
pedestrian_dec <- pedestrian_2016 %>% 
  filter(Month == "December") %>% 
  frame_calendar(
    x = Time, y = Hourly_Counts, date = Date, width = 0.97, height = 0.97
)
p_boxplot <- pedestrian_dec %>% 
  ggplot() +
  geom_boxplot(
    aes(x = .Time, y = .Hourly_Counts, group = Date_Time),
    outlier.size = 0.3, width = 0.004, position = "identity",
    colour = "grey50"
  ) +
  geom_smooth(
    aes(.Time, .Hourly_Counts, group = Date), 
    se = FALSE, method = "loess"
  )
prettify(p_boxplot, label = c("label", "text", "text2"))

## ---- chn
# boxplots for hourly counts across all the sensors in 2016 Dec with Chinese 
# labels
showtext.auto()
prettify(
  p_boxplot, locale = "zh", abbr = FALSE, 
  size = 3, label.padding = unit(0.15, "lines"),
  label = c("label", "text", "text2"),
  family = "wqy-microhei"
)
showtext.auto(FALSE) 

## ---- load-elec
elec <- read_rds("data/elec.rds")

## ---- sample-elec
elec %>% 
  group_by(id) %>% 
  slice(1:2)

## ---- dow
hol1718 <- holiday_aus(2017:2018, state = "VIC")

elec <- elec %>% 
  mutate(
    weekday = wday(date, label = TRUE, week_start = 1),
    workday = if_else(
      (date %in% hol1718$date) | weekday %in% c("Sat", "Sun"),
      "Non-work day", "Workday")
  )

elec %>% 
  group_by(date, weekday, id) %>% 
  summarise(kwh = sum(kwh, na.rm = TRUE)) %>% 
  ggplot(aes(x = weekday, y = kwh)) +
  lvplot::geom_lv(aes(fill = ..LV..), colour = "black", outlier.shape = 8) +
  facet_wrap(~ id, labeller = label_both) +
  xlab("Day of week") +
  ylab("kWh") +
  scale_fill_brewer(palette = "Blues", direction = -1) +
  theme(legend.position = "bottom")

## ---- hod
avg_elec <- elec %>% 
  group_by(id, workday, time) %>% 
  summarise(avg = mean(kwh))

ggplot(elec, aes(x = time, y = kwh)) +
  geom_point(alpha = 0.5, size = 0.1) +
  geom_line(aes(y = avg, colour = as.factor(id)), data = avg_elec, size = 1) +
  facet_grid(workday ~ id) +
  scale_colour_brewer(palette = "Dark2") +
  scale_x_time(breaks = hms::hms(hours = c(6, 18))) +
  xlab("Time of day") +
  ylab("kWh") +
  guides(colour = "none")

## ---- household23
hh <- elec %>% 
  filter(id %in% c(1, 3))

hh_cal <- hh %>% 
  group_by(id) %>% 
  frame_calendar(x = time, y = kwh, date = date)

p_hh <- hh_cal %>% 
  ggplot(aes(x = .time, y = .kwh, group = date)) +
  geom_line(aes(colour = workday)) +
  facet_grid(id ~ ., label = label_both) +
  theme(legend.position = "bottom")
prettify(p_hh)
