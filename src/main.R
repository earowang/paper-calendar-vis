## ---- load
library(ggmap)
library(forcats)
library(lubridate)
library(tidyverse)
library(sugrrants)
library(showtext)

pedestrian_2016 <- read_rds("data/pedestrian-2016.rds")

## ---- ped-map
ped_loc <- pedestrian_2016 %>% 
  distinct(Longitude, Latitude)
melb_map <- get_map(
  location = c(
    min(ped_loc$Longitude), min(ped_loc$Latitude),
    max(ped_loc$Longitude), max(ped_loc$Latitude)
  ),
  zoom = 14
)

ggmap(melb_map) +
  geom_point(data = ped_loc, aes(x = Longitude, y = Latitude),
    colour = "#756bb1", alpha = 0.8, size = 3) +
  xlab("Longitude") +
  ylab("Latitude")

## ---- time-series-plot
sensors <- c(
  "State Library",
  "Flagstaff Station",
  "Flinders Street Station Underpass"
)
subdat <- pedestrian_2016 %>% 
  filter(Sensor_Name %in% sensors) %>% 
  mutate(Sensor_Name = fct_reorder(Sensor_Name, -Latitude))
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
flinders <- subdat %>% 
  filter(Sensor_Name == "Flinders Street Station Underpass")

flinders_cal <- flinders %>%
  frame_calendar(x = Time, y = Hourly_Counts, date = Date)

p_flinders <- flinders_cal %>% 
  ggplot(aes(x = .Time, y = .Hourly_Counts, group = Date)) +
  geom_line()
prettify(p_flinders, size = 3, label.padding = unit(0.15, "lines"))

## ---- flinders-free
flinders_cal_free <- flinders %>% 
  frame_calendar(x = Time, y = Hourly_Counts, date = Date, scale = "free")

p_flinders_free <- flinders_cal_free %>% 
  ggplot(aes(x = .Time, y = .Hourly_Counts, group = Date)) +
  geom_line()
prettify(p_flinders_free, size = 3, label.padding = unit(0.15, "lines"))

## ---- flinders-polar
flinders_polar <- flinders %>% 
  frame_calendar(x = Time, y = Hourly_Counts, date = Date, polar = TRUE)

p_flinders_polar <- flinders_polar %>% 
  ggplot(aes(x = .Time, y = .Hourly_Counts, group = Date)) +
  geom_path()
prettify(p_flinders_polar, size = 3, label.padding = unit(0.15, "lines"))

## ---- overlay
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
prettify(p_three, size = 3, label.padding = unit(0.15, "lines"))

## ---- facet
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
prettify(p_facet, label = NULL)

## ---- scatterplot
flinders_cal_day <- flinders %>% 
  mutate(Lagged_Counts = dplyr::lag(Hourly_Counts)) %>% 
  frame_calendar(x = Hourly_Counts, y = Lagged_Counts, date = Date, 
    calendar = "daily", width = 0.95, height = 0.8)

p_flinders_day <- flinders_cal_day %>% 
  ggplot(aes(x = .Hourly_Counts, y = .Lagged_Counts, group = Date)) +
  geom_point(size = 0.5, alpha = 0.6)
prettify(p_flinders_day, size = 3, label.padding = unit(0.15, "lines"))

## ---- boxplot
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
showtext.auto()
prettify(
  p_boxplot, locale = "zh", abbr = FALSE, 
  size = 3, label.padding = unit(0.15, "lines"),
  label = c("label", "text", "text2")
)
showtext.auto(FALSE) 
