library(viridis)
library(forcats)
library(geofacet)
library(sugrrants)
library(tidyverse)
library(lubridate)

pm25 <- read_rds("data/chn_pm25.rds") %>% 
  rename(Date_Time = Date) %>% 
  mutate(Date = as_date(Date_Time)) %>% 
  filter(!is.na(Value))
pm25_2016 <- pm25 %>% 
  filter(Year == 2016)
pm25_2016_cal <- pm25_2016 %>% 
  group_by(Site) %>% 
  frame_calendar(Hour, "Value", Date)

pm25_2016_cal %>% 
  ggplot(aes(x = .Hour, y = .Value, group = Date)) +
  geom_line() +
  facet_wrap(~ Site) +
  theme_void()

pm25_2016_wday <- pm25_2016 %>% 
  group_by(Site) %>% 
  frame_calendar(Hour, "Value", Date, scale = "free_wday")

pm25_2016_wday %>% 
  ggplot(aes(x = .Hour, y = .Value, group = Date)) +
  geom_line() +
  facet_wrap(~ Site) +
  theme_void()

pm25_2016_mday <- pm25_2016 %>% 
  group_by(Site) %>% 
  frame_calendar(Hour, "Value", Date, scale = "free_mday")

pm25_2016_mday %>% 
  ggplot(aes(x = .Hour, y = .Value, group = Date)) +
  geom_line() +
  facet_wrap(~ Site) +
  theme_void()

pm25_day_max <- pm25_2016 %>% 
  group_by(Date, Site) %>% 
  summarise(Max = max(Value))

pm25_index <- fct_inorder(c("Good", "Moderate", 
  "Unhealthy for Sensitive Groups",
  "Unhealthy", "Very Unhealthy", "Hazardous"), order = TRUE)
pm25_day_label <- pm25_day_max %>% 
  mutate(Level = case_when(
    Max < 51 ~ pm25_index[1],
    Max < 101 ~ pm25_index[2],
    Max < 151 ~ pm25_index[3],
    Max < 201 ~ pm25_index[4],
    Max < 301 ~ pm25_index[5],
    TRUE ~ pm25_index[6]
  ))

site_grid <- tribble(
  ~ row, ~ col, ~ code, ~ name,
  1, 3, "Beijing", "Beijing",
  3, 1, "Chengdu", "Chengdu",
  4, 2, "Guangzhou", "Guangzhou",
  3, 3, "Shanghai", "Shanghai",
  1, 4, "Shenyang", "Shenyang"
)

pm25_label_cal <- pm25_day_label %>% 
  mutate(
    Day = 1,
    Value = 1
  ) %>% 
  group_by(Site) %>% 
  frame_calendar(Day, "Value", Date) # could pass a single integer as "identity"

pm25_label_cal %>% 
  ggplot(aes(.Day, .Value, fill = Level, colour = "grey50")) +
  geom_tile() +
  facet_geo(~ Site, grid = site_grid) +
  scale_fill_viridis(discrete = TRUE)

pm25_label_day <- pm25_day_label %>% 
  mutate(
    Day = 1,
    Value = 1
  ) %>% 
  group_by(Site) %>% 
  frame_calendar(Day, "Value", date = Date, calendar = "daily")
# undebug(sugrrants:::frame_calendar.default)

pm25_label_day %>% 
  ggplot(aes(.Day, .Value, fill = Level, colour = "grey50")) +
  geom_tile() +
  facet_geo(~ Site, grid = site_grid) +
  scale_fill_viridis(discrete = TRUE)

pm25_label_week <- pm25_day_label %>% 
  ungroup() %>% 
  mutate(
    Day = 1,
    Value = 1
  ) %>% 
  frame_calendar(Day, "Value", date = Date, calendar = "weekly")
# undebug(sugrrants:::frame_calendar.default)

pm25_label_week %>% 
  ggplot(aes(.Day, .Value, fill = Level, colour = "grey50")) +
  geom_tile() +
  facet_geo(~ Site, grid = site_grid) +
  scale_fill_viridis(discrete = TRUE)
