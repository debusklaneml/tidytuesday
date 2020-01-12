# Week 2 Tidy Tuesday: Austrailia Wild Fires

library(tidyverse)
library(tidylog)
library(ggdark)
library(ggforce)

# Get the Data
rainfall <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-01-07/rainfall.csv')
temperature <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-01-07/temperature.csv')

temp_1 <- temperature %>%
  mutate(
    year = lubridate::year(date), 
    month = lubridate::month(date),
    day = lubridate::day(date)) %>%
  select(-site_name) %>%
  pivot_wider(names_from = "temp_type", values_from = "temperature") %>%
  filter(!is.na(max),
         !is.na(min)) %>%
  mutate(city_name = str_to_lower(city_name))

rain_1 <- rainfall %>%
  select(-station_code, -lat, -long, -station_name, -period, -quality) %>%
  mutate(year = as.numeric(year),
         month = as.numeric(month),
         day = as.numeric(day)) %>%
  mutate(city_name = str_to_lower(city_name))

temp_rain <- left_join(temp_1, rain_1, by = c("city_name", "year", "month", "day")) %>%
  mutate(rainfall = if_else(is.na(rainfall), 0, rainfall)) %>%
  mutate(avg.temp = (max+min)/2,
         rain = if_else(rainfall > 0, 1, 0))

yearly <- temp_rain %>%
  filter(year < 2019) %>%
  group_by(year) %>%
  summarise_each(funs(mean,sd,se=sd(.)/sqrt(n()))) %>%
  rename(rain = rainfall_mean,
         temp = avg.temp_mean,
         se = avg.temp_se) %>%
  select(year, rain, temp, se) %>%
  mutate(u_se = temp + (1.96 * se),
         l_se = temp - (1.96 * se))

temp_dark <- ggplot(yearly, aes(x = year, y = temp)) +
  geom_errorbar(aes(ymin=l_se, ymax=u_se), color = "#726A6A", alpha = .2) +
  geom_point(aes(size = rain, color = temp)) +
  geom_path() +
  geom_mark_circle(aes(filter = year == 2014, label = str_wrap("Year: 2014: 18.25C Highest Average Temp")), 
                   color = "orange", 
                   label.family = "American Typewriter Light",
                   label.fill = "grey10",
                   label.colour = "white",
                   label.fontsize = 15,
                   label.buffer = unit(20, "mm"),
                   con.colour = "grey") +
  geom_mark_circle(aes(filter = year == 1974, label = str_wrap("Year: 1974: 2.22mm Highest Rainfall")), 
                   color = "orange", 
                   label.family = "American Typewriter Light",
                   label.fill = "grey10",
                   label.colour = "white",
                   label.fontsize = 15,
                   label.buffer = unit(50, "mm"),
                   con.colour = "grey") +
  labs(title = "Average Yearly Temperature and Rain Amount",
       x = "Year (1910-2018)",
       y = "Average Temperature ((Max - Min)/2)") +
  scale_y_continuous(label = label_number_si(unit = "° C", sep = ""))

temp_dark + dark_theme_gray(base_family = "American Typewriter Light") + 
  theme(plot.title = element_text(family = "American Typewriter Light"),
        plot.background = element_rect(fill = "grey10"),
        panel.background = element_blank(),
        panel.grid.major = element_line(color = "grey30", size = 0.2),
        panel.grid.minor = element_line(color = "grey30", size = 0.2),
        legend.background = element_blank(),
        axis.ticks = element_blank(),
        legend.key = element_blank(),
        legend.position = c(0.85, 0.22)) 
  

