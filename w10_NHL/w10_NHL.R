library(tidyverse)
library(janitor)
library(readxl)
library(tidylog)
library(ggdark)
library(ggforce)
library(rvest)
library(glue)
library(patchwork)
library(stringi)

# Read in the data. 
game_goals <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-03-03/game_goals.csv')
top_250 <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-03-03/top_250.csv')
season_goals <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-03-03/season_goals.csv')

# Take a stab at recreating #jkaupp's work, but looking at what happend between about 1980 and 2003. 

url <- "https://www.hockey-reference.com/leagues/stats.html"

total_scoring <- read_html(url) %>% 
  html_node("#stats") %>% 
  html_table() %>% 
  filter(!str_detect(Rk, "[A-Z]")) %>% 
  mutate_at(vars(GP:GAA), as.numeric) %>% 
  mutate(year = as.numeric(str_sub(Season, 1, 4)))

ppm_data <- season_goals %>% 
  group_by(player, season) %>% 
  summarize(goals = sum(goals, na.rm = TRUE),
            assists = sum(assists, na.rm = TRUE)) %>% 
  group_by(season) %>% 
  mutate(min_goals = min(goals, na.rm = TRUE),
         max_goals = max(goals, na.rm = TRUE)) %>% 
  ungroup() %>% 
  mutate(year = as.numeric(str_sub(season, 1, 4))) %>% 
  arrange(year) %>% 
  filter(between(year, 1980, 2003))

top_2 <- ppm_data %>% 
  filter(goals == max_goals) %>% 
  count(player) %>% 
  ungroup() %>% 
  top_n(2, n) %>% 
  arrange(desc(n)) %>% 
  mutate(color = case_when(str_detect(player, "Gretzky") ~ "#fc4903",
                           str_detect(player, "Mike Gartner") ~ "#FFB81C",
                           TRUE ~ NA_character_),
         alpha = 1,
         size = 1)

bkg_rect <- ppm_data %>% 
  distinct(year, season, min_goals, max_goals) %>% 
  left_join(total_scoring)

plot_data <- ppm_data %>% 
  left_join(top_2) %>% 
  replace_na(list(color = "#363636",
                  alpha = 0.2,
                  size = 0.5))

annotations <- ppm_data %>% 
  filter(goals == max_goals) %>% 
  semi_join(top_2) %>% 
  group_by(player) %>% 
  filter(goals == max(max_goals)) %>% 
  ungroup() %>% 
  mutate(xend = c(1952, 1970),     # Not sure what these are for?
         yend = c(70, 90)) %>% 
  mutate(color = case_when(str_detect(player, "Gretzky") ~ "#fc4903",
                           str_detect(player, "Mike Gartner") ~ "#FFB81C",
                           TRUE ~ NA_character_))

avg_scoring <- ggplot(bkg_rect, aes(x = year)) +
  geom_tile(aes(y = 0, fill = G, height = 1, width = .9),  color = "white") +
  labs(x = NULL,
       y = NULL) +
  coord_equal() +
  scale_x_continuous(limits = c(1980, 2003), expand = c(0,0), breaks = seq(1985, 2004, 5), position = "top") +
  scale_fill_gradient("Average Goals per game, for comparative puporses and\nto account for the pace of the game in different eras.",  low = "#e5e5e5", high = "#DC143C") +
  #theme_jk(grid = FALSE) +
  theme(axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        plot.margin = margin(0, 0, 0, 0),
        plot.background = element_rect(fill = "white", colour = 'white'),
        panel.background = element_rect(fill = 'white'),
        panel.grid.major = element_line(colour = "white"),
        legend.position = "bottom", #c(0.5, 0),
        legend.direction = "horizontal",
        legend.key.height = unit(2.5, "mm")) 

avg_scoring

goal_leaders <- ggplot(bkg_rect, aes(x = year)) +
  geom_rect(aes(xmin = year - 0.3, xmax = year + 0.3, ymin = min_goals, ymax = max_goals), fill = "#e5e5e5", color = "#e5e5e5") +
  #geom_rect(aes(xmin = 2004 - 0.3, xmax = 2004 + 0.3, ymin = 0, ymax = 60), fill = "#FFFFFF", color = "#363636", size = 0.5) +
  geom_segment(data = plot_data, aes(x = year - 0.3, xend = year + 0.3, y = goals, yend = goals, color = color, alpha = alpha, size = size)) +
  geom_mark_circle(data = annotations, aes(y = goals, group = player, filter = str_detect(player, "Gretzky"), label = glue("{year} Goal Leader"), description = glue("{player}: {goals} goals")), label.family = "American Typewriter Light", expand = unit(7, "mm"), label.colour = c("black", "#fc4903"), label.fontface = c("plain", "bold")) +
  geom_mark_circle(data = annotations, aes(y = goals, group = player, filter = str_detect(player, "Gartner"), label = glue("{year} Goal Leader"), description = glue("{player}: {goals} goals")), label.family = "American Typewriter Light", expand = unit(7, "mm"), label.colour = c("black", "#FFB81C"), label.fontface = c("plain", "bold")) + 
  scale_x_continuous(limits = c(1980, 2003), expand = c(0,0), breaks = seq(1985, 2003, 5)) +
  scale_y_continuous(limits = c(0, 120), breaks = seq(0, 120, 20)) +
  labs(x = NULL, 
       y = NULL) +
  scale_color_identity() +
  scale_size_identity() +
  scale_alpha_identity() +
  theme_bw() +
  #theme_jk(grid = "Y", markdown = TRUE) +
  theme(plot.margin = margin(0, 0, 0, 0),
        axis.text.y = element_blank(),
        plot.background = element_rect(fill = "#fbfcfc", colour = NA),
        plot.title = element_text(family = "American Typewriter Light"))

goal_leaders
stringx <- "Both Gretzky and Gartner have recorded the highest scoring seasong between 1980 and 2003. Each line in the bar represents the goals scored by one of the top 250 goal scorers in the NHL."
subtitle <- (str_wrap(stringx, width = 130))


out <- wrap_plots(avg_scoring, goal_leaders, ncol = 1, heights = c(0.05, 0.95), widths = c(1, 1)) +
  plot_annotation(title = "The Three Way Tie for the Record of Most Seasons Leading Goal-Scoring in the NHL",
                  subtitle = subtitle,
                  caption = "**Data**: hockey-reference.com | **Graphic**: @mldebusklane | *Idea*: @jakekaupp") &
  theme(text = element_text(family = "American Typewriter Light", face = "bold"))

out


