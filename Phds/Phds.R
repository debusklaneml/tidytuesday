# Tidy Tuesday - PhDs

library(tidyverse)
library(janitor)
library(readxl)
library(skimr)

phd_field <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-02-19/phd_by_field.csv")

#get the gist
skim(phd_field)
Hmisc::describe(phd_field)

ed <- phd_field %>%
  filter(broad_field == "Education")

#filter to research field and clean up names a bit
ed_research_rank <- ed %>%
  filter(major_field == "Education research") %>%
  select(-broad_field, -major_field) %>%
  spread(field, n_phds) %>% #just to rename
  clean_names() %>%
  rename(Counseling = starts_with("Counseling"),
         'Curriculum and Inst' = starts_with("curriculum"),
         'Ed Inst Media Design' = starts_with("educational_and_instructional_media_design"),
         'Instructional Tech' = starts_with("educational_and_instructional_technology"),
         'Measurement' = starts_with("educational_assessment"),
         'Policy Analysis' = starts_with("educational_policy"),
         'Ed Psych' = starts_with("educational_psy"),
         'Ed Stat Research Meth' = starts_with("educational_statistics"),
         'Ed Eval & Research' = starts_with("higher_education_eval"),
         'Intern Ed' = starts_with("international"),
         'School Psych' = starts_with("school_psychology"),
         'Foundations of Ed' = starts_with("social_and_phil"),
         'Special Ed' = starts_with("special_ed")) %>%
  gather('Counseling':'Special Ed', key = field, value = n_phds) %>%
  group_by(year, field) %>%
  summarise(tot_phds = sum(na.omit(n_phds))) %>%
  arrange(year, desc(tot_phds)) %>%
  mutate(ranking = row_number())

down_field <- c("Ed Inst Media Design", "Foundations of Ed", "Measurement")
up_field <- c("Policy Analysis", "Instructional Tech")

ed_research_rank <- ed_research_rank %>%
  mutate(swing = ifelse(field %in% down_field, "Down", ifelse(field %in% up_field, "Up", ifelse(field == "Ed Psych", "EdPsych", "None"))))





#Bump Chart - derived from code provided by @parkermquinn 
bump <- ed_research_rank %>%
  ggplot(aes(year, ranking, group = field)) + 
  geom_line(aes(color = swing, alpha = .9), size = 2) + 
  geom_point(aes(color = swing, alpha = .9, size = tot_phds)) + 
  geom_point(color = "#FFFFFF", size = .6) + 
  scale_y_reverse(breaks = c(1,2,3,4,5,6,7,8,9,10,11,12,13)) + 
  my_theme() +
  scale_x_continuous(breaks = 2008:2017, minor_breaks = 2008:2017, limits = c(2007, 2018)) + 
  geom_text(data = ed_research_rank %>% filter(year == 2008),
            aes(label = field, x = 2007.8) , hjust = 1, fontface = "bold", color = "#888888", size = 3) +
  geom_text(data = ed_research_rank %>% filter(year == 2017),
            aes(label = field, x = 2017.2) , hjust = 0, fontface = "bold", color = "#888888", size = 3) + 
  scale_color_manual(values = c(Down = "#D8B365", Up = "#5AB4AC", None = "gray", EdPsych = "#FA8072")) + 
  labs(x = "Year", y = "Popularity rank", title = "Educational Research PhDs awarded by field")

bump
  
#Also stole this from Parker.
my_theme <- function() {
  
  # Colors
  color.background = "#f5f5f2"
  color.text = "#22211d"
  
  # Begin construction of chart
  theme_bw(base_size=15) +
    
    # Format background colors
    theme(panel.background = element_rect(fill=color.background, color=color.background)) +
    theme(plot.background  = element_rect(fill=color.background, color=color.background)) +
    theme(panel.border     = element_rect(color=color.background)) +
    theme(strip.background = element_rect(fill=color.background, color=color.background)) +
    
    # Format the grid
    theme(panel.grid.major.y = element_blank()) +
    theme(panel.grid.minor.y = element_blank()) +
    theme(axis.ticks       = element_blank()) +
    
    # Format the legend
    theme(legend.position = "none") +
    
    # Format title and axis labels
    theme(plot.title       = element_text(color=color.text, size=20, face = "bold.italic")) +
    theme(plot.subtitle    = element_text(color=color.text, size=12, face = "italic")) + 
    theme(axis.title.x     = element_text(size=14, color="black", face = "bold")) +
    theme(axis.title.y     = element_text(size=14, color="black", face = "bold", vjust=1.25)) +
    theme(axis.text.x      = element_text(size=10, vjust=0.5, hjust=0.5, color = color.text)) +
    theme(axis.text.y      = element_text(size=10, color = color.text)) +
    theme(strip.text       = element_text(face = "bold")) +
    
    # Plot margins
    theme(plot.margin = unit(c(0.35, 0.2, 0.3, 0.35), "cm"))
}  
  
  
  












            