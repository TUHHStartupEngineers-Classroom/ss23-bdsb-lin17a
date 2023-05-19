library(tidyverse)
library(ggplot2)
library(ggdark)
library(ggthemes)


covid_data_tbl <- read_csv("https://covid.ourworldindata.org/data/owid-covid-data.csv")


covid_data_tbl %>% 
  # filter countries
  filter(location %in% c("Germany", "United Kingdom", "France", "Spain", "United States")) %>%
  
  # fill NA with zeros
  mutate_at("total_cases", ~replace_na(.,0)) %>%
  
  # plot total cases per country
  ggplot(aes(x = date, y = total_cases, color = location)) +
  geom_line(linewidth = 0.5) + 
  
  # set theme
  dark_theme_dark()+ 
  theme(panel.background = element_rect(fill='transparent'),
        plot.background = element_rect(fill='transparent', color=NA),
        legend.background = element_rect(fill='transparent'),
        legend.box.background = element_rect(linewidth=0, fill='transparent')) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme(legend.position="bottom",
        legend.key=element_blank()) +
  
  # set scale
  scale_x_date(date_labels = "%B '%y", date_breaks = "month", minor_breaks = NULL) + 
  scale_y_continuous(labels = scales::label_number(scale = 0.000001, suffix = " M")) + 
  
  # set legend
  labs(
    title = "COVID-19 confirmed cases worldwide",
    subtitle = "As of 11/05/2023",
    x = "",
    y = "Cumulative Cases",
    color = "Continent / Country" # Legend text
  )

# get world map
world <- map_data("world") %>%
         select(c("lat", "long", "region", "group"))

# prepare 
covid_data_tbl_prepared <- covid_data_tbl %>% 
    select(c("location", "total_deaths_per_million")) %>%
    # fill NA with zeros
    mutate_at("total_deaths_per_million", ~replace_na(.,0)) %>%
  
    # get mortality rate per country
    group_by(location) %>%
    summarize(mortality_rate = max(total_deaths_per_million) / 10000) %>%
    #summarize(mortality_rate = scales::percent(max(total_deaths_per_million) / 1000000, 
    #                                           accuracy=0.01)) %>%
  
    # rename location  for world_tbl
    mutate(location = case_when(
    
      location == "United Kingdom" ~ "UK",
      location == "United States" ~ "USA",
      location == "Democratic Republic of Congo" ~ "Democratic Republic of the Congo",
      TRUE ~ location
    
    )) %>%
    distinct()

  
covid_world_data_tbl <- world %>%
                        left_join(covid_data_tbl_prepared, by = c("region" = "location")) 


covid_world_data_tbl %>% ggplot(aes(long, lat, group, map_id = region, fill = mortality_rate)) +
   scale_fill_gradient(
    low = "#f7968f",
    high = "#8f0b01") +
   geom_map(map = covid_world_data_tbl) +
   expand_limits(x = covid_world_data_tbl$long, y = covid_world_data_tbl$lat) +
   guides(fill = guide_colourbar(labels = ~scales::percent(.x, accuracy = 0.01))) +
   dark_theme_dark() + 
   theme(panel.grid.major = element_blank(), 
         panel.grid.minor = element_blank(),
         panel.background = element_rect(fill='transparent'),
         plot.background = element_rect(fill='transparent', color=NA),
         legend.background = element_rect(fill='transparent'))


covid_world_data_tbl %>% ggplot(aes(long, lat, group, map_id = region, fill = mortality_rate)) +
  guides(fill = guide_colourbar(labels = (function(x) comma(x, suffix= " %")))) +
  scale_fill_gradient(
    low = "#f7968f",
    high = "#8f0b01") +
  geom_map(map = covid_world_data_tbl) +
  expand_limits(x = covid_world_data_tbl$long, y = covid_world_data_tbl$lat) +
  dark_theme_dark() + 
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill='transparent'),
        plot.background = element_rect(fill='transparent', color=NA),
        legend.background = element_rect(fill='transparent'),
        axis.text=element_blank(),
        axis.ticks=element_blank(),
        axis.title=element_blank()) +
  labs(
    title = "Confirmed COVID-19 deaths relative to the size of the population",
    subtitle = "As of 11/05/2023")
library(purrr)
#  ...

    