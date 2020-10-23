# Get the Data

# Read in with tidytuesdayR package 
# Install from CRAN via: install.packages("tidytuesdayR")
# This loads the readme and all the datasets for the week of interest

# Either ISO-8601 date or year/week works!

# tuesdata <- tidytuesdayR::tt_load('2020-10-20')
tuesdata <- tidytuesdayR::tt_load(2020, week = 43)

beer_awards <- tuesdata$beer_awards


# https://www.datanovia.com/en/blog/how-to-create-a-map-using-ggplot2/
library(ggplot2)
library(maps)
library(dplyr)
library(usmap)

# cleaning
beer_awards$state <- toupper(beer_awards$state)

# aggregating
beer_aggregates <- beer_awards %>%
    group_by(medal, state) %>%
    count()

# retrieve us map and make beer_map states map data and merge with crime data
us_map <- us_map()
beer_map <- left_join(us_map, beer_aggregates, 
                      by = c("abbr" = "state")) %>%
    group_by(abbr, full, medal, n) %>%
    summarize(x = mean(x),
              y = mean(y))

# preparing title
title <- paste0("US Beer awards, per state, ", 
                min(beer_awards$year), "-",
                max(beer_awards$year))

# create the map
ggplot(us_map, aes(x, y, group = group)) +
    
    theme_void() + 
    theme(panel.background = element_rect(fill = "black"),
          plot.background = element_rect(fill = "dimgrey"),
          plot.title = element_text(hjust = 0.1)) +
    
    ggtitle(title) +
    
    geom_polygon(fill = "palegreen4", color = "white") +
    
    geom_point(data = filter(beer_map, medal == "Bronze"),
               aes(x + 5*10^4, y - 5*10^4, group = abbr, size = n), 
               color = "#cd7f32") +
    geom_point(data = filter(beer_map, medal == "Silver"),
               aes(x - 5*10^4, y - 5*10^4, group = abbr, size = n), 
               color = "#c0c0c0") +
    geom_point(data = filter(beer_map, medal == "Gold"),
               aes(x, y + 5*10^4, group = abbr, size = n), 
               color = "#ffd700")

ggsave(format(Sys.time(), "./2020-10-20_Beer-Awards/%Y%m%d_%H%M%S.jpg"),
       width = 320, height = 240, units = "mm")
