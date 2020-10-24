# Get the Data

# Read in with tidytuesdayR package 
# Install from CRAN via: install.packages("tidytuesdayR")
# This loads the readme and all the datasets for the week of interest

# Either ISO-8601 date or year/week works!

# tuesdata <- tidytuesdayR::tt_load('2020-10-20')
if (!exists("beer_awards")) {
    tuesdata <- tidytuesdayR::tt_load(2020, week = 43)
    
    beer_awards <- tuesdata$beer_awards
}

# https://www.datanovia.com/en/blog/how-to-create-a-map-using-ggplot2/
library(ggplot2)
library(maps)
library(dplyr)
library(usmap)

# cleaning
beer_awards$state <- toupper(beer_awards$state)

# aggregating
beer_aggregates <- beer_awards %>%
    mutate(medal = factor(medal, levels = c("Gold", "Silver", "Bronze"))) %>%
    group_by(medal, state) %>%
    count()

# retrieve us map and make beer_map states map data and merge with crime data
us_map <- us_map()

beer_map <- left_join(us_map, beer_aggregates, 
                      by = c("abbr" = "state")) %>%
    group_by(abbr, full, medal, n) %>%
    summarize(x = mean(x),
              y = mean(y)) %>%
    group_by(medal) %>%
    mutate(rank = row_number(-n)) %>%
    ungroup() %>%
    mutate(x = ifelse(medal == "Gold", x,
                      ifelse(medal == "Silver", 
                             x - 7*10^4, 
                             x + 7*10^4)),
           y = ifelse(medal == "Gold", 
                      y + 6*10^4,
                      y - 6*10^4)) %>%
    na.omit()
    

# preparing title
title <- paste0("US Beer awards, per state, ", 
                min(beer_awards$year), "-",
                max(beer_awards$year))
subtitle <- "California won most Gold, Silver, and Bronze medals, with Colorado 2nd"

medal_fill_colors <- c("Bronze" = "#cd7f32",
                       "Silver" = "#c0c0c0",
                       "Gold"   = "#ffd700")
medal_rank_colors <- c("Bronze" = "white",
                       "Silver" = "black",
                       "Gold"   = "red")

# create the map
ggplot(us_map, aes(x, y, group = group)) +
    
    theme_void() + 
    theme(text = element_text(family = "serif",
                              color = "white"),
          
          plot.background = element_rect(fill = "dimgrey"),
          plot.margin = unit(rep(5, 4), "mm"),
          plot.title = element_text(hjust = 0.1,
                                    face = "bold",
                                    size = 20),
          plot.subtitle = element_text(hjust = 0.1)) +
    
    ggtitle(title, subtitle) +
    labs(caption = "@martinvanelp, Source: https://github.com/rfordatascience/tidytuesday") +
    
    guides(
        fill  = guide_legend(title = "Medal"),
        color = guide_legend(title = "Rank (top 10)",
                             override.aes = aes(label = "1")),
        size  = guide_legend(title = "# of medals")
    ) +
    
    scale_fill_manual(values = medal_fill_colors) +
    scale_color_manual(values = medal_rank_colors) +
    scale_size(range = c(0, 12)) +

    geom_polygon(fill = "palegreen4", color = "white") +
    
    geom_point(data = beer_map,
               aes(x, y, group = abbr, size = n, fill = medal),
               shape = 21) +
    
    geom_text(data = filter(beer_map, rank <= 10),
          aes(x, y, group = abbr, color = medal, label = rank))

ggsave(format(Sys.time(), "./2020-10-20_Beer-Awards/%Y%m%d_%H%M%S.jpg"),
       width = 320, height = 240, units = "mm")
