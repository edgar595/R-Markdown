

library(dplyr)

episodes <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-11-23/episodes.csv") %>%
  filter(!is.na(uk_viewers))

head(episodes)
View(episodes)