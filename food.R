

library(tidyverse)
library(countrycode)
library(GGally)
library(tidymodels)

food_consumption <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-02-18/food_consumption.csv")

food <- food_consumption %>%
  mutate(continent = countrycode(country, 
                                 origin = "country.name",
                                 destination = "continent"))  %>%
  select(-co2_emmission) %>%
  pivot_wider(names_from = food_category,
              values_from = consumption) %>%
  janitor::clean_names() %>%
  mutate(africa = case_when(continent == "Africa"~ "Africa",
                            TRUE ~ "other")) %>%
  select(-country, -continent) %>%
  mutate_if(is.character, factor)

food


ggscatmat(food, columns = 1:11, color = "africa", alpha = 0.6)


#tune hyperparameters

set.seed(4567)
food_boot <- bootstraps(food, times = 30)

rf_spec <- rand_forest(
  mode = "classification",
  mtry = tune(),
  min_n = tune(),
  trees = 1000
) %>%
  set_engine("ranger")

rf_grid <- tune_grid(
  rf_spec,
  africa ~ .,
  resamples = food_boot
)


rf_grid %>%
  collect_metrics()




















