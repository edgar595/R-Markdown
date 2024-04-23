

library(tidyverse)
library(dplyr)
library(skimr)
library(GGally)
library(tidymodels)
library(themis)
library(kknn)

hotels <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-02-11/hotels.csv")


#Exploratory
hotel_stays <- hotels %>%
  filter(is_canceled == 0) %>%
  mutate(children = case_when(children + babies > 0 ~ "children",
                              TRUE ~ "none"),
         required_car_parking_spaces = case_when(required_car_parking_spaces > 0 ~ "parking",
                                                 TRUE ~ "none")) %>%
  select(-is_canceled, -babies, -reservation_status)

hotel_stays

hotel_stays %>%
  count(children)

hotel_stays %>%
  mutate(arrival_date_month = factor(arrival_date_month, levels = month.name)) %>%
  count(hotel, arrival_date_month, children) %>%
  group_by(children) %>%
  mutate(propotion = n/sum(n)) %>%
  ggplot(aes(arrival_date_month, propotion, fill = children)) +
  geom_col(position = "dodge") +
  scale_y_continuous(labels = scales::percent_format()) +
  facet_wrap(~hotel, nrow = 2)


hotel_stays %>%
  select(children, adr, required_car_parking_spaces,
         total_of_special_requests) %>%
  ggpairs(aes(color = children))


#models

hotels_df <- hotel_stays %>%
  select(children, adults, required_car_parking_spaces, 
         total_of_special_requests, meal, adr, arrival_date_month,
         stays_in_weekend_nights, stays_in_week_nights, hotel) %>%
  mutate_if(is.character, factor)


set.seed(4567)

hotels_split <- hotels_df %>%
  initial_split()

hotels_train <- training(hotels_split)
hotels_test <- testing(hotels_split)


#feature engineering

hotel_recipe <- recipe(children ~ ., data = hotels_train) %>%
  step_downsample(children) %>%
  step_dummy(all_nominal(), -all_outcomes()) %>%
  step_zv(all_numeric()) %>%
  step_normalize(all_numeric()) %>%
  prep()


hotel_recipe


test_reciped <- bake(hotel_recipe, new_data = hotels_test)

knn_spec <- nearest_neighbor() %>%
  set_engine("kknn") %>%
  set_mode("classification")

knn_fit <- knn_spec %>%
  fit(children ~., data = juice(hotel_recipe))


tree_spec <- decision_tree() %>%
  set_engine("rpart") %>%
  set_mode("classification")

tree_fit <- tree_spec %>%
  fit(children ~ ., data = juice(hotel_recipe))


#evaluate models

validation_split <- mc_cv(juice(hotel_recipe), prop = 0.9, strata = children)
validation_split

KNN_res <- fit_resamples(
  knn_spec,
  children ~.,
  validation_split,
  control = control_resamples(save_pred = TRUE)
)

KNN_res %>%
  collect_metrics()


TREE_res <- fit_resamples(
  tree_spec,
  children ~ .,
  validation_split,
  control = control_resamples(save_pred = TRUE)
)

TREE_res %>%
  collect_metrics


KNN_res %>%
  unnest(.predictions) %>%
  mutate(model = "knn") %>%
  bind_rows(TREE_res %>%
              unnest(.predictions) %>%
              mutate(model = "rpart"))
























