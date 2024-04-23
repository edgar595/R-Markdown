

library(tidyverse)
library(tidymodels)

attendance <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-02-04/attendance.csv")
standings <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-02-04/standings.csv")

attendance_joined <- attendance %>%
  left_join(standings,
            by = c("year", "team_name", "team")
  )

attendance_joined %>%
  filter(!is.na(weekly_attendance)) %>%
  ggplot(aes(fct_reorder(team_name, weekly_attendance),
         weekly_attendance, 
         fill = playoffs)) +
  geom_boxplot(outlier.alpha = 0.5) +
  coord_flip()


attendance_joined %>%
  distinct(team_name, margin_of_victory, playoffs, year) %>%
  ggplot(aes(margin_of_victory, fill = playoffs)) +
  geom_histogram(position = "identity", alpha = 0.8)

attendance_joined %>%
  mutate(week = factor(week)) %>%
  ggplot(aes(week, weekly_attendance, fill= week)) +
  geom_boxplot(outlier.alpha = 0.7, show.legend = FALSE)

attendance_df <- attendance_joined %>%
   filter(!is.na(weekly_attendance)) %>%
   select(team_name, weekly_attendance, margin_of_victory,
          strength_of_schedule, week,year, playoffs) 

attendance_split <- attendance_df %>%
    initial_split(strata = playoffs)

nfl_train <- training(attendance_split)
nfl_test <- testing(attendance_split)


#Training Models

#linear model

lm_spec <- linear_reg() %>%
  set_engine(engine = "lm")

lm_fit <- lm_spec %>%
  fit(weekly_attendance~ ., data = nfl_train)

#Random Forest

rf_spec <- rand_forest(mode = "regression") %>%
  set_engine("ranger")

rf_fit <- rf_spec %>%
  fit(weekly_attendance ~ ., data = nfl_train)


#Evaluate the Model

result_train <- lm_fit %>%
  predict(new_data = nfl_train) %>%
  mutate(truth = nfl_train$weekly_attendance,
         model = "lm") %>%
  bind_rows(rf_fit %>%
              predict(new_data = nfl_train) %>%
              mutate(truth = nfl_train$weekly_attendance,
                     model = "rf"))

result_test <- lm_fit %>%
  predict(new_data = nfl_test) %>%
  mutate(truth = nfl_test$weekly_attendance,
         model ="lm") %>%
  bind_rows(rf_fit %>%
              predict(new_data = nfl_test) %>%
              mutate(truth = nfl_test$weekly_attendance,
                     model = "rf"))

result_train %>%
  group_by(model) %>%
  rmse(truth = truth, estimate = .pred)

result_test %>%
  group_by(model) %>%
  rmse(truth = truth, estimate = .pred)

result_test %>%
  mutate(train = "testing") %>%
  bind_rows(result_train %>%
              mutate(train = "training")) %>%
  ggplot(aes(truth, .pred, color = train)) +
  geom_abline(lty =2, color = "black", size = 2) +
  geom_point(alpha = 0.5) +
  facet_wrap(~train)

#trying random
set.seed(2345)
nfl_folds <- vfold_cv(nfl_train, strata = playoffs)

rf_res <- fit_resamples(
  rf_spec,
  weekly_attendance ~ .,
  nfl_folds,
  control = control_resamples(save_pred = TRUE)
)

rf_res %>%
  collect_metrics()


 


  
  




















