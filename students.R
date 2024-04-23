

library(tidyverse)
library(tidymodels)

tuition_cost <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-03-10/tuition_cost.csv")

diversity_raw <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-03-10/diversity_school.csv") %>%

  
#exploratory analysis

school_diversity <- diversity_raw %>%
  filter(category == "Total Minority") %>%
  mutate(total_minority = enrollment/ total_enrollment)

school_diversity %>%
  ggplot(aes(total_minority)) +
  geom_histogram()

university_df <- school_diversity %>%
  transmute(diversity = case_when(total_minority > 0.3 ~ "high",
                                  TRUE ~ "low"),
            name, state, total_enrollment) %>%
  inner_join(tuition_cost %>%
               select(name, type,degree_length, in_state_tuition,
                      out_of_state_total)) %>%
  left_join(tibble(state = state.name, region = state.region)) %>%
  select(-state, -name) %>%
  mutate_if(is.character, factor)

#visualization analysis

university_df %>%
  ggplot(aes(type, in_state_tuition, fill = diversity)) +
  geom_boxplot()

#Train and Test

university_split <- university_df %>%
  initial_split(strata = diversity)

uni_train <- training(university_split)
uni_test <- testing(university_split)


uni_rec <- recipe(diversity ~ ., data = uni_train) %>%
  step_corr(all_numeric()) %>%
  step_dummy(all_nominal(), -all_outcomes()) %>%
  step_zv(all_numeric()) %>%
  step_normalize(all_numeric()) %>%
  prep()


#logistic Regression

glm_spec <- logistic_reg() %>%
  set_engine("glm")

glm_fit <- glm_spec %>%
  fit(diversity ~ ., uni_juiced)


#knn

knn_spec <- nearest_neighbor() %>%
  set_engine("kknn") %>%
  set_mode("classification")

knn_fit <- knn_spec %>%
  fit(diversity ~ ., data = juice(uni_rec))


#Tree decisions
tree_spec <- decision_tree() %>%
  set_engine("rpart") %>%
  set_mode("classification")

tree_fit <- tree_spec %>%
  fit(diversity ~ ., data = juice(uni_rec))

tree_fit

#############evaluating model

set.seed(123)
folds <- vfold_cv(uni_train, strata = diversity)

set.seed(234)
glm_rs <- glm_spec %>%
  fit_resamples(
    uni_rec,
    folds,
    metrics = metric_set(roc_auc, sens, spec),
    control = control_resamples(save_pred = TRUE)
  )

set.seed(234)
knn_rs <- knn_spec %>%
  fit_resamples(
    uni_rec,
    folds,
    metrics = metric_set(roc_auc, sens, spec),
    control = control_resamples(save_pred = TRUE)
  )

set.seed(234)
tree_rs <- tree_spec %>%
  fit_resamples(
    uni_rec,
    folds,
    metrics = metric_set(roc_auc, sens, spec),
    control = control_resamples(save_pred = TRUE)
  )

glm_rs %>%
  collect_metrics()

knn_rs %>%
  collect_metrics()

tree_rs %>%
  collect_metrics()




























  