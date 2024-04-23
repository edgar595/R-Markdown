

measles <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-02-25/measles.csv")

measles_df <- measles %>%
  filter(mmr > 0) %>%
  transmute(state,
            mmr_threshold = case_when(mmr > 95 ~ "Above",
                                      TRUE ~ "Below")) %>%
  mutate_if(is.character, factor)


measles_df %>%
  group_by(state) %>%
  summarise(mmr = mean(mmr_threshold == "Above")) %>%
  ggplot(aes(state, mmr, fill = state)) +
  geom_col(show.legend = FALSE)


#glm


glm_fit <- logistic_reg() %>%
  set_engine("glm") %>%
  fit(mmr_threshold ~ state, data = measles_df)


tidy(glm_fit) %>% filter(p.value < 0.05)

new_school <- tibble(state = unique(measles_df$state))

conf_int <- predict(glm_fit,
                    new_data = new_school,
                    type = "conf_int")

mean_pred <- predict(glm_fit,
                     new_data = new_school,
                     type = "prob")

schools_result <- new_school %>%
  bind_cols(mean_pred) %>%
  bind_cols(conf_int)

