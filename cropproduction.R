

library(ggrepel)
library(tidyverse)
library(tidymodels)

key_crop_yields <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-09-01/key_crop_yields.csv")
land_use <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-09-01/land_use_vs_yield_change_in_cereal_production.csv")

top_countries <- land_use %>%
  janitor::clean_names() %>%
  filter(!is.na(code),
         entity != "World") %>%
  group_by(entity) %>%
  filter(year == max(year)) %>%
  ungroup() %>%
  slice_max(total_population_gapminder, n = 30 ) %>%
  pull(entity)

tidy_yield <- key_crop_yields %>%
  janitor::clean_names() %>%
  pivot_longer(wheat_tonnes_per_hectare: bananas_tonnes_per_hectare,
               names_to = "crop" , values_to = "yield") %>%
  mutate(crop = str_remove(crop, "_tonnes_per_hectare")) %>%
  filter(crop %in% c("wheat", "maize", "rice", "barley"),
        entity %in% top_countries,
        !is.na(yield))

tidy_yield %>%
  ggplot(aes(year, yield, color = crop)) +
  geom_line() +
  facet_wrap(~entity)

tidy_lm <- tidy_yield %>%
  nest(yields = c("year", "yield")) %>%
  mutate(model = map(yields, ~ lm(yield ~ year, data = .x)))

slopes <- tidy_lm %>%
  mutate(coefs =map(model, tidy)) %>%
  unnest(coefs) %>%
  filter(term == "year") %>%
  mutate(p.value = p.adjust(p.value))

slopes%>%
 ggplot(aes(estimate, p.value)) +
  geom_vline(xintercept = 0) +
  geom_point(aes(color = crop), show.legend = FALSE) +
  geom_text_repel(
    aes(label = paste(entity)),  # Add labels here
    size =3
  )+
  scale_y_log10() +
  facet_wrap(~crop)











