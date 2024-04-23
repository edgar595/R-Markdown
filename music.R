library(tidyverse)
library(spotifyr)
library(corrr)
library(tidymodels)

# Set your Spotify Developer credentials
Sys.setenv(SPOTIFY_CLIENT_ID = "08d406074a2741afb8237e6bae170e63",
           SPOTIFY_CLIENT_SECRET = "8028b272e50a416f8ebe7ace4f010e44")

rankings <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-04-14/rankings.csv")

rankings %>%
  count(year, sort = TRUE) 

rankings %>%
  ggplot(aes(year, points, color = gender)) +
  geom_point()

rankings %>%
  pull(artist)


access_token <- get_spotify_access_token()

playlist_features <- get_playlist_audio_features("tmock1923", "7esD007S7kzeSwVtcH9GFe")


rankings %>%
  mutate(search_query = paste0(title ,artist),
         search_query = str_to_lower(search_query)) %>%
  select(search_query)


pull_id <- function(query) {
  search_spotify(query, "track") %>%
    arrange(-popularity) %>%
    filter(row_number() == 1) %>%
    pull(id)
}

rankings %>%
  head(5) %>%
  mutate(search_query = paste(title, artist),
         search_query = str_to_lower(search_query)) %>%
  mutate(id = map_chr(search_query, pull_id)) %>%
  select(artist, title, id)


ranking_ids <- rankings %>%
  mutate(
    search_query = paste(title, artist),
    search_query = str_to_lower(search_query),
    search_query = str_remove(search_query, "ft.*$")
  ) %>%
  mutate(id = map_chr(search_query, possibly(pull_id, NA_character_)))

ranking_ids %>%
  select(title, artist, id)

sum(is.na(ranking_ids$id))

ranking_features <- ranking_ids %>%
  mutate(id_group = row_number() %/% 80) %>%
  select(id_group,id) %>%
  nest(data = c(id)) %>%
  mutate(audio_features = map(data, ~get_track_audio_features(.$id)))

ranking_features %>%
  select(audio_features) %>%
  unnest(audio_features)

ranking_df <- ranking_ids %>%
  bind_cols(ranking_features %>%
              select(audio_features) %>%
              unnest(audio_features)) %>%
  select(title, artist, points, year, danceability:tempo) %>%
  na.omit()





ranking_df %>%
  select(year:tempo) %>%
  correlate() %>%
  rearrange() %>%
  shave() %>%
  rplot(shape = 15, colors = c("darkorange","green","darkcyan"))


rank_lm <- ranking_df %>%
  select(-title, -artist) %>%
  lm(log(points) ~ ., data =.)

summary(rank_lm)

##prinicpal component analysis 

ranking_rec <- recipe(points ~., data = ranking_df) %>%
  update_role(title, artist, new_role = "id") %>%
  step_log(points) %>%
  step_normalize(all_predictors()) %>%
  step_pca(all_predictors())


ranking_prep <- prep(ranking_rec)

tidy_pcaa <- tidy(ranking_prep, 3)

tidy_pcaa %>%
  mutate(component = fct_inorder(component)) %>%
  ggplot(aes(value, terms, fill = terms)) +
  geom_col() +
  facet_wrap(~component)

juice(ranking_prep) %>%
  ggplot(aes(PC1, PC3, label = title)) +
  geom_point() +
  geom_text(check_overlap = TRUE)


sdev <- ranking_prep$steps[[3]]$res$sdev

percent_variation <- sdev^2/sum(sdev^2)

tibble(component = unique(tidy_pcaa$component),
       percent_var = percent_variation) %>%
  mutate(component = fct_inorder(component)) %>%
  ggplot(aes(component, percent_variation)) +
  geom_col()

pca_lm <- juice(ranking_prep) %>%
  select(-title, - artist) %>%
  lm(points ~ ., data= .,)

summary(pca_lm)











































