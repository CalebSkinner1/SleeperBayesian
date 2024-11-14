# load in data
source("Data Manipulation.R")
library("tidymodels")

# teams and games they play
nba_teams <- load_nba_team_box(season = 2025) %>%
  filter(season_type == 2) %>%
  select(game_date, team_name) %>%
  rename(date = game_date)

# each player with missed games
injury_data <- load_nba_player_box(season = 2025) %>%
  select(athlete_display_name, team_name, game_date) %>%
  left_join(df_2025, ., by = join_by(name == athlete_display_name, date == game_date)) %>%
  select(name, team_name) %>%
  distinct() %>%
  full_join(nba_teams, by = join_by(team_name), relationship = "many-to-many") %>%
  select(name, date) %>%
  left_join(df_2025, join_by(name, date)) %>%
  rowwise() %>%
  mutate(played = case_when(
    is.na(sleeper_points) ~ 0,
    sleeper_points > 0 ~ 1,
    .default = 0)) %>%
  ungroup() %>%
  group_by(name) %>%
  left_join(last_year_statistics, by = join_by(name)) %>%
  select(name, date, played, play_rate) %>%
  mutate(
    played = factor(played),
    last_game = lead(played),
    two_games = lead(played, 2),
    three_games = lead(played, 3)) %>%
  drop_na()

lr_spec <- logistic_reg() %>%
  set_engine("glm") %>%
  set_mode("classification")

lr_fit <- lr_spec %>%
  fit(
    played ~ play_rate + last_game + two_games + three_games,
    data = injury_data)

lr_fit %>%
  pluck("fit") %>%
  summary()

augment(lr_fit, new_data = injury_data) %>% view()

augment(lr_fit, new_data = injury_data) %>%
  conf_mat(truth = played, estimate = .pred_class)
