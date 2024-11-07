source("Data Manipulation.R")

nba_teams <- load_nba_team_box(season = 2025) %>%
  filter(season_type == 2) %>%
  select(game_date, team_name) %>%
  rename(date = game_date)

load_nba_player_box(season = 2025) %>%
  select(athlete_display_name, team_name, game_date) %>%
  left_join(df_2025, ., by = join_by(name == athlete_display_name, date == game_date)) %>%
  select(name, team_name) %>%
  distinct() %>%
  full_join(nba_teams, by = join_by(team_name), relationship = "many-to-many") %>%
  select(name, date) %>%
  left_join(df_2025, join_by(name, date)) %>%
  rowwise() %>%
  mutate(played = if_else(sleeper_points > 0, 1, 0)) %>%
  ungroup() %>%
  group_by(name)
  





