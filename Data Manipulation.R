library("hoopR")
library("tidyverse")

# sleeper point computation
source("Scrape.R")

# fix box score into our desired format
clean_box_score <- function(df){
  df %>% clean_names() %>%
    clean_names() %>%
    filter(season_type == 2) %>%
    select(game_date, athlete_display_name, points, three_point_field_goals_made,
           rebounds, assists, steals, turnovers, blocks) %>%
    rename(
      "date" = game_date,
      "pts" = points,
      "ast" = assists,
      "stl" = steals,
      "x3p" = three_point_field_goals_made,
      "trb" = rebounds,
      "blk" = blocks,
      "tov" = turnovers,
      "name" = athlete_display_name)
}

# last year
df_2024 <- load_nba_player_box(seasons = 2024) %>%
  clean_box_score() %>%
  sleeper_points()

# current year
df_2025 <- load_nba_player_box(seasons = 2025) %>%
  clean_box_score() %>%
  sleeper_points()

# select nine players ie Daniel's team for funsies
team <- c("Kyrie Irving", "Jalen Suggs", "Miles Bridges", "DeMar DeRozan", "Alperen Sengun",
          "Draymond Green", "Deandre Ayton", "Jordan Poole", "Collin Sexton")

# last years stats for priors
last_year_statistics <- df_2024 %>%
  filter(name %in% team) %>%
  group_by(name) %>%
  drop_na() %>%
  summarize(
    n_last_year = n(),
    sse_last_year = (n_last_year - 1)*var(sleeper_points))

# grab projections



