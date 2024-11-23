library("hoopR")
library("tidyverse")
library("janitor")

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

# last year - commented out to make faster
# df_2024 <- load_nba_player_box(seasons = 2024) %>%
#   clean_box_score() %>%
#   sleeper_points()

# write to csv for ease
# write_csv(df_2024, "2024_stats.R")
df_2024 <- read_csv("2024_stats.R")

# select nine players ie Daniel's team for funsies
team <- c("Kyrie Irving", "Jalen Suggs", "Miles Bridges", "DeMar DeRozan", "Alperen Sengun",
          "Draymond Green", "Deandre Ayton", "Jordan Poole", "Collin Sexton")

# last years stats for priors/injury
last_year_statistics <- df_2024 %>%
  group_by(name) %>%
  drop_na() %>%
  summarize(
    n = n(),
    sse = (n - 1)*var(sleeper_points),
    play_rate = n/82
    )

df_2024 %>% filter(str_detect(name, "Brook Lopez")) %>%
  drop_na() %>%
  ggplot() +
  geom_density(aes(x = sleeper_points))


# current year
df_2025 <- load_nba_player_box(seasons = 2025) %>%
  clean_box_score() %>%
  sleeper_points()

# teams and games they play
nba_teams_25 <- load_nba_schedule(season = 2025) %>%
  filter(season_type == 2) %>%
  select(game_date, home_name, away_name) %>%
  pivot_longer(cols = c(home_name, away_name), names_to = "loc", values_to = "team_name") %>%
  select(-loc) %>%
  rename(date = game_date)

# each player with missed games
partial_data <- load_nba_player_box(season = 2025) %>%
  # regular season games
  filter(season_type == 2) %>%
  select(athlete_display_name, team_name, game_date) %>%
  left_join(df_2025, ., by = join_by(name == athlete_display_name, date == game_date)) %>%
  select(name, team_name) %>%
  distinct() %>%
  # join with games that team played in
  full_join(nba_teams_25, by = join_by(team_name), relationship = "many-to-many") %>%
  select(name, date) %>%
  # join back with all data
  left_join(df_2025, join_by(name, date)) %>% 
  arrange(date) %>%
  group_by(name) %>%
  mutate(game_no = row_number()) %>%
  mutate(
    # week of year - will only work through 2024
    week = case_when(
      year(date) == 2024 ~ week(date) - 42,
      year(date) == 2025 ~ week(date + days(2)) + 10,
      .default = NA))

proj_function <- function(proj_list){
  # takes first entry and makes searches for data base for player name
  player <- proj_list[[1]] %>% str_to_title()
  player_up <- partial_data %>% filter(str_detect(name, player)) %>% select(name) %>% distinct() %>% pull()
  
  # if more than one player, we have a problem
  if(length(player_up) != 1){
    stop(player)}
  
  # grab projection vector
  projection <- proj_list[[2]]
  
  # number of entries there should be in the vector
  games <- partial_data %>%
    filter(week < through_week + 1) %>%
    filter(str_detect(name, player)) %>%
    nrow()
  if(games != length(projection)){
    stop(player)
  }
  
  # create tibble
  tibble("name" = player_up,
         "sleeper_projection" = projection) %>%
    return()
}

# grab projections through week 5
through_week <- 5
projections_list <- list(
  list("irving", c(24.1, 23.11, 24.29, 22.37, 23.82, 24.41, 24.45, 26.21, 23.09, 23.63, 24.67, 24.74, 24.22, 24.75, 22.87, 23.73, 23.53)),
  list("suggs",c(16.51, 15.97, 16.46, 18.08, 19.29, 21.2, 21.95, 20.81, 21.53, 20.06, 22.05, 21.57, 20.26, 21.4, 20.9, 20.6, 20.43, 20.73)),
  list("miles bridges",c(22.73, 22.27, 20.88, 22.5, 18.88, 20.79, 20.39, 20.11, 0, 0, 0, 19.61, 19.07, 18.86, 20.66, 18.75)),
  list("Rozan",c(19.19, 21.16, 22.71, 20.81, 20.95, 22.89, 21.46, 22.43, 20.52, 20.95, 23.77, 23.01, 0, 0, 0, 21.4, 19.78)),
  list("sengun",c(28.32, 29.29, 26.82, 26.11, 23.38, 23.73, 23.13, 25.31, 23.8, 27.92, 27.64, 23.68, 23.88, 23.89, 23.24, 23.78, 26.57, 26.57)),
  list("draymond green",c(22.48, 20.5, 19.52, 20.25, 19.9, 20.82, 22.79, 20.25, 18.69, 17.61, 20.55, 22.42, 21.3, 22.2, 19.37, 21.75)),
  list("ayton",c(21.34, 23.24, 23.04, 23.63, 23.7, 25.02, 23.63, 21.71, 24.25, 21.77, 22.73, 0, 0, 22.48, 20.38, 21.65, 21.65)),
  list("poole",c(21.14, 21.21, 21.54, 22.55, 21.86, 22.56, 22.98, 21.1, 20.59, 21.61, 22.38, 20.16, 19.12, 19.58, 19.27)),
  list("sexton",c(18.84, 18.73, 18.74, 18.46, 16.3, 19.61, 18.58, 19.38, 16.27, 18.29, 18.02, 17.32, 13.55, 16.3, 15.13, 12.64)),
  list("wemba",c(34.27, 34.71, 33.58, 34.31, 34.76, 34.48, 33.48, 34.07, 35.2, 34.49, 33.95, 36.2, 35.56, 0, 0, 34.27, 38.68)),
  list("portis",c(18.12, 19.84, 19.67, 19.45, 19.52, 19, 26.19, 19.26, 19.15, 18.27, 18.21, 0, 18.77, 16.83, 15.83, 18.1, 17.65)),
  list("shai",c(31.63, 32.04, 31.49, 32.16, 31.07, 30.3, 30.32, 30.77, 31.69, 30.81, 31.73, 33.37, 33.15, 33.7, 34.1, 32.57)),
  list("keyonte",c(15.94, 15.75, 15.8, 16.76, 17.49, 18.9, 18.72, 17.66, 0, 16.82, 17.63, 18.26, 15.62, 17.03, 16.5, 14.81)),
  list("dort",c(14.56, 14.75, 14.76, 14.36, 13.94, 13.69, 13.78, 14.12, 13.8, 13.93, 14.95, 16.41, 17.3, 17.72, 18.57, 17.43)),
  list("huerter",c(14.61, 11.63, 12.85, 14.76, 15.11, 0, 0, 13.56, 12.86, 12.46, 17.05, 16.46, 17.46, 20.57, 20.39, 16.95, 15.20)),
  list("pippen",c(8.52, 12.59, 13.88, 15.65, 13.46, 15.39, 17.14, 19.33, 18.01, 19.1, 21.19, 19.72, 16.84, 17.58, 18.64, 17.98, 12.95)),
  list("tatum",c(27.36, 29.03, 28.32, 29.62, 29.95, 30.35, 30.86, 29.03, 29.34, 29.84, 28.44, 29.98, 27.82, 30.34, 29.37, 31.13, 26.83)),
  list("harden",c(27.59, 27.43, 28.32, 29.15, 28.26, 28.08, 27.82, 26.95, 26.98, 28.78, 27.08, 27.38, 26.94, 27.55, 27.18, 26.14, 27.36, 26.04)),
  list("Bron James",c(26.45, 27.49, 28.26, 27.53, 27.85, 28.91, 28.27, 34.49, 29.95, 30.47, 30.37, 30.57, 29.82, 32.22, 29.77, 29.7)),
  list("Jalen Williams",c(21.78, 22.43, 22.57, 21.46, 21.49, 21.02, 20.67, 21.83, 22.4, 22, 22.71, 24, 24.2, 23.33, 24.42, 22.99)),
  list("Franz",c(20.43, 22.05, 23.3, 22.53, 22.57, 23.21, 22.71, 21.3, 22.75, 23.38, 23.46, 23.26, 23.21, 22.63, 23.61, 24.88, 24.24, 25.59)),
  list("vucevic",c(25.12, 25.26, 25.31, 26.05, 26.72, 26.83, 26.93, 27.05, 25.46, 26.8, 26.92, 26.5, 27.11, 27.28, 27.66, 27.14, 28.14, 28.38)),
  list("poeltl",c(22.71, 21.8, 21.65, 20.77, 26.84, 28.1, 29.8, 29.68, 28.01, 27.38, 26.61, 25.76, 25.55, 27.17, 27.64, 27.07, 24.92)),
  list("Jarrett",c(24.15, 24.01, 24.78, 22.95, 23.54, 22.51, 23.79, 25.3, 24.13, 24.52, 24.73, 24.59, 22.33, 25.78, 27.13, 25.27, 23.82, 24.26)),
  list("melo ball",c(27.79, 29.95, 27.76, 29.02, 27.29, 27.36, 27.57, 27.73, 28.93, 27.61, 28.52, 27.07, 27.77, 28.85, 29.53, 29.35)),
  list("Fox",c(24.02, 26.11, 26.14, 26.5, 26.34, 26.98, 25.74, 27.1, 25.18, 26.23, 26.88, 27.44, 27.59, 31.58, 30.62, 26.73, 25.26)),
  list("zubac",c(23.35, 27.53, 27.34, 29.11, 26.73, 27.8, 27.51, 27.89, 27.39, 29.52, 27.45, 27.46, 27.5, 26.98, 27.18, 25.33, 24.89, 25.34)),
  list("devin booker",c(23.12, 23.12, 23.5, 23.35, 26.97, 24.58, 23.59, 23.55, 25.33, 28.01, 27.91, 26.45, 25.94, 24.67, 25.33, 25.57)),
  list("anthony edwards",c(23.9, 25.72, 26.83, 26.51, 26.08, 25.83, 26.61, 25.45, 25.47, 24.84, 25.68, 26.52, 25.76, 25.79, 28.81, 26.12)))

projections <- map(projections_list, ~proj_function(.x)) %>%
  data.table::rbindlist() %>%
  as_tibble() %>%
  group_by(name) %>%
  mutate(game_no = row_number())

# Compiling into one df ---------------------------------------------------

# all projected games




# complete
full_data <- partial_data %>%
  full_join(projections, join_by(name, game_no)) %>%
  filter(!is.na(sleeper_projection)) %>%
  ungroup() %>%
  relocate(date) %>%
  relocate(game_no, .before = pts)

# # current year - deprecated
# current_team <- df_2025 %>%
#   filter(name %in% team) %>%
#   group_by(name) %>%
#   arrange(date) %>%
#   mutate(
#     game_no = row_number()) %>%
#   ungroup() %>% 
#   full_join(projections, by = join_by("name", "game_no")) %>%
#   select(date, name, game_no, sleeper_points, sleeper_projection)

## Naive Alphas
#### Not Divided by 2 yet :(
naiveAlphasBetas <- df_2024 %>% group_by(name) %>% summarise(GP = length(name), 
                                                             Betas = (GP - 1) * var(sleeper_points)) %>%
  ungroup()

teamParams <- naiveAlphasBetas %>% filter(name %in% team)


## Delete everything
rm(projections_list, partial_data, projections)
  