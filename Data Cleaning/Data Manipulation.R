library("hoopR")
library("tidyverse"); theme_set(theme_minimal())
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
df_2024 <- read_csv("Data Cleaning/2024_stats.R")

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

# grab projections through week 7
through_week <- 7
projections_list <- list(
  list("irving", c(24.1, 23.11, 24.29, 22.37, 23.82, 24.41, 24.45, 26.21, 23.09, 23.63, 24.67, 24.74, 24.22, 24.75, 22.87, 23.73, 23.53, 27.99, 29.32, 27.33, 26.39, 24.2, 24.12, 25.56)),
  list("suggs",c(16.51, 15.97, 16.46, 18.08, 19.29, 21.2, 21.95, 20.81, 21.53, 20.06, 22.05, 21.57, 20.26, 21.4, 20.9, 20.6, 20.43, 20.73, 0, 22.32, 22.42, 18.57, 19.88, 21.91, 17.92, 15)),
  list("miles bridges",c(22.73, 22.27, 20.88, 22.5, 18.88, 20.79, 20.39, 20.11, 0, 0, 0, 19.61, 19.07, 18.86, 20.66, 18.75, 0, 0, 20.25, 22.82, 0, 22.58, 21.29, 21.84)),
  list("Rozan",c(19.19, 21.16, 22.71, 20.81, 20.95, 22.89, 21.46, 22.43, 20.52, 20.95, 23.77, 23.01, 0, 0, 0, 21.4, 19.78, 20.59, 0, 21.93, 18.98, 21.46, 20.94, 19.41, 21.62)),
  list("sengun",c(28.32, 29.29, 26.82, 26.11, 23.38, 23.73, 23.13, 25.31, 23.8, 27.92, 27.64, 23.68, 23.88, 23.89, 23.24, 23.78, 26.57, 26.57, 28.88, 31.98, 26.54, 32.52, 28.75, 25.97)),
  list("draymond green",c(22.48, 20.5, 19.52, 20.25, 19.9, 20.82, 22.79, 20.25, 18.69, 17.61, 20.55, 22.42, 21.3, 22.2, 19.37, 21.75, 21.13, 20.66, 19.06, 0, 20.33, 18.2, 18.2)),
  list("ayton",c(21.34, 23.24, 23.04, 23.63, 23.7, 25.02, 23.63, 21.71, 24.25, 21.77, 22.73, 0, 0, 22.48, 20.38, 21.65, 21.65, 0, 21.91, 21.49, 21.87, 25.49, 23.35, 21.78)),
  list("poole",c(21.14, 21.21, 21.54, 22.55, 21.86, 22.56, 22.98, 21.1, 20.59, 21.61, 22.38, 20.16, 19.12, 19.58, 19.27, 0, 20.65, 18.43, 22.84, 20.15, 19.67, 19.54)),
  list("sexton",c(18.84, 18.73, 18.74, 18.46, 16.3, 19.61, 18.58, 19.38, 16.27, 18.29, 18.02, 17.32, 13.55, 16.3, 15.13, 12.64, 16.63, 20.11, 14.97, 15.61, 14.99, 16.12, 14.89)),
  list("wemba",c(34.27, 34.71, 33.58, 34.31, 34.76, 34.48, 33.48, 34.07, 35.2, 34.49, 33.95, 36.2, 35.56, 0, 0, 34.27, 38.68, 36.54, 35.18, 35.2, 34.86, 34.47, 36.06, 41.58)),
  list("portis",c(18.12, 19.84, 19.67, 19.45, 19.52, 19, 26.19, 19.26, 19.15, 18.27, 18.21, 0, 18.77, 16.83, 15.83, 18.1, 17.65, 28.1, 18.2, 16.36, 15.9, 15.07, 15.58)),
  list("shai",c(31.63, 32.04, 31.49, 32.16, 31.07, 30.3, 30.32, 30.77, 31.69, 30.81, 31.73, 33.37, 33.15, 33.7, 34.1, 32.57, 32.31, 30.61, 28.57, 27.02, 32.52, 29.4, 30.78)),
  list("keyonte",c(15.94, 15.75, 15.8, 16.76, 17.49, 18.9, 18.72, 17.66, 0, 16.82, 17.63, 18.26, 15.62, 17.03, 16.5, 14.81, 18.55, 19.63, 16.21, 17.03, 16.62, 17.07, 15.98)),
  list("dort",c(14.56, 14.75, 14.76, 14.36, 13.94, 13.69, 13.78, 14.12, 13.8, 13.93, 14.95, 16.41, 17.3, 17.72, 18.57, 17.43, 16.63, 19.8, 16.86, 12.66, 17.41, 15.52, 13.8)),
  list("huerter",c(14.61, 11.63, 12.85, 14.76, 15.11, 0, 0, 13.56, 12.86, 12.46, 17.05, 16.46, 17.46, 20.57, 20.39, 16.95, 15.20, 14.54, 15.86, 9.14, 0, 7.88, 6.63, 6.39, 7.14)),
  list("pippen",c(8.52, 12.59, 13.88, 15.65, 13.46, 15.39, 17.14, 19.33, 18.01, 19.1, 21.19, 19.72, 16.84, 17.58, 18.64, 17.98, 12.95, 15.51, 18.13, 11.63, 10.69, 7.9, 9.22, 7.72, 9.65)),
  list("tatum",c(27.36, 29.03, 28.32, 29.62, 29.95, 30.35, 30.86, 29.03, 29.34, 29.84, 28.44, 29.98, 27.82, 30.34, 29.37, 31.13, 26.83, 27.57, 28.73, 28.31, 32.34, 28.79, 28.2, 29.9)),
  list("harden",c(27.59, 27.43, 28.32, 29.15, 28.26, 28.08, 27.82, 26.95, 26.98, 28.78, 27.08, 27.38, 26.94, 27.55, 27.18, 26.14, 27.36, 26.04, 26.86, 30.58, 27.08, 30.16, 32.46, 27.21, 26.36)),
  list("Bron James",c(26.45, 27.49, 28.26, 27.53, 27.85, 28.91, 28.27, 34.49, 29.95, 30.47, 30.37, 30.57, 29.82, 32.22, 29.77, 29.7, 29.94, 29.05, 27.18, 31.14, 28.02, 27.9, 30.74, 29.81)),
  list("Jalen Williams",c(21.78, 22.43, 22.57, 21.46, 21.49, 21.02, 20.67, 21.83, 22.4, 22, 22.71, 24, 24.2, 23.33, 24.42, 22.99, 23.3, 27.65, 29.32, 27, 29.13, 26.53, 24.53)),
  list("Franz",c(20.43, 22.05, 23.3, 22.53, 22.57, 23.21, 22.71, 21.3, 22.75, 23.38, 23.46, 23.26, 23.21, 22.63, 23.61, 24.88, 24.24, 24.63, 25.16, 30.5, 28.41, 28.32, 32.66, 26.1, 21.59, 22.45)),
  list("vucevic",c(25.12, 25.26, 25.31, 26.05, 26.72, 26.83, 26.93, 27.05, 25.46, 26.8, 26.92, 26.5, 27.11, 27.28, 27.66, 27.14, 28.14, 28.38, 30.67, 27.29, 27.77, 27.52, 27.38, 25.55, 24.14)),
  list("poeltl",c(22.71, 21.8, 21.65, 20.77, 26.84, 28.1, 29.8, 29.68, 28.01, 27.38, 26.61, 25.76, 25.55, 27.17, 27.64, 27.07, 24.92, 24.27, 24.27, 23.71, 25.42, 25.64, 23.73, 23.1)),
  list("Jarrett",c(24.15, 24.01, 24.78, 22.95, 23.54, 22.51, 23.79, 25.3, 24.13, 24.52, 24.73, 24.59, 22.33, 25.78, 27.13, 25.27, 23.82, 24.26, 26.74, 24.57, 20.27, 26.13, 23.53, 20.79, 20.79)),
  list("melo ball",c(27.79, 29.95, 27.76, 29.02, 27.29, 27.36, 27.57, 27.73, 28.93, 27.61, 28.52, 27.07, 27.77, 28.85, 29.53, 29.35, 29.57, 35.14, 0, 0, 0, 0, 0, 0)),
  list("Fox",c(24.02, 26.11, 26.14, 26.5, 26.34, 26.98, 25.74, 27.1, 25.18, 26.23, 26.88, 27.44, 27.59, 31.58, 30.62, 26.73, 25.26, 25.97, 27.43, 25.14, 24.46, 24.87, 24.66, 25.41, 28.44)),
  list("zubac",c(23.35, 27.53, 27.34, 29.11, 26.73, 27.8, 27.51, 27.89, 27.39, 29.52, 27.45, 27.46, 27.5, 26.98, 27.18, 25.33, 24.89, 25.34, 24.86, 25.57, 24.37, 22.75, 26.68, 23.52, 22.16)),
  list("devin booker",c(23.12, 23.12, 23.5, 23.35, 26.97, 24.58, 23.59, 23.55, 25.33, 28.01, 27.91, 26.45, 25.94, 24.67, 25.33, 25.57, 24.53, 23.9, 25.27, 25.51, 24.72, 24.04, 22.04)),
  list("josh hart",c(21.34, 22.36, 22.17, 22.49, 24.52, 23.66, 24.43, 23.63, 23.33, 22.34, 24.35, 24.24, 23.13, 25.9, 24.43, 24, 24.86, 24.35, 24.21, 19.84, 21.68, 23.07, 21.56)),
  list("brunson",c(25.12, 25.32, 24.29, 24.86, 25.92, 25.47, 27.95, 27.22, 26.44, 25.09, 26.73, 27.68, 24.98, 26.75, 25.46, 26.82, 26.6, 23.92, 22.41, 24.53, 23.52, 23.45, 23.79)),
  list("coby white",c(19.52, 20.71, 20, 19.91, 19.78, 19.82, 22.04, 20.57, 19.71, 20.27, 20.16, 20.01, 21.02, 19.87, 20.11, 19.53, 20.21, 22.14, 21.48, 19.54, 18.45, 0, 17.67, 18.99, 17.37)),
  list("john collins",c(20.41, 14.49, 15.28, 16.28, 19.94, 20.17, 17.83, 17.57, 17.4, 21.98, 24.38, 25.6, 22.54, 24.85, 23.72, 21.55, 25.46, 0, 26.07, 25.66, 26.53, 21.05, 18.39)),
  list("tobias harris",c(21.94, 22.32, 21.65, 21.19, 22.75, 22.41, 22.33, 22.49, 21.6, 22.91, 23.04, 21.28, 21.8, 22.48, 23.13, 22.61, 21.57, 19.91, 22.3, 0, 20.32, 19.91, 18.47, 19.02, 18.82)),
  list("derrick white",c(19.25, 21.79, 20.49, 24.17, 21.7, 21.66, 22.85, 22.95, 22.37, 22.26, 22.66, 23.41, 21.21, 24.47, 21.37, 23.71, 21.84, 22.28, 21.71, 20.25, 23.29, 20.03, 19.89, 21.46)),
  list("siakam",c(23.57, 22.21, 22.03, 21.71, 23.04, 22.72, 24.28, 23.75, 26.58, 23.16, 21.62, 24.45, 22.99, 26.06, 22.03, 22.74, 23.69, 22.04, 23.76, 21.98, 23.53, 23.68, 22.62, 26.62, 21.93)),
  list("adebayo",c(25.06, 26.13, 27.21, 25.45, 27.26, 25.76, 25.59, 27.88, 29.02, 29.85, 29.77, 31.12, 30.15, 30.24, 25.7, 28.58, 27.16, 26.46, 30.83, 27.7, 24.97, 24.83)),
  list("brook lopez",c(18.96, 21.86, 21.6, 19.92, 21.13, 19.66, 23.64, 21.98, 20.3, 19.91, 19.23, 23.05, 22.12, 20.41, 22.14, 21.47, 21.36, 26.92, 22.41, 21.94, 19.84, 16.58, 19.34)),
  list("tari eason",c(15.29, 10.06, 11.35, 11.79, 14.13, 13.4, 16.94, 17.62, 17.16, 18.61, 20.79, 18.63, 18.23, 19.02, 18.15, 19.82, 20.29, 18.26, 21.16, 17.77, 18.18, 18.79, 18.61, 17.3)))

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


# Quick Graphs for Results ------------------------------------------------

# quick_players <- c("LeBron James", "Jalen Williams", "Jayson Tatum", "Victor Wembanyama", "Shai Gilgeous-Alexander", "Nikola Jokic")
# 
# # 2024 player densities
# df_2024 %>%
#   mutate(season = 2024) %>% 
#   # bind_rows(df_2025 %>% mutate(season = 2025)) %>%
#   filter(name %in% quick_players, sleeper_points != 0) %>%
#   # group_by(season) %>%
#   ggplot() +
#   facet_wrap(~name, strip.position = "bottom") +
#   geom_density(aes(x = sleeper_points), color = "cadetblue4") +
#   labs(x = "Fantasy Points", y = "", title = "Figure 1: 2024 Player Densities")
# 
# # sleeper projections vs realities
# 
# full_data %>%
#   # filter(name %in% quick_players) %>%
#   filter(sleeper_points != 0, !is.na(sleeper_projection)) %>%
#   ggplot() +
#   geom_jitter(aes(x = sleeper_projection, y = sleeper_points), color = "indianred3", size = 1, width = .1, height = .1) +
#   labs(x = "Sleeper Projections", y = "Fantasy Points", title = "Figure 2: Projections vs Scores")


               