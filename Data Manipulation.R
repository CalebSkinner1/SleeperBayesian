library("hoopR")

# sleeper point computation
source("SleeperBayesian/Scrape.R")

# current year
df_2025 <- load_nba_player_box(seasons = 2025)

# last year
df_2024 <- load_nba_player_box(seasons = 2024)
