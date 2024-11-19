# load in data
source("Data Manipulation.R")

# libraries
library("tidymodels")
library("bayesrules")
library("rstanarm")
library("bayesplot")
library("tidybayes")
library("broom.mixed")

# 2024-2025 ---------------------------------------------------------------

# teams and games they play
nba_teams_25 <- load_nba_team_box(season = 2025) %>%
  filter(season_type == 2) %>%
  select(game_date, team_name) %>%
  rename(date = game_date)

# each player with missed games
injury_data_25 <- load_nba_player_box(season = 2025) %>%
  filter(season_type == 2) %>%
  select(athlete_display_name, team_name, game_date) %>%
  left_join(df_2025, ., by = join_by(name == athlete_display_name, date == game_date)) %>%
  select(name, team_name) %>%
  distinct() %>%
  full_join(nba_teams_25, by = join_by(team_name), relationship = "many-to-many") %>%
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
    three_games = lead(played, 3),
    back_to_back = if_else(date == lead(date) + days(1) & lead(played) == 1, 1, 0)) %>%
  drop_na()

# 2023-2024 ---------------------------------------------------------------

# teams and games they play
nba_teams_24 <- load_nba_team_box(season = 2024) %>%
  filter(season_type == 2) %>%
  select(game_date, team_name) %>%
  rename(date = game_date)

# compute play rate from 2023
statistics_2023 <- load_nba_player_box(season = 2023) %>%
  filter(season_type == 2) %>%
  rename(name = athlete_display_name) %>%
  select(name) %>%
  group_by(name) %>%
  summarize(
    play_rate = n()/82)

# each player with missed games
injury_data_24 <- load_nba_player_box(season = 2024) %>%
  filter(season_type == 2) %>%
  select(athlete_display_name, team_name, game_date) %>%
  left_join(df_2024, ., by = join_by(name == athlete_display_name, date == game_date)) %>%
  select(name, team_name) %>%
  distinct() %>%
  full_join(nba_teams_24, by = join_by(team_name), relationship = "many-to-many") %>%
  select(name, date) %>%
  left_join(df_2024, join_by(name, date)) %>%
  rowwise() %>%
  mutate(played = case_when(
    is.na(sleeper_points) ~ 0,
    sleeper_points > 0 ~ 1,
    .default = 0)) %>%
  ungroup() %>%
  group_by(name) %>%
  left_join(statistics_2023, by = join_by(name)) %>%
  select(name, date, played, play_rate) %>%
  mutate(
    played = factor(played),
    last_game = lead(played),
    two_games = lead(played, 2),
    three_games = lead(played, 3),
    back_to_back = if_else(date == lead(date) + days(1) & lead(played) == 1, 1, 0)) %>%
  drop_na()


# Frequentist Baloney -----------------------------------------------------

# 2024-25
# model
lr_spec <- logistic_reg() %>%
  set_engine("glm") %>%
  set_mode("classification")

lr_fit_25 <- lr_spec %>%
  fit(
    played ~ play_rate + last_game + two_games + three_games + back_to_back,
    data = injury_data_25)

lr_fit_25 %>%
  pluck("fit") %>%
  summary()

# augment(lr_fit, new_data = injury_data) %>% view()

augment(lr_fit_25, new_data = injury_data_25) %>%
  conf_mat(truth = played, estimate = .pred_class)

augment(lr_fit_25, new_data = injury_data_25) %>%
  accuracy(truth = played, estimate = .pred_class)

# 2023-24

lr_fit_24 <- lr_spec %>%
  fit(
    played ~ play_rate + last_game + two_games + three_games + back_to_back,
    data = injury_data_24)

lr_fit_24 %>%
  pluck("fit") %>%
  summary()

# augment(lr_fit_24, new_data = injury_data_24) %>% view()

augment(lr_fit_24, new_data = injury_data_24) %>%
  conf_mat(truth = played, estimate = .pred_class)

# accuracy with three games is approx equal to accuracy with six games
augment(lr_fit_24, new_data = injury_data_24) %>%
  accuracy(truth = played, estimate = .pred_class)

# Stan Bayesian -----------------------------------------------------------

# use global 2023-2024 results as prior for 2024-2025
# this would be good if i can implement
priors <- lr_fit_24 %>% tidy() %>% select(term, estimate, std.error)

bayes_run_data <- injury_data_25 %>% filter(name %in% team)

# Run a prior simulation
injury_prior <- stan_glm(played ~ last_game + two_games + three_games + play_rate + back_to_back,
                         data = injury_data_25,
                         family = binomial(link = "logit"),
                         prior_intercept = normal(priors$estimate[1], priors$std.error[1]^2*1000),
                         prior = normal(priors$estimate[2:6], priors$std.error[2:6]^2*1000),
                         chains = 4, iter = 5000*2,
                         prior_PD = TRUE)

# Run a posterior simulation
injury_posterior <- stan_glm(played ~ last_game + two_games + three_games + play_rate + back_to_back,
                             data = injury_data_25,
                             family = binomial(link = "logit"),
                             prior_intercept = normal(priors$estimate[1], priors$std.error[1]^2*1000),
                             prior = normal(priors$estimate[2:6], priors$std.error[2:6]^2*1000),
                             chains = 4, iter = 5000*2,
                             prior_PD = FALSE)

print(injury_posterior)
prior_summary(injury_posterior)

mcmc_trace(injury_posterior)
mcmc_dens_overlay(injury_posterior)
mcmc_acf(injury_posterior)

# overall injury comments
# difficult, because we often know more information than we are able to insert into the model
# i.e if sleeper's model projects 0 points, then the player will likely miss the game.
# i'm not sure how to account for this in the injury prediction model

# on the other side, we need a way to show that players expected to score 0 points
# are very likely to score 0 in the main model. Not sure how to quantify that


