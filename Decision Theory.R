# Decision Theory

# load functions
source("Gibbs Sampler Functions.R")
source("Dec_Theory Functions.R")

# load data
source("Data Manipulation.R")

# Let's look at one player - Shai Gilgeous Alexander, let's say we are trying to
# make a decision to lock or hold him in Week 4 of his regular season

# Shai --------------------------------------------------------------------
shai_full <- full_data %>% filter(str_detect(name, "Shai"))
shai_priors <- last_year_statistics %>% filter(str_detect(name, "Shai"))

# look at his posterior samples for week 4
shai_week <- week_pred(1e+4, data = shai_full,
                       this_week = 3, gp_week = 0,
                       alpha = shai_priors$n/2, beta = shai_priors$sse/2,
                       burnIn = 5e+4)
shai_week %>% summary
shai_week %>% effectiveSize()
shai_week %>% plot

# function computes probability of exceeding score in future games
shai_week %>% prob_decision(best_score = 45, remaining_games = 1, week_data = NULL)

# altogether - estimate cumulative density
N <- 10000
x <- seq(0, 80, length.out = N)
shai_week %>% prob_decision(best_score = x, remaining_games = 1, week_data = NULL) %>%
  ggplot() +
  geom_line(aes(score, exceed_prob))

# rough decision boundary using cdf of max method
shai_week %>%
  prob_decision(best_score = x, remaining_games = 1, week_data = NULL) %>%
  filter(exceed_prob < .5) %>%
  slice(1)

# Dort --------------------------------------------------------------------
dort_full <- full_data %>% filter(str_detect(name, "Dort"))
dort_priors <- last_year_statistics %>% filter(str_detect(name, "Dort"))


dort_week <- week_pred(1e+4, data = dort_full,
                       this_week = 3, gp_week = 0,
                       alpha = dort_priors$n/2, beta = dort_priors$sse/2,
                       burnIn = 5e+4)

# Backwards-Induction Decision Theory -------------------------------------

# for comparison
week_pred(1e+4, data = shai_full,
          this_week = 3, gp_week = 0,
          alpha = shai_priors$n/2, beta = shai_priors$sse/2,
          burnIn = 5e+4) %>%
  single_bid()

shai_week %>%
  prob_decision(best_score = x, remaining_games = 1, week_data = NULL) %>%
  filter(exceed_prob < .5) %>%
  slice(1)


# Backwards Induction Multiple Players ------------------------------------

testPlayers <- c("Kevin Huerter", "Scotty Pippen Jr.", "Luguentz Dort", "DeMar DeRozan", "Miles Bridges")
# 40.5 seconds
t <- Sys.time()
multiChain <- multiplePlayers(testPlayers, 5)
Sys.time() - t

# this works for one spot
multiChain %>% multi_bid_one(full_data, this_week = 5, testPlayers)

# two spots
multiChain %>% multi_bid_two(full_data, this_week = 5, testPlayers)

multiChain %>% multi_bid_two(full_data, this_week = 5, testPlayers[-5])

# three spots
t <- Sys.time()
multiChain %>% multi_bid_three(full_data, this_week = 5, testPlayers)
Sys.time() - t

# Results -----------------------------------------------------------------

# Single Player Results
# the goal of this is to look through each week for each player and see how the decision boundary rules fair
# I will compare this to the naive boundary and no-lock boundary (always take last score)

# function that moves through decision boundaries and players score to compute what the user
# would have scored
decisions <- function(boundaries, scores){
  df <- boundaries %>%
    left_join(scores, by = join_by(name, week, game)) %>%
    rowwise() %>%
    mutate(accept = sleeper_points > dec_boundary) %>%
    ungroup()
  
  # if never meets dec boundary
  if(sum(df$accept) == 0){
    final_game <- df$game %>% tail(1)
  }else{
    # if does exceed boundary
    final_game <- df %>%
      filter(accept == TRUE) %>%
      slice(1) %>%
      select(game) %>%
      pull()
    }
  df %>%
    filter(game == final_game) %>%
    rename("final_points" = sleeper_points) %>%
    return()
}

# makes final points tibble clean
final_points_clean <- function(final_points_list){
  final_points_list[[1]] %>%
    data.table::rbindlist() %>%
    as_tibble() %>%
    select(name, week, game, final_points, dec_boundary) %>%
    mutate(method = final_points_list[[2]])
}

single_player_results <- function(full_data){
  # all combination of players/weeks
  iterations <- full_data %>%
    mutate(
      # only want weeks without injury - our model isn't prepared to handle injuries
      injury = case_when(
        is.na(sleeper_points) ~ 1,
        sleeper_projection == 0 ~ 1,
        sleeper_points == 0 ~ 1,
        .default = 0)) %>%
    group_by(name, week) %>%
    mutate(injured_in_week = sum(injury)) %>%
    ungroup() %>%
    filter(injured_in_week == 0) %>% # lose 81 observations - down to 184
    filter(week > 2) %>% # need some data lose another 94 observations - down to 90
    select(name, week) %>%
    distinct() %>%
    arrange(name, week)
  
  players <- iterations$name
  weeks <- iterations$week
  comb <- str_c(players, "/", weeks)
  
  # chains for each player/week iteration ~6 seconds per chain
  chains <- map2(players, weeks, ~weekPred(3.5e+4, .x, .y, 0, burnIn = 5e+4))
  
  # backward induction decision boundaries for each player/week
  bi_dec_boundaries <- map2(chains, comb, ~single_bid(.x) %>%
                              mutate(name = str_remove(.y, "/.*"), week = str_remove(.y, ".*/") %>% as.integer()))
  
  # cumulative density method ~much faster than before :)
  cdf_dec_boundaries <- map2(chains, comb, ~cdf_boundary(.x) %>%
                               mutate(name = str_remove(.y, "/.*"), week = str_remove(.y, ".*/") %>% as.integer()))
  
  # pure expected value method
  ev_dec_boundaries <- full_data %>%
    right_join(iterations, by = join_by(name, week)) %>%
    group_by(name, week) %>%
    arrange(week) %>%
    mutate(dec_boundary = max(sleeper_projection)) %>%
    mutate(game = row_number()) %>%
    select(name, week, game, dec_boundary) %>%
    relocate(dec_boundary) %>%
    relocate(game) %>%
    group_split()
  
  # Nick Di Method (never lock - ie threshold so high you never lock)
  nd_dec_boundaries <- full_data %>%
    right_join(iterations, by = join_by(name, week)) %>%
    group_by(name, week) %>%
    mutate(dec_boundary = 150) %>%
    mutate(game = row_number()) %>%
    select(name, week, game, dec_boundary) %>%
    relocate(dec_boundary) %>%
    relocate(game) %>%
    group_split()
  
  # tibble of player's real scores
  real_scores <- full_data %>%
    right_join(iterations, by = join_by(name, week)) %>%
    group_by(name, week) %>% 
    mutate(
      game = row_number()) %>%
    select(name, week, game, sleeper_points) %>%
    ungroup()
  
  # computing final points if using each method
  bi_final_points <- list(map(bi_dec_boundaries, ~decisions(.x, real_scores)), "bi")
  cdf_final_points <- list(map(cdf_dec_boundaries, ~decisions(.x, real_scores)), "cdf")
  ev_final_points <- list(map(ev_dec_boundaries, ~decisions(.x, real_scores)), "ev")
  nd_final_points <- list(map(nd_dec_boundaries, ~decisions(.x, real_scores)), "nd")
  
  # compute results
  results <- map(list(bi_final_points, cdf_final_points, ev_final_points, nd_final_points), ~final_points_clean(.x)) %>%
    data.table::rbindlist() %>%
    as_tibble()
  
  return(results)
}

t <- Sys.time()
results <- single_player_results(full_data)
Sys.time() - t

# graph densities
results %>%
  filter(method %in% c("bi", "ev")) %>%
  ggplot() +
  facet_wrap(~week) +
  geom_density(aes(x = final_points, color = method))

# means
results %>%
  filter(method %in% c("bi", "ev")) %>%
  group_by(week, method) %>%
  # group_by(method) %>%
  summarize(
    dec_boundary = mean(dec_boundary),
    mean = mean(final_points),
    game = mean(game))

# view data
results %>%
  filter(method %in% c("bi", "ev")) %>%
  pivot_wider(names_from = method, values_from = c(final_points, dec_boundary, game))

# is sleeper off? no
full_data %>%
  drop_na() %>%
  filter(sleeper_projection != 0) %>%
  group_by(week) %>%
  summarize(diff = mean(sleeper_projection - sleeper_points))















# Real Results from league
# not really sure how we can use this, because there is massive selection bias
tibble(name = c("Alperen Sengun", "Alperen Sengun", "Bobby Portis", "Collin Sexton", "Collin Sexton", "DeMar DeRozan", "Deandre Ayton",
                "Draymond Green", "Draymond Green", "Jalen Suggs", "Jalen Suggs", "Jordan Poole", "Jordan Poole", "Kevin Huerter",
                "Keyonte George", "Kyrie Irving", "Luguentz Dort", "Luguentz Dort", "Luguentz Dort", "Scotty Pippen Jr.", "Scotty Pippen Jr.", "Shai Gilgeous-Alexander",
                "Shai Gilgeous-Alexander", "Shai Gilgeous-Alexander", "Victor Wembanyama"),
       week = c(3, 4, 3, 3, 4, 3, 3, 3, 4, 3, 4, 3, 4, 4, 4, 3, 3, 4, 5, 3, 4, 3, 4, 5, 3),
       real_score = c(36.5, 39, "", "", "", 25.5, 25, 15, 29.5, 4, "", 30, 35, "", "", 32.5, 25, 20.5, "", "", 23, 19, 51.5, 29, 52)) %>% group_by(name, week) %>%
  group_split()

