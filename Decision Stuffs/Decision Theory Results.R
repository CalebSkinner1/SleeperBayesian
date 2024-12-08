# Decision Theory

# load functions
source("Gibbs Sampler Functions.R")
source("Dec_Theory Functions.R")
source("multiplePlayers.R")

# load data
source("Data Manipulation.R")

# tables
library("gt")
library("gtExtras")

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

chain_players <- c("Kevin Huerter", "Scotty Pippen Jr.", "Luguentz Dort", "DeMar DeRozan", "Miles Bridges")
# 40.5 seconds
t <- Sys.time()
multiChain <- multiplePlayers(chain_players, 5)
Sys.time() - t

# players to test
test_players <- c("Kevin Huerter", "Scotty Pippen Jr.", "Luguentz Dort", "DeMar DeRozan", "Miles Bridges")

# this works for one spot
multiChain %>% multi_bid_one(full_data, this_week = 5, chain_players, )

# two spots
multiChain %>% multi_bid_two(full_data, this_week = 5, chain_players)

multiChain %>% multi_bid_two(full_data, this_week = 5, chain_players[-5])

# three spots
t <- Sys.time()
multiChain %>% multi_bid_three(full_data, this_week = 5, chain_players)
Sys.time() - t

# Results - Single Players -----------------------------------------------------------------

# the goal of this is to look through each week for each player and see how the decision boundary rules fair
# I will compare this to the naive boundary and no-lock boundary (always take last score)

# function that moves through decision boundaries and players score to compute what the user
# would have scored
decisions <- function(boundaries, scores){
  df <- boundaries %>%
    add_row(game = max(boundaries$game) + 1, dec_boundary = 0, name = boundaries$name[1], week = boundaries$week[1]) %>%
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
    filter(injured_in_week == 0) %>%
    filter(week > 2) %>%
    select(name, week) %>%
    distinct() %>%
    arrange(week, name)
  
  # convert iterations to list
  player_list <- iterations %>%
    group_by(week) %>%
    summarise(players = list(name)) %>%
    deframe()
  
  players <- iterations$name
  weeks <- iterations$week
  comb <- str_c(players, "/", weeks)
  
  # chains for each player/week iteration
  # old method
  # chains <- map2(players, weeks, ~weekPred(3.5e+4, .x, .y, 0, burnIn = 5e+4))
  
  # new method
  nested_chains <- map(weeks %>% unique(), ~multiPred(5e3, players %>% unique() %>% sort(), .x, "Monday", burnIn = 2.5e3))
  
  chains <- map2(nested_chains, player_list, ~{
      keep <- match(.y, names(.x))
      .x <- .x[keep]}) %>%
    unlist(recursive = FALSE)
  
  # backward induction decision boundaries for each player/week
  bi_dec_boundaries <- map2(chains, comb, ~single_bid(.x) %>%
                              mutate(name = str_remove(.y, "/.*"), week = str_remove(.y, ".*/") %>% as.integer()))
  
  # cumulative density method ~much faster than before :)
  cdf_dec_boundaries <- map2(chains, comb, ~cdf_boundary(.x) %>%
                               mutate(name = str_remove(.y, "/.*"), week = str_remove(.y, ".*/") %>% as.integer()))
  
  # pure expected value method
  ev_dec_boundaries <- full_data %>%
    right_join(iterations, by = join_by(name, week)) %>%
    group_by(week, name) %>%
    arrange(week) %>%
    mutate(dec_boundary = max(sleeper_projection)) %>%
    mutate(
      game = row_number(),
      last_game = max(game)) %>%
    filter(game != last_game) %>% #remove the last game because no dec boundary
    select(name, week, game, dec_boundary) %>%
    relocate(dec_boundary) %>%
    relocate(game) %>%
    group_split()
  
  # Nick Di Method (never lock - ie threshold so high you never lock)
  nd_dec_boundaries <- full_data %>%
    right_join(iterations, by = join_by(name, week)) %>%
    group_by(week, name) %>%
    mutate(dec_boundary = 150) %>%
    mutate(game = row_number(),
           last_game = max(game)) %>%
    filter(game != last_game) %>% #remove the last game because no dec boundary
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
  mutate(week = str_c("week ", week)) %>%
  ggplot() +
  facet_wrap(~week) +
  geom_density(aes(x = final_points, color = method)) +
  labs(x = "Final Points", y = "", title = "Figure 3: One Player for One Spot")

# means
results %>%
  # filter(method %in% c("bi", "ev")) %>%
  # group_by(week, method) %>%
  group_by(method) %>%
  summarize(
    dec_boundary = mean(dec_boundary),
    "final points" = mean(final_points),
    game = mean(game)) %>%
  select(method, "final points") %>%
  mutate(method = recode(method,
                         "bi" = "Backwards Induction",
                         "cdf" = "CDF",
                         "ev" = "Expected Value",
                         "nd" = "Nick Di")) %>%
  gt() %>%
  gt_theme_538() %>%
  fmt_number(decimals = 2) %>%
  tab_header(title = "Table 1: One Player for One Spot")

real_scores %>% left_join(bi_dec_boundaries[[8]], by = join_by(name, week, game)) %>%
  rename(bi_dec_boundary = dec_boundary) %>% 
  left_join(ev_dec_boundaries[[24]], by = join_by(name, week, game)) %>%
  rename(ev_dec_boundary = dec_boundary) %>% 
  drop_na()

# real_scores %>%
#   ggplot() +
#   facet_wrap(~week) +
#   geom_smooth(aes(x = game, y = sleeper_points))

# compare decisions of bi and ev
results %>% filter(method == "bi") %>% rename(bi_final_points = final_points,
                                              bi_dec_boundary = dec_boundary,
                                              bi_game = game) %>%
  select(-method) %>% 
  left_join(
    results %>% filter(method == "ev") %>% rename(ev_final_points = final_points,
                                                  ev_dec_boundary = dec_boundary,
                                                  ev_game = game) %>%
      select(-method),
    by = join_by(name, week)) %>%
  # group_by(week) %>%
  group_by() %>% 
  summarize(
    same = sum(bi_game == ev_game),
    diff = sum(bi_game != ev_game)
  )

# view data
results %>%
  filter(method %in% c("bi", "ev")) %>%
  pivot_wider(names_from = method, values_from = c(final_points, dec_boundary, game))

# proportion of max
prop_max <- full_data %>% right_join(results %>% select(name, week) %>% distinct(), by = join_by(name, week), relationship = "many-to-many") %>%
  group_by(name, week) %>%
  summarize(max_score = max(sleeper_points)) %>%
  right_join(results, by = join_by(name, week)) %>%
  ungroup() %>%
  rowwise() %>%
  mutate(prop_of_max = final_points/max_score) %>%
  ungroup()

# density of prop of max
prop_max %>% ggplot() +
  facet_wrap(~week) +
  geom_density(aes(x = prop_of_max, color = method))

prop_max %>%
  group_by(method) %>%
  summarize(
    correct = mean(prop_of_max == 1))

# is sleeper off? no
full_data %>%
  drop_na() %>%
  filter(sleeper_projection != 0) %>%
  group_by(week) %>%
  summarize(diff = mean(sleeper_projection - sleeper_points))

# Results - Multiple Players ----------------------------------------------

# Decisions function but for multiple players
mult_decisions <- function(boundaries, scores){
  df <- boundaries %>%
    add_row(game = max(boundaries$game) + 1, dec_boundary = 0, players = boundaries$players[1], week = boundaries$week[1]) %>%
    left_join(scores, by = join_by(players, week, game)) %>%
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
mult_final_points_clean <- function(final_points_list){
  map(final_points_list[[1]], ~data.table::rbindlist(.x)) %>%
    data.table::rbindlist() %>%
    as_tibble() %>%
    select(players, week, game, final_points, dec_boundary) %>%
    mutate(method = final_points_list[[2]])
}

# This function computes player results for multiple players and a single spot
mult_player_results <- function(full_data, n_pool_players){
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
    filter(injured_in_week == 0) %>%
    # only keep week 3 and beyond
    filter(week > 2) %>%
    select(name, week) %>%
    distinct() %>%
    arrange(name, week)
  
  # convert iterations to list
  mult <- iterations %>%
    group_by(week) %>%
    reframe(names = list(name))
  
  # all combinations of n_pool_players (for each week)
  test_players <- map2(mult$names, mult$week, ~list(combn(.x, n_pool_players, simplify = FALSE), .y))
  
  # create chain_players (this is all players in chain)
  # w/o week
  player_list <- iterations %>%
    group_by(week) %>%
    summarise(players = list(name)) %>%
    deframe()
  
  # week
  player_weeks <- names(player_list) %>% as_tibble() %>% group_by(value) %>% summarize(value = list(value)) %>% deframe()
  
  # full chain players
  chain_players <- map2(player_list, player_weeks, ~list(.x, .y))
  
  # chains for each player/week iteration
  # old method
  # chains <- map(chain_players, ~map2(.x[[1]], .x[[2]], ~weekPred(3.5e+4, .x, .y, 0, burnIn = 5e+4)))
  
  # new method
  nested_chains <- map(player_weeks, ~multiPred(5e3, iterations %>% select(name) %>% distinct() %>% pull(), .x, "Monday", burnIn = 2.5e3))
  
  chains <- map2(nested_chains, player_list, ~{
    keep <- match(.y, names(.x))
    .x <- .x[keep]})
  
  # backward induction decision boundaries for each player/week
  bi_dec_boundaries <- pmap(list(test_players, chains, chain_players), function(test_players, chains, chain_players){
    # maps over weeks
    t <- test_players
    c <- chains
    cp <- chain_players
    # maps over player combinations in each week
    map2(t[[1]], t[[2]], ~multi_bid_one(c, full_data, .y, cp[[1]], .x) %>%
         mutate(players = str_flatten(.x, collapse = " "),
                week = .y,
                game = row_number()) %>%
           select(players, week, game, dec_boundary)
         )})
  
  # multi_bid_one(chains[[3]], full_data, test_players[[3]][[2]], chain_players[[3]][[1]], test_players[[3]][[1]][[386]])
  
  # cumulative density method
  cdf_dec_boundaries <- pmap(list(test_players, chains, chain_players), function(test_players, chains, chain_players){
    # maps over weeks
    t <- test_players
    c <- chains
    cp <- chain_players
    # maps over player combinations in each week
    map(t[[1]], ~cdf_boundary2(c, cp[[1]], .x) %>%
      mutate(players = str_flatten(.x, collapse = " "),
             week = t[[2]],
             game = row_number()) %>%
        select(players, week, game, dec_boundary))
  })
  
  # pure expected value method
  ev_dec_boundaries <- map(test_players, ~{
    p <- .x[[1]]
    w <- .x[[2]]
    map(p, ~full_data %>%
          filter(name %in% .x,
                 week == w) %>%
          mutate(dec_boundary = max(sleeper_projection),
                 players = str_flatten(.x, collapse = " ")) %>%
          distinct(date, .keep_all = TRUE) %>%
          mutate(game = row_number(),
                 last_game = max(game)) %>%
          filter(game != last_game) %>% #remove the last game because no dec boundary
          select(players, week, game, dec_boundary)
      )
  })

  # Nick Di Method (never lock - ie threshold so high you never lock)
  
  nd_dec_boundaries <- map(test_players, ~{
    p <- .x[[1]]
    w <- .x[[2]]
    map(p, ~full_data %>%
          filter(name %in% .x,
                 week == w) %>%
          mutate(dec_boundary = 150,
                 players = str_flatten(.x, collapse = " ")) %>%
          distinct(date, .keep_all = TRUE) %>%
          mutate(game = row_number(),
                 last_game = max(game)) %>%
          filter(game != last_game) %>% #remove the last game because no dec boundary
          select(players, week, game, dec_boundary))})
  
  # tibble of player's real scores
  real_scores <- map(test_players, ~{
    p <- .x[[1]]
    w <- .x[[2]]
    map(p, ~full_data %>%
          filter(name %in% .x,
                 week == w) %>%
          group_by(date) %>%
          mutate(slpr_proj = max(sleeper_projection),
                 players = str_flatten(.x, collapse = " ")) %>%
          filter(sleeper_projection == slpr_proj) %>%
          ungroup() %>%
          mutate(game = row_number()) %>%
          select(players, week, game, sleeper_points)
    )
  })
  
  # computing final points if using each method
  bi_final_points <- list(map2(bi_dec_boundaries, real_scores, ~map2(.x, .y, ~mult_decisions(.x, .y))), "bi")
  cdf_final_points <- list(map2(cdf_dec_boundaries, real_scores, ~map2(.x, .y, ~mult_decisions(.x, .y))), "cdf")
  ev_final_points <- list(map2(ev_dec_boundaries, real_scores, ~map2(.x, .y, ~mult_decisions(.x, .y))), "ev")
  nd_final_points <- list(map2(nd_dec_boundaries, real_scores, ~map2(.x, .y, ~mult_decisions(.x, .y))), "nd")
  
  # compute results
  multi_results <- map(list(bi_final_points, cdf_final_points, ev_final_points, nd_final_points), ~mult_final_points_clean(.x)) %>%
    data.table::rbindlist() %>%
    as_tibble()
  
  return(multi_results)
}

# for example only (takes less time)
example_data <- full_data %>% filter(name %in% c("Deandre Ayton", "LaMelo Ball", "Bobby Portis", "Brook Lopez", "Jalen Brunson", "LeBron James",
                                                 "Jayson Tatum", "Derrick White", "Shai Gilgeous Alexander", "Jalen Williams")| name %in% team)

t <- Sys.time()
multi_results <- mult_player_results(full_data, 2)
Sys.time() - t

t <- Sys.time()
m5_results <- mult_player_results(example_data, 5)
Sys.time() - t

# graph densities
multi_results %>%
  # filter(method %in% c("bi", "ev")) %>%
  ggplot() +
  # facet_wrap(~week) +
  geom_density(aes(x = final_points, color = method))

# means
multi_results %>%
  # filter(method %in% c("bi", "ev")) %>%
  # group_by(week, method) %>%
  group_by(method) %>%
  summarize(
    dec_boundary = mean(dec_boundary),
    "final points" = mean(final_points),
    game = mean(game)) %>%
  select(method, "final points") %>%
  mutate(method = recode(method,
                         "bi" = "Backwards Induction",
                         "cdf" = "CDF",
                         "ev" = "Expected Value",
                         "nd" = "Nick Di")) %>%
  gt() %>%
  gt_theme_538() %>%
  fmt_number(decimals = 2) %>%
  tab_header(title = "Table 2: Two Players for One Spot")

m5_results %>%
  # filter(method %in% c("bi", "ev")) %>%
  # group_by(week, method) %>%
  group_by(method) %>%
  summarize(
    dec_boundary = mean(dec_boundary),
    "final points" = mean(final_points),
    game = mean(game)) %>%
  select(method, "final points") %>%
  mutate(method = recode(method,
                         "bi" = "Backwards Induction",
                         "cdf" = "CDF",
                         "ev" = "Expected Value",
                         "nd" = "Nick Di")) %>%
  gt() %>%
  gt_theme_538() %>%
  fmt_number(decimals = 2) %>%
  tab_header(title = "Table 5: Five Players for One Spot")

# graph densities
multi_results %>%
  filter(method %in% c("bi", "ev")) %>%
  mutate(week = str_c("week ", week)) %>%
  ggplot() +
  facet_wrap(~week) +
  geom_density(aes(x = final_points, color = method)) +
  labs(x = "Final Points", y = "", title = "Figure 4: Two Players for One Spot")

# which players are consistent
map(chains[[3]], ~mean(.x %>% as_tibble() %>% select("Consistency") %>% pull())) %>% as_tibble() %>%
  pivot_longer(cols = everything(), names_to = "player", values_to = "consistency") %>%
  arrange(desc(consistency)) %>% view()

