# Decision Theory

# load functions
source("Gibbs Sampler Functions.R")

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
  prob_decision(best_score = x, remaining_games = 3, week_data = NULL) %>%
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
single_bid <- function(mcmc_object){
  df <- mcmc_object %>% as_tibble() %>%
    select(contains("newY"))
  
  # number of games to be estimated
  games <- ncol(df)
  
  # initialize tibble to store data
  dec_boundary <- tibble()
  
  # mean posterior for last game
  mean <- df[[games]] %>% mean()
  for(i in games:2){
    # add mean value to decision boundary
    dec_boundary <- tibble(game = i - 1, dec_boundary = mean) %>%
      bind_rows(dec_boundary)
    
    # compute T (expected score after game i-1)
    # this code is way to complex because I can't think of a simpler way to do it
    new_T <- df[i-1] %>%
      rename_with(~paste0("y"), contains("Y")) %>%
      rowwise() %>%
      # user keeps score if greater than mean for next game
      mutate(t = max(y, mean)) %>%
      select(t) %>% pull()
    
    # compute new mean
    mean <- new_T %>% mean()
  }
  return(dec_boundary)
  
}


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

# thus far, this only works for two spots
multi_bid <- function(mcmc_object, data, this_week, testPlayers){
  # make chain malleable
  malleable_chain <- map(mcmc_object, ~.x %>% as_tibble() %>% select(contains("newY")) %>% as.mcmc())
  
  # player's order in mcmc_object
  player_order <- testPlayers %>% as_tibble() %>% mutate(order = row_number()) %>% rename(name = value)
  
  df <- full_data %>% filter(week == this_week, name %in% testPlayers) %>%
    # need to identify player order to grab from multiChain
    left_join(player_order, by = join_by(name))
  
  # last day in week
  last_day <- max(df$date)
  
  dec_boundaries <- tibble()
  
  # Have to do last day first, because it is slightly different
  
  # grab players playing on last day
  players_last_day <- df %>% filter(date == last_day) %>%
    select(order) %>% pull()
  
  # chains for last day
  values_last_day <- malleable_chain[players_last_day]
  # compute means for each chain
  mean <- map_dbl(values_last_day, ~.x %>% as.data.frame() %>% select(last_col()) %>% pull() %>% mean)
  
  # remove these values so chain can continue to grab the last column
  malleable_chain[players_last_day] <- map(values_last_day, ~.x %>% as_tibble() %>% select(-last_col()) %>% as.mcmc())
  
  # grab largest value (this guy is starting)
  ET_1 <- max(mean)
  # grab second largest value (this guy is also starting)
  
  if(length(mean) == 1){
    ET_2 <- 0 # in case one player played in last day
  } else{
    ET_2 <- sort(mean,partial=length(mean)-1)[length(mean)-1]
  }
  
  # decision boundary if only one spot is remaining
  L_1 <- ET_1
  
  # add to tibble
  dec_boundaries <- tibble(day = last_day - 1, dec_boundary1 = ET_2, dec_boundary2 = ET_1, locked_dec_boundary = L_1) %>%
    bind_rows(dec_boundaries)
  
  # for loop across each day (7 days in a week, minus weird last day, so 5 decisions)
  for(i in 5:1){
    # grab players playing on (i+1)th day
    players_day_ahead <- df %>% filter(date == last_day + i - 6) %>%
      select(order) %>% pull()
    
    # if no players this day
    if(length(players_day_ahead) == 0){ next}
    
    # chains for (i+1)th day
    values_day_ahead <- malleable_chain[players_day_ahead]
    
    # probability that will exceed decision boundary
    P <- map_dbl(values_day_ahead, ~mean(.x %>% as.data.frame() %>%
                                select(last_col()) %>% pull()>ET_2)) %>%
      as_tibble() %>%
      mutate(player = players_day_ahead) %>%
      arrange(desc(value))
    # starters -  will need to expand this for more than 2 spots later
    S1 <- P %>% slice(1) %>% select(player) %>% pull()
    
    if(length(values_day_ahead) == 1){
      S_2 <- 0 # in case one player played in last day
    } else{
      S2 <- P %>% slice(2) %>% select(player) %>% pull()
    }
    
    # compute T1
    T1 <- malleable_chain[S1] %>% as.data.frame() %>%
      select(last_col()) %>% rowwise() %>%
      rename_with(~paste0("y"), starts_with("newY")) %>%
      mutate(max = max(y, ET_2)) %>%
      select(max) %>% pull()
    
    # compute T2
    if(length(values_day_ahead) == 1){
      T2 <- ET_1 # in case one player played in last day
    } else{
      T2 <- malleable_chain[S2] %>% as.data.frame() %>%
        select(last_col()) %>% rowwise() %>%
        rename_with(~paste0("y"), starts_with("newY")) %>%
        mutate(max = max(y, ET_1)) %>%
        select(max) %>% pull()
    }
    
    # order
    Ts <- tibble(T1 = T1, T2 = T2) %>%
      rowwise() %>%
      mutate(
        T_1 = max(T1, T2),
        T_2 = min(T1, T2)) %>%
      group_by() %>%
      summarize(
        ET_1 = mean(T_1),
        ET_2 = mean(T_2))
    ET_1 <- Ts$ET_1
    ET_2 <- Ts$ET_2
    
    # compute locked boundary
    L_1 <- malleable_chain[S1] %>% as.data.frame() %>%
      select(last_col()) %>% rowwise() %>%
      rename_with(~paste0("y"), starts_with("newY")) %>%
      mutate(max = max(y, L_1)) %>%
      select(max) %>% pull() %>% mean()
    
    # remove these values so chain can continue to grab the last column
    malleable_chain[players_day_ahead] <- map(values_day_ahead, ~.x %>% as_tibble() %>% select(-last_col()) %>% as.mcmc())
    
    # add to tibble
    dec_boundaries <- tibble(day = last_day + i - 7, dec_boundary1 = ET_2, dec_boundary2 = ET_1, locked_dec_boundary = L_1) %>%
      bind_rows(dec_boundaries)
  }
  
  return(dec_boundaries)
  
}

# BOOM
multiChain %>% multi_bid(full_data, this_week = 5, testPlayers)
















