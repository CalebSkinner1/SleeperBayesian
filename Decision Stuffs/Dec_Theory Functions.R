# Decision Theory Functions

# single bid - single player
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

single_bid_expand <- function(n.iter, playerName, this_week, gp_week = 0,
                               consistencyParams = priorPuller(playerName), 
                               hSigma = 1, theSigma = 1, burnIn = 0){
  weekPred(n.iter, playerName, this_week, gp_week = 0,
           consistencyParams = priorPuller(playerName), 
           hSigma = 1, theSigma = 1, burnIn = 0) %>%
    single_bid()
}

# this works for one spot
multi_bid_one <- function(mcmc_object, data, this_week, chain_players, test_players){
  # function needs to know the players in the chain, and the order that they are in (chain_players)
  # It also needs to know the players that we want tested (test_players)
  
  # make chain malleable
  malleable_chain <- map(mcmc_object, ~.x %>% as_tibble() %>% select(contains("newY")) %>% as.mcmc())
  
  # player's order in mcmc_object
  # this needs to be chain_players not test_players! (it needs to order the players in the big boy chain)
  player_order <- chain_players %>% as_tibble() %>% mutate(order = row_number()) %>% rename(name = value) 
  
  df <- full_data %>% filter(week == this_week, name %in% test_players) %>%
    # need to identify player order to grab from mcmc_object
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
  
  # add to tibble
  dec_boundaries <- tibble(day = last_day - 1, dec_boundary = ET_1) %>%
    bind_rows(dec_boundaries)
  
  first_day <- min(df$date)
  
  for_length <- interval(first_day, last_day) %/% days(1) - 1
  if(for_length == 0){
    return(dec_boundaries)
  }
  
  # for loop across each day (7 days in a week, minus weird last day, so 5 decisions)
  for(i in for_length:1){
    # grab players playing on (i+1)th day
    players_day_ahead <- df %>% filter(date == last_day + i - 1 - for_length) %>%
      select(order) %>% pull()
    
    # if no players this day
    if(length(players_day_ahead) == 0){ next}
    
    # chains for (i+1)th day
    values_day_ahead <- malleable_chain[players_day_ahead]
    
    # probability that will exceed decision boundary
    P <- map_dbl(values_day_ahead, ~mean(.x %>% as.data.frame() %>%
                                           select(last_col()) %>% pull()>ET_1)) %>%
      as_tibble() %>%
      mutate(player = players_day_ahead) %>%
      arrange(desc(value))
    # starters
    S1 <- P %>% slice(1) %>% select(player) %>% pull()
    
    # compute T1 (expected total score after date)
    T1 <- malleable_chain[S1] %>% as.data.frame() %>%
      select(last_col()) %>%
      rename_with(~paste0("y"), contains("newY")) %>%
      rowwise() %>%
      mutate(max = max(y, ET_1)) %>%
      select(max) %>% pull()
    
    # compute mean (expected total score if date is unknown)
    ET_1 <- mean(T1)
    
    # remove these values so chain can continue to grab the last column
    malleable_chain[players_day_ahead] <- map(values_day_ahead, ~.x %>% as_tibble() %>% select(-last_col()) %>% as.mcmc())
    
    # add to tibble
    dec_boundaries <- tibble(day = last_day + i - 7, dec_boundary = ET_1) %>%
      bind_rows(dec_boundaries)
  }
  
  return(dec_boundaries)
  
}

# two spots
multi_bid_two <- function(mcmc_object, data, this_week, chain_players, test_players){

  # make chain malleable
  malleable_chain <- map(mcmc_object, ~.x %>% as_tibble() %>% select(contains("newY")) %>% as.mcmc())
  
  # player's order in mcmc_object
  player_order <- test_players %>% as_tibble() %>% mutate(order = row_number()) %>% rename(name = value)
  
  df <- full_data %>% filter(week == this_week, name %in% test_players) %>%
    # need to identify player order to grab from multiChain
    left_join(player_order, by = join_by(name))
  
  # last day in week
  last_day <- max(df$date)
  
  dec_boundary1 <- tibble()
  
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
  
  # add to tibble
  dec_boundary1 <- tibble(day = last_day - 1, dec_boundary = ET_2, locked_players = "NONE") %>%
    bind_rows(dec_boundary1)
  
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
    # starters
    S1 <- P %>% slice(1) %>% select(player) %>% pull()
    
    if(length(values_day_ahead) == 1){
      S_2 <- 0 # in case one player played in last day
    } else{
      S2 <- P %>% slice(2) %>% select(player) %>% pull()
    }
    
    # compute T1
    T1 <- malleable_chain[S1] %>% as.data.frame() %>%
      select(last_col()) %>% rowwise() %>%
      rename_with(~paste0("y"), contains("newY")) %>%
      mutate(max = max(y, ET_2)) %>%
      select(max) %>% pull()
    
    # compute T2
    if(length(values_day_ahead) == 1){
      T2 <- ET_1 # in case one player played in last day
    } else{
      T2 <- malleable_chain[S2] %>% as.data.frame() %>%
        select(last_col()) %>% rowwise() %>%
        rename_with(~paste0("y"), contains("newY")) %>%
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
    
    # remove these values so chain can continue to grab the last column
    malleable_chain[players_day_ahead] <- map(values_day_ahead, ~.x %>% as_tibble() %>% select(-last_col()) %>% as.mcmc())
    
    # add to tibble
    dec_boundary1 <- tibble(day = last_day + i - 7, dec_boundary = ET_2, locked_players = "NONE") %>%
      bind_rows(dec_boundary1)
  }
  
  # second boundary
  # make players a list
  players <- test_players %>% as.list()
  
  # create list of all possible combinations of n-1 players (if one player is locked, these are the possible combinations of n-1 players left)
  tp <- combn(players, length(players) - 1) %>% as.data.frame() %>% as.list()
  
  # players that have been locked
  missing <- map(tp, ~setdiff(players, .x)) %>% as.data.frame() %>% as.list()
  
  # iterate through and compute the second decision boundary for each possibility of a locked player
  dec_boundaries <- map(tp, ~multi_bid_one(mcmc_object, full_data, this_week, chain_players, .x)) %>%
    map2(missing, ~.x %>% mutate(locked_players = .y)) %>%
    data.table::rbindlist() %>%
    as_tibble() %>%
    bind_rows(dec_boundary1) %>%
    arrange(desc(day)) %>%
    relocate(locked_players, .before = dec_boundary)
  
  return(dec_boundaries)
  
}

# three spots
multi_bid_three <- function(mcmc_object, data, this_week,chain_players, test_players){
  # first boundary
  
  # make chain malleable
  malleable_chain <- map(mcmc_object, ~.x %>% as_tibble() %>% select(contains("newY")) %>% as.mcmc())
  
  # player's order in mcmc_object
  player_order <- test_players %>% as_tibble() %>% mutate(order = row_number()) %>% rename(name = value)
  
  df <- full_data %>% filter(week == this_week, name %in% test_players) %>%
    # need to identify player order to grab from multiChain
    left_join(player_order, by = join_by(name))
  
  # last day in week
  last_day <- max(df$date)
  
  dec_boundary1 <- tibble()
  
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
  
  # grab third largest value (this guy is also starting)
  if(length(mean) < 3){
    ET_3 <- 0 # in case less than three players played in last day
  } else{
    ET_3 <- sort(mean,partial=length(mean)-2)[length(mean)-2]
  }
  
  
  # add to tibble
  dec_boundary1 <- tibble(day = last_day - 1, dec_boundary = ET_3, lock1 = "NONE", lock2 = "NONE") %>%
    bind_rows(dec_boundary1)
  
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
    # starters
    S1 <- P %>% slice(1) %>% select(player) %>% pull()
    
    if(length(values_day_ahead) == 1){
      S_2 <- 0 # in case one player played in last day
    } else{
      S2 <- P %>% slice(2) %>% select(player) %>% pull()
    }
    
    if(length(values_day_ahead) < 3){
      S_3 <- 0 # in case one player played in last day
    } else{
      S3 <- P %>% slice(3) %>% select(player) %>% pull()
    }
    
    # compute T1
    T1 <- malleable_chain[S1] %>% as.data.frame() %>%
      select(last_col()) %>% rowwise() %>%
      rename_with(~paste0("y"), contains("newY")) %>%
      mutate(max = max(y, ET_3)) %>%
      select(max) %>% pull()
    
    # compute T2
    if(length(values_day_ahead) == 1){
      T2 <- ET_2 # in case one player played in last day
    } else{
      T2 <- malleable_chain[S2] %>% as.data.frame() %>%
        select(last_col()) %>% rowwise() %>%
        rename_with(~paste0("y"), contains("newY")) %>%
        mutate(max = max(y, ET_2)) %>%
        select(max) %>% pull()
    }
    
    # compute T3
    if(length(values_day_ahead) < 3){
      T3 <- ET_1 # in case less than three players played in last day
    } else{
      T3 <- malleable_chain[S3] %>% as.data.frame() %>%
        select(last_col()) %>% rowwise() %>%
        rename_with(~paste0("y"), contains("newY")) %>%
        mutate(max = max(y, ET_1)) %>%
        select(max) %>% pull()
    }
    
    # order the Ts
    Ts <- tibble(T1 = T1, T2 = T2, T3 = T3) %>%
      rowwise() %>%
      mutate(
        T_1 = max(T1, T2, T3),
        T_2 = Rfast::nth(c(T1, T2, T3), 2, descending = TRUE),
        T_3 = min(T1, T2, T3)) %>%
      group_by() %>%
      summarize(
        ET_1 = mean(T_1),
        ET_2 = mean(T_2),
        ET_3 = mean(T_3))
    
    ET_1 <- Ts$ET_1
    ET_2 <- Ts$ET_2
    ET_3 <- Ts$ET_3
    
    # remove these values so chain can continue to grab the last column
    malleable_chain[players_day_ahead] <- map(values_day_ahead, ~.x %>% as_tibble() %>% select(-last_col()) %>% as.mcmc())
    
    # add to tibble
    dec_boundary1 <- tibble(day = last_day + i - 7, dec_boundary = ET_3, lock1 = "NONE", lock2 = "NONE") %>%
      bind_rows(dec_boundary1)
  }
  
  # next boundary
  # make players a list
  players <- test_players
  
  # create list of all possible combinations of n-1 players (if one player is locked, these are the possible combinations of n-1 players left)
  tp <- combn(players, length(players) - 1) %>% as.data.frame() %>% as.list()
  
  # players that have been locked
  missing <- map(tp, ~setdiff(players, .x)) %>% as.data.frame() %>% as.list()
  
  # iterate through and compute the second decision boundary for each possibility of a locked player
  dec_boundaries <- map(tp, ~multi_bid_two(mcmc_object, full_data, this_week, chain_players, .x)) %>%
    map2(players, ~.x %>% mutate(lock1 = .y)) %>%
    data.table::rbindlist() %>%
    as_tibble() %>%
    rename(lock2 = "locked_players") %>%
    bind_rows(dec_boundary1) %>%
    arrange(day) %>%
    relocate(lock1, .before = dec_boundary) %>%
    relocate(lock2, .before = dec_boundary) %>%
    distinct() %>%
    # remove fake problems where players are locked
    filter(lock1 != lock2 | lock1 == "NONE")
  
  return(dec_boundaries)
  
}
