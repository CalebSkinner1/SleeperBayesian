# Gibbs Sampler Functions Page
library("tidyverse")
library("coda")

# only prior mean
singlePlayerModel <- function(n.iter, data, priorMean, alpha, beta, 
                              theSigma = 1, hSigma = 1,
                              burnIn = 0) {
  # caleb's prop to make function take in less parameters
  sleeper_points <- na.omit(data$sleeper_points)
  thetaStarts <- rep(20, length(sleeper_points))
  
  ## Initializing Matrices
  gamesPlayed <- length(sleeper_points)
  playerDF <- gamesPlayed - 1
  thetaMat <- matrix(1, nrow = n.iter + burnIn + 1, ncol = gamesPlayed)
  thetaMat[1, ] <- thetaStarts
  # postYs <- matrix(1, nrow = n.iter + burnIn + 1, ncol = gamesPlayed)
  # postYs[1, ] <- data
  newY <- 1
  
  ## Loop 
  
  for (j in 2:(n.iter + burnIn + 1)) {
    
    ## Hierarchical Variance
    hSigma[j] <- 1/rgamma(1, playerDF/2, sum((thetaMat[j - 1, ] - priorMean)^2)/2)
    
    ## Player Consistency
    theSigma[j] <- 1/rgamma(1, gamesPlayed/2 + alpha, 
                            sum((sleeper_points - thetaMat[j - 1, ])^2)/2 + beta)
    
    ## All Thetas 
    currentPrecision <- (1/theSigma[j] + 1/hSigma[j])^(-1)
    thetaMat[j, ] <- map_dbl(sleeper_points, ~rnorm(1, currentPrecision * 
                                                      (priorMean/hSigma[j] + .x/theSigma[j]), 
                                                    sqrt(currentPrecision)))
    
    ## Posterior Predictive Sampling
    newTheta <- rnorm(1, priorMean, sqrt(hSigma[j]))
    newY[j] <- rnorm(1, newTheta, sqrt(theSigma[j]))
    
  }
  ## End for loop
  
  ## Prepare Results
  resMat <- cbind(thetaMat, theSigma, hSigma, newY)
  colnames(resMat)[1:gamesPlayed] <- paste0("theta_", 1:gamesPlayed)
  #colnames(resMat)[(gamesPlayed + 2 + 1):(2 * gamesPlayed + 2)] <- paste0("Game_", 1:gamesPlayed)
  return(mcmc(resMat[-1:(-1 * burnIn - 1), ]))
  
}


# multiple priors

singlePlayerMP <- function(n.iter, data, alpha, beta, 
                           theSigma = 1, hSigma = 1,
                           burnIn = 0) {
  
  ## caleb's prop to make function take in less parameters
  #grabs sleeper points from full data tibble
  sleeper_points <- na.omit(data$sleeper_points)
  gamesPlayed <- length(sleeper_points)
  #grabs projections from data tibble
  priorMean <- data$sleeper_projection[1:gamesPlayed] 
  #initializes thetaStarts
  thetaStarts <- rep(20, gamesPlayed) 
  #grabs estimate for next game
  sleepEst <- data$sleeper_projection[gamesPlayed + 1]
  
  ## Initializing Matrices
  playerDF <- gamesPlayed - 1
  # Add a stop to make shit clear
  if (length(priorMean) != gamesPlayed) stop("Only for multiple priors!\n")
  thetaMat <- matrix(1, nrow = n.iter + burnIn + 1, ncol = gamesPlayed)
  thetaMat[1, ] <- thetaStarts
  # postYs <- matrix(1, nrow = n.iter + burnIn + 1, ncol = gamesPlayed)
  # postYs[1, ] <- data
  newY <- 1
  
  ## Loop 
  
  for (j in 2:(n.iter + burnIn + 1)) {
    
    ## Hierarchical Variance
    hSigma[j] <- 1/rgamma(1, playerDF/2, sum((thetaMat[j - 1, ] - priorMean)^2)/2)
    # All deviations should vectorize :)
    
    ## Player Consistency
    theSigma[j] <- 1/rgamma(1, gamesPlayed/2 + alpha, 
                            sum((sleeper_points - thetaMat[j - 1, ])^2)/2 + beta)
    
    ## All Thetas 
    currentPrecision <- (1/theSigma[j] + 1/hSigma[j])^(-1)
    thetaMat[j, ] <- map_dbl(1:gamesPlayed, ~rnorm(1, currentPrecision * 
                                                     (priorMean[.x]/hSigma[j] + sleeper_points[.x]/theSigma[j]), 
                                                   sqrt(currentPrecision)))
    # Change to index over various prior means
    
    ## Posterior Predictive Sampling
    newTheta <- rnorm(1, sleepEst, sqrt(hSigma[j])) # New Est
    newY[j] <- rnorm(1, newTheta, sqrt(theSigma[j]))
    
  }
  ## End for loop
  
  ## Prepare Results
  resMat <- cbind(thetaMat, theSigma, hSigma, newY)
  colnames(resMat)[1:gamesPlayed] <- paste0("theta_", 1:gamesPlayed)
  #colnames(resMat)[(gamesPlayed + 2 + 1):(2 * gamesPlayed + 2)] <- paste0("Game_", 1:gamesPlayed)
  return(mcmc(resMat[-1:(-1 * burnIn - 1), ]))
  
}

# week function

week_pred <- function(n.iter, data, this_week, gp_week, alpha, beta,
                      hSigma = 1, theSigma = 1, burnIn = 0){
  
  # select week data, only uncompleted games
  week_data <- data %>% filter(week == this_week) %>%
    mutate(sleeper_points = NA) %>% 
    # remove first gp_week games
    slice((gp_week+1):n())
  
  # slice is weird, so correct
  total_games <- data %>% filter(week == this_week) %>% nrow()
  if(total_games == gp_week){
    stop("Must have unplayed games remaining in week")
    }

  # use only data from up to that week
  data <- data %>% filter(week < this_week + 1) %>%
    # only take uncompleted games from data
    anti_join(week_data, by = join_by(game_no))
  
  #grabs sleeper points from full data tibble
  sleeper_points <- na.omit(data$sleeper_points)
  gamesPlayed <- length(sleeper_points)
  #grabs projections from data tibble
  priorMean <- data$sleeper_projection[1:gamesPlayed] 
  #initializes thetaStarts
  thetaStarts <- rep(20, gamesPlayed)
  # grabs estimates for each game remaining in the week
  sleepEst <- week_data %>% filter(is.na(sleeper_points)) %>% select(sleeper_projection) %>% pull()
  
  ## Initializing Matrices
  playerDF <- gamesPlayed - 1
  # Add a stop to make shit clear
  if (length(priorMean) != gamesPlayed) stop("Only for multiple priors!\n")
  thetaMat <- matrix(1, nrow = n.iter + burnIn + 1, ncol = gamesPlayed)
  thetaMat[1, ] <- thetaStarts
  # postYs <- matrix(1, nrow = n.iter + burnIn + 1, ncol = gamesPlayed)
  # postYs[1, ] <- data
  newY <- matrix(1, nrow = n.iter + burnIn + 1, ncol = length(sleepEst))
  
  ## Loop 
  
  for (j in 2:(n.iter + burnIn + 1)) {
    
    ## Hierarchical Variance
    hSigma[j] <- 1/rgamma(1, playerDF/2, sum((thetaMat[j - 1, ] - priorMean)^2)/2)
    # All deviations should vectorize :)
    
    ## Player Consistency
    theSigma[j] <- 1/rgamma(1, gamesPlayed/2 + alpha, 
                            sum((sleeper_points - thetaMat[j - 1, ])^2)/2 + beta)
    
    ## All Thetas 
    currentPrecision <- (1/theSigma[j] + 1/hSigma[j])^(-1)
    thetaMat[j, ] <- map_dbl(1:gamesPlayed, ~rnorm(1, currentPrecision * 
                                                     (priorMean[.x]/hSigma[j] + sleeper_points[.x]/theSigma[j]), 
                                                   sqrt(currentPrecision)))
    # Change to index over various prior means
    
    ## Posterior Predictive Sampling
    newTheta <- map(sleepEst, ~rnorm(1, ., sqrt(hSigma[j]))) # New Estimates
    newY[j, ] <- map_dbl(newTheta, ~rnorm(1, ., sqrt(theSigma[j])))
  }
  ## End for loop
  
  ## Prepare Results
  
  
  
  resMat <- cbind(thetaMat, theSigma, hSigma, newY)
  colnames(resMat)[1:gamesPlayed] <- paste0("theta_", 1:gamesPlayed)
  colnames(resMat)[(gamesPlayed + 3):ncol(resMat)] <- paste0("newY_", (gamesPlayed+1):(gamesPlayed + length(sleepEst)))
  
  #colnames(resMat)[(gamesPlayed + 2 + 1):(2 * gamesPlayed + 2)] <- paste0("Game_", 1:gamesPlayed)
  return(mcmc(resMat[-1:(-1 * burnIn - 1), ]))
  
}


# probability to exceed
prob_decision <- function(mcmc_object, week_data = NULL, best_score = NULL, remaining_games = NULL){
  # enter best_score for specific integer or week_data for function to directly compute
  
  # remaining games for player in week
  if(is.null(remaining_games)){
    if(is.null(week_data)){
      stop("enter data")
      remaining_games <- sum(is.na(week_data$sleeper_points))
    }}
  
  
  # best score of player thus far
  if(is.null(best_score)){
    if(is.null(week_data)){
      stop("enter data")
    best_score <- max(week_data$sleeper_points, na.rm = TRUE)
    }}
  
  # compute max remaining score
    df <- mcmc_object %>% as_tibble() %>%
      # select only unplayed games
      select(tail(names(.), remaining_games)) %>%
      rowwise() %>%
      mutate(max_score = max(c_across(contains("newY"))))
    
    # cdf
    cdf <- tibble(
      score = best_score,
      exceed_prob = best_score %>% map_dbl(~mean(df$max_score > .x)))
  return(cdf)}

# probability decision boundary
cdf_boundary <- function(mcmc_object){
  # enter best_score for specific integer or week_data for function to directly compute

  df <- mcmc_object %>% as_tibble() %>%
    select(contains("newY"))
  
  total_games <- df %>% ncol()
  games_it <- seq(1, total_games - 1, by=1)
  
  # compute median max remaining score
  cdf <- map(games_it, ~df %>%
        select(tail(names(.), total_games - .x)) %>%
        rowwise() %>%
        mutate(max_score = max(c_across(everything()))) %>%
        ungroup() %>%
        group_by() %>%
        summarize(dec_boundary = median(max_score)) %>%
        mutate(game = .x) %>%
        relocate(game)) %>%
    data.table::rbindlist() %>%
    as_tibble()
  
  return(cdf)}


# Daniel's New and Improved -----------------------------------------------

# pulls priors
priorPuller <- function(player_names) {
  
  return(naiveAlphasBetas %>% filter(name %in% player_names) %>%
           select(GP, Betas) %>% as.numeric())
  
}


weekPred <- function(n.iter, playerName, this_week, gp_week = 0,
                     consistencyParams = priorPuller(playerName), 
                     hSigma = 1, theSigma = 1, burnIn = 0) {
  
  #####
  # Gather Player Data and Prepare It
  ####
  
  ## Find Player Data
  data <- full_data %>% filter(str_detect(name, playerName), 
                               week <= this_week) 
  
  ## Week Safety Check
  currentWeek <- which(data$week == this_week)
  if(gp_week >= length(currentWeek) | gp_week < 0) stop("Must have unplayed games remaining in week")
  ## gp_week acts as an index on currentWeek
  
  ## Convert to NA
  data$sleeper_points[currentWeek] <- NA
  
  #####
  # Prepare for Gibbs
  #####
  
  #grabs sleeper points from full data tibble
  sleeper_points <- na.omit(data$sleeper_points) # Removes all references to this_week
  gamesPlayed <- length(sleeper_points)
  playerDF <- gamesPlayed - 1
  
  #grabs projections from data tibble
  priorMean <- data$sleeper_projection[1:gamesPlayed]
  ### Add a stop to make shit clear
  if (length(priorMean) != gamesPlayed) stop("Only for multiple priors!\n")
  
  # Set Consistency Priors
  alpha <- (consistencyParams[1] - 1)/2
  beta <- (consistencyParams[2])/2
  
  #initializes thetaStarts
  thetaStarts <- rep(20, gamesPlayed)
  
  # grabs estimates for each game remaining in the week
  if (gp_week == 0) {
    
    sleepEsts <- data$sleeper_projection[currentWeek]
    
  } else {
    
    sleepEsts <- data$sleeper_projection[currentWeek[-1:-gp_week]]
    
  } 
  # End if
  
  ## Initializing Matrices
  thetaMat <- matrix(1, nrow = n.iter + burnIn + 1, ncol = gamesPlayed)
  thetaMat[1, ] <- thetaStarts
  newY <- matrix(1, nrow = n.iter + burnIn + 1, ncol = length(sleepEsts))
  
  ## Loop 
  
  for (j in 2:(n.iter + burnIn + 1)) {
    
    ## Hierarchical Variance
    hSigma[j] <- 1/rgamma(1, playerDF/2, sum((thetaMat[j - 1, ] - priorMean)^2)/2)
    # All deviations should vectorize :)
    
    ## Player Consistency
    theSigma[j] <- 1/rgamma(1, gamesPlayed/2 + alpha, 
                            sum((sleeper_points - thetaMat[j - 1, ])^2)/2 + beta)
    
    ## All Thetas 
    currentPrecision <- (1/theSigma[j] + 1/hSigma[j])^(-1)
    thetaMat[j, ] <- map_dbl(1:gamesPlayed, ~rnorm(1, currentPrecision * 
                                                     (priorMean[.x]/hSigma[j] + sleeper_points[.x]/theSigma[j]), 
                                                   sqrt(currentPrecision)))
    # Change to index over various prior means
    
    ## Posterior Predictive Sampling
    newTheta <- map_dbl(sleepEsts, ~rnorm(1, .x, sqrt(hSigma[j]))) # New Estimates
    newY[j, ] <- map_dbl(newTheta, ~rnorm(1, .x, sqrt(theSigma[j])))
    
  }
  ## End for loop
  
  ## Prepare Results
  
  resMat <- cbind(thetaMat, theSigma, hSigma, newY)
  colnames(resMat)[1:gamesPlayed] <- paste0("theta_", 1:gamesPlayed)
  colnames(resMat)[(gamesPlayed + 3):ncol(resMat)] <- paste0("newY_", (gp_week + 1):length(currentWeek))
  
  #colnames(resMat)[(gamesPlayed + 2 + 1):(2 * gamesPlayed + 2)] <- paste0("Game_", 1:gamesPlayed)
  return(mcmc(resMat[-1:(-1 * burnIn - 1), ]))
  
}

# takes multiple players
multiplePlayers <- function(players, thisWeek, lastDayPlayed = 0) {
  
  return(map(players, ~weekPred(5e+4, .x, thisWeek, burnIn = 5e+4)))
  
}



# need iterative(?) function that computes expectation of locking vs holding