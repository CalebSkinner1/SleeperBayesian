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


# probability decision boundary
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




# need iterative(?) function that computes expectation of locking vs holding