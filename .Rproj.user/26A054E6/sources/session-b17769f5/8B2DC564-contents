library("tidyverse")
library("coda")

# load in data
source("Data Manipulation.R")

#### Variance Priors

## Naive Alphas
#### Not Divided by 2 yet :(
naiveAlphasBetas <- df_2024 %>% group_by(name) %>% summarise(GP = length(name), 
                                                             Betas = (GP - 1) * var(sleeper_points)) %>%
  ungroup()

teamParams <- naiveAlphasBetas %>% filter(name %in% team)

## Summary Stats for Prior Standard Deviations
map(map2(teamParams$GP/2, teamParams$Betas/2, 
      ~1/sqrt(rgamma(1e+4, .x, .y))), summary)

# replace currentTeam w current_team (has both projections and points)
# currentTeam <- df_2025 %>% filter(name %in% team)

# sleeperDeviation <- function(player, boxScores, projections, precision = F) {
#   
#   playerPoints <- boxScores %>% filter(name == player) %>% pull(sleeper_points)
#   gamesPlayed <- length(playerPoints)
#   
#   projectionVec <- (projections %>% filter(name == player) %>%
#     pull(sleeper_projection))[1:gamesPlayed]
#   theDeviation <- sum((playerPoints - projectionVec)^2)/(gamesPlayed - 1)
#   
#   if (precision) {
#     
#     return(1/theDeviation)
#     
#   } else {
#     
#     return(theDeviation)
#     
#   }
#   
# }
# projConsistencies <- map_dbl(team, sleeperDeviation, currentTeam, data$sleeper_projection)


### Gibbs Sampling
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

firstSamples <- singlePlayerModel(3000, data = current_team %>% filter(name == team[1]),
                  priorMean = 26.2, alpha = naiveAlphasBetas$GP[8]/2, beta = naiveAlphasBetas$Betas[8]/2,
                  burnIn = 1.5e+4)
firstSamples %>% summary
firstSamples %>% effectiveSize()
firstSamples %>% plot

mean(firstSamples[, 10] >= 22) # Probability that he does as good or outperforms his previous game

## Multiple Priors

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

multiSamples <- singlePlayerMP(1e+4, data = current_team %>% filter(name == team[1]),
                                  alpha = naiveAlphasBetas$GP[8]/2, beta = naiveAlphasBetas$Betas[8]/2,
                                  burnIn = 5e+4)
multiSamples %>% summary
multiSamples %>% effectiveSize()

# Week Function
week_pred <- function(n.iter, data, week_data, alpha, beta, burnIn){
  
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

weekSamples <- week_pred(1e+4, data = current_team %>% filter(name == team[1]),
          week_data = current_team %>% filter(name == team[1]) %>% filter(game_no > 6),
          alpha = naiveAlphasBetas$GP[8]/2, beta = naiveAlphasBetas$Betas[8]/2,
          burnIn = 5e+4)

# initial analysis
weekSamples %>% summary
weekSamples %>% effectiveSize()
weekSamples %>% plot

# function computes probability of exceeding score in future games
prob_decision <- function(mcmc_object, week_data){
  # best score of player thus far
  best_score <- max(week_data$sleeper_points, na.rm = TRUE)
  
  # compute number of samples where future score exceeds current best score
  exceed_prob <- mcmc_object %>% as_tibble() %>%
    rowwise() %>%
    mutate(exceed = max(c_across(contains("newY"))) > best_score) %>%
    group_by() %>%
    summarize(
      exceed_prob = mean(exceed))
  
  return(exceed_prob %>% pull())}

weekSamples %>% prob_decision(current_team %>% filter(name == team[1]) %>% filter(game_no > 6))

# different player because I want to (Sengun)
week_pred(1e+4, data = current_team %>% filter(name == team[5]),
          week_data = current_team %>% filter(name == team[5]) %>% filter(game_no > 6),
          alpha = naiveAlphasBetas$GP[8]/2, beta = naiveAlphasBetas$Betas[8]/2,
          burnIn = 5e+4) %>%
  prob_decision(current_team %>% filter(name == team[5]) %>% filter(game_no > 6))



