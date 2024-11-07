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

## 

currentTeam <- df_2025 %>% filter(name %in% team)

sleeperDeviation <- function(player, boxScores, projections, precision = F) {
  
  playerPoints <- boxScores %>% filter(name == player) %>% pull(sleeper_points)
  gamesPlayed <- length(playerPoints)
  
  projectionVec <- (projections %>% filter(name == player) %>%
    pull(sleeper_projection))[1:gamesPlayed]
  theDeviation <- sum((playerPoints - projectionVec)^2)/(gamesPlayed - 1)
  
  if (precision) {
    
    return(1/theDeviation)
    
  } else {
    
    return(theDeviation)
    
  }
  
}
projConsistencies <- map_dbl(team, sleeperDeviation, currentTeam, sleeperProjs)


### Gibbs Sampling
singlePlayerModel <- function(n.iter, data, priorMean, alpha, beta, 
                              thetaStarts, theSigma, hSigma,
                              burnIn = 0) {
  
  ## Initializing Matrices
  gamesPlayed <- length(data)
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
                         sum((data - thetaMat[j - 1, ])^2)/2 + beta)
    
    ## All Thetas 
    currentPrecision <- (1/theSigma[j] + 1/hSigma[j])^(-1)
    thetaMat[j, ] <- map_dbl(data, ~rnorm(1, currentPrecision * 
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

firstSamples <- singlePlayerModel(3000, currentTeam %>% filter(name == team[1]) %>% pull(sleeper_points),
                  priorMean = 26.2, alpha = naiveAlphasBetas$GP[8]/2, beta = naiveAlphasBetas$Betas[8]/2,
                  thetaStarts = rep(20, 7), theSigma = 1, hSigma = 1, 
                  burnIn = 1.5e+4)
firstSamples %>% summary
firstSamples %>% effectiveSize()
firstSamples %>% plot

mean(firstSamples[, 10] >= 22) # Probability that he does as good or outperforms his previous game

## Multiple Priors

singlePlayerMP <- function(n.iter, data, sleepEst, priorMean, alpha, beta, 
                              thetaStarts, theSigma, hSigma,
                              burnIn = 0) {
  
  ## Initializing Matrices
  gamesPlayed <- length(data)
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
                            sum((data - thetaMat[j - 1, ])^2)/2 + beta)
    
    ## All Thetas 
    currentPrecision <- (1/theSigma[j] + 1/hSigma[j])^(-1)
    thetaMat[j, ] <- map_dbl(1:gamesPlayed, ~rnorm(1, currentPrecision * 
                                            (priorMean[.x]/hSigma[j] + data[.x]/theSigma[j]), 
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

multiSamples <- singlePlayerMP(1e+4, currentTeam %>% filter(name == team[1]) %>% pull(sleeper_points),
                                  sleepEst = 26.2, priorMean = (sleeperProjs %>% filter(name == team[1]) %>% pull(sleeper_projection))[1:7],
                                  alpha = naiveAlphasBetas$GP[8]/2, beta = naiveAlphasBetas$Betas[8]/2,
                                  thetaStarts = rep(20, 7), theSigma = 1, hSigma = 1, 
                                  burnIn = 5e+4)
multiSamples %>% summary
multiSamples %>% effectiveSize()
