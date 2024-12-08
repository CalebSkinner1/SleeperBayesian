## Have data saved globally
library(tidyverse)
library(coda)

priorPuller <- function(player_names) {
  
  return(naiveAlphasBetas %>% filter(str_detect(name, player_names)) %>%
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
  priorMean <- data$sleeper_projection[1:gamesPlayed] ## Aint this an error
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
    hSigma[j] <- 1/rgamma(1, (gamesPlayed)/2 + 1, sum((thetaMat[j - 1, ] - priorMean)^2)/2 + 1)
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

testPlayer <- weekPred(3.5e+4, "Shai Gilgeous-Alexander", 5, 1, burnIn = 5e+4)
summary(testPlayer)
traceplot(testPlayer)
plot(testPlayer[, 17])
plot(density(exp(testPlayer[, 17])))
exp(testPlayer[, 17]) %>% HPDinterval()

## Dort Example
dortSim <- weekPred(5e+4, "Luguentz Dort", 5, 1, burnIn = 5e+4)

multiplePlayers <- function(players, thisWeek, lastDayPlayed = 0) {
  
  return(map(players, ~weekPred(5e+4, .x, thisWeek, burnIn = 5e+4)))
  
}

# test
testPlayers <- c("Kevin Huerter", "Pippen", "Dort", "DeRozan", "Miles Bridges")
multiChain <- multiplePlayers(testPlayers, 5)
postYs <- do.call(cbind, multiChain)
postYs <- postYs[, str_detect(colnames(postYs), "newY_[^1]")]
apply(postYs, 2, function(x) return(mean(x >= 22))) # How much often is each greater than 23
postYs <- cbind(postYs, 17, 23, 22, 0, 17)
rankYs <- apply(postYs, 1, rank) %>% apply(1, median)
pippenEsts <- weekPred(5e+4, "Pippen", 5, 1, burnIn = 5e+4)
pippenEsts[, 17:18] %>% summary 
## Interesting, if he is popping off it'll be tonight
## probably not next game. The players most likely to beat his score are DeRozan and Bridges
## So I should probably lock. 
## Doesn't consider that DeRozan is out
