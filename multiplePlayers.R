## Have data saved globally
library(tidyverse)
library(coda)

priorPuller <- function(player_names) {
  
  return(naiveAlphasBetas %>% filter(str_detect(name, player_names)) %>%
           select(GP, Betas) %>% as.numeric())
  
}

multiPuller <- function(player_names) {
  
  ## Allow for easy input, while also preserving order
  playerIndices <- map_dbl(player_names, ~str_which(naiveAlphasBetas$name, .x))
  print(playerIndices)
  return(naiveAlphasBetas[playerIndices, c(2, 3)] %>% t)
  ## Put Players in Columns
  
}

multiPred <- function(n.iter, playerNames, this_week, gp_week = 0,
                     consistencyParams = multiPuller(playerNames), 
                     hSigma = 1, theSigma = 1, burnIn = 0) {
  
  #####
  # Gather Player Data and Prepare It
  ####
  
  ### Take potentially simplified input, and match it
  
  ## Find Player Data
  matchedNames <- map_chr(playerNames, 
                          ~str_extract(unique(full_data$name), .x))
  return(matchedNames) ## Fix this
  data <- full_data %>% filter(str_detect(name, regexNames), 
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
  gamesPlayed <- length(sleeper_points) ## This has to be changed to account for different players
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
  
  ## Get player data into columns to have it calculate things properly
  ## Second for loop will likely start after calculating hSigma 
  ## Vectorization will literally do god's work
  ## I beleive hSigma will literally require us to change the structure of priorMean
  ## Ideally have input be a list, then squish it down to a vector that presevers order
  ## Then vectorization can take the wheel
  ## Next steps will be by player will proceed how it is now
  ## alpha and beta will depend on the column
  ## gamesPlayed can become a vector, 
  ## All in all, the tricky part will having the correct structures to represent the players. 
  
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
