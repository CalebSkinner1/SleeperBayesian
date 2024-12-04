## Have data saved globally
library(tidyverse)
library(coda)

## Functions. Some only work inside the big function
multiPuller <- function(player_names) {
  
  ## Allow for easy input, while also preserving order
  playerIndices <- map_dbl(player_names, ~str_which(naiveAlphasBetas$name, .x))
  return(naiveAlphasBetas[playerIndices, c(2, 3)] %>% t)
  ## Put Players in Columns
  
}

nameCorrecter <- function(player_names) {
  
  playerIndices <- map_dbl(player_names, ~str_which(naiveAlphasBetas$name, .x))
  correctedNames <- naiveAlphasBetas$name[playerIndices]
  return(correctedNames)
  
}

pullPoints <- function(player_name, data) {
  
  playerIndex <- which(data$name == player_name & !is.na(data$sleeper_points)) ## Get Index
  return(data$sleeper_points[playerIndex]) ## Pull points
  
}

pullPriors <- function(player_name, data) {
  
  playerIndex <- which(data$name == player_name & !is.na(data$sleeper_points)) ## Get Index
  return(data$sleeper_projection[playerIndex]) ## Pull points
  
}

fantasyWD <- function(dateVec) {
  
  theWD <- c("Monday", "Tuesday", "Wednesday", 
             "Thursday", "Friday", "Saturday", "Sunday")
  wdVec <- factor(weekdays(dateVec), levels = theWD)
  return(as.numeric(wdVec))
  
}

pullPredictions <- function(player_name, curWeek, dotw, data) {
  
  playerIndex <- which(data$name == player_name  & data$week == curWeek & 
                         fantasyWD(data$date) >= dotw) ## Get Index
  return(data$sleeper_projection[playerIndex])
  
}

paramCreate <- function(numVec) {
  
  return(c((numVec[1] - 1)/2, numVec[2]/2))
  
}

mcmcPrep <- function(theMatrix, indices) {
  
  return(mcmc(theMatrix[indices, ]))
  
}

dayFinder <- function(player_name, curWeek, dotw, data) {
  
  playerIndex <- which(data$name == player_name & data$week == curWeek & 
                         fantasyWD(data$date) >= dotw)
  return(as.character(factor(fantasyWD(data$date[playerIndex]), levels = 1:7, 
         labels = c("Monday", "Tuesday", "Wednesday", "Thursday", 
                    "Friday", "Saturday", "Sunday"))))
  
}

multiPred <- function(n.iter, playerNames, this_week, dotw = c("Monday", "Tuesday", "Wednesday", 
                                                               "Thursday", "Friday", "Saturday", "Sunday"),
                     consistencyParams = multiPuller(playerNames), 
                     burnIn = 0) {
  
  # hSigma and all normalSigmas start at 1
  
  #####
  # Gather Player Data and Prepare It
  ####
  
  ## Day of the Week Input
  ## Global Var
  theWD <- c("Monday", "Tuesday", "Wednesday", 
             "Thursday", "Friday", "Saturday", "Sunday")
  dotw <- as.numeric(factor(match.arg(dotw, theWD), levels = theWD))
  
  ### Take potentially simplified input, and match it
  
  ## Find Player Data
  matchedNames <- nameCorrecter(playerNames)
  data <- full_data %>% filter(name %in% matchedNames, 
                               week <= this_week) ## Works as expected
  ## Adaptive Week Method
  currentWeek <- which(data$week == this_week & fantasyWD(data$date) >= dotw)
  data$sleeper_points[currentWeek] <- NA
  ### Check which players have NAs
  matchedNames <- data %>% filter(week == this_week, 
                             fantasyWD(date) >= dotw) %>%
    pull(name) %>% factor(levels = matchedNames) %>% droplevels %>% levels
  
  #####
  # Prepare for Gibbs
  #####
  
  #grabs sleeper points from full data tibble
  sleeperPoints <- map(matchedNames, pullPoints, data)
  
  ## Gets total gamesPlayed in a vector
  gamesPlayed <- map_dbl(sleeperPoints, length)
  gameDays <- map(matchedNames, dayFinder, this_week, dotw, data)
  playerDF <- gamesPlayed - 1
  ## Both of these are vectors of length of playerNames
  
  #grabs projections from data tibble
  priorMean <- map(matchedNames, pullPriors, data)
  ### Recreating stop to make shit clear
  clearCheck <- map_lgl(seq_along(matchedNames), ~gamesPlayed[.x] == length(priorMean[[.x]]))
  if (!all(clearCheck)) stop("Only for multiple priors!\n")
  
  # Set Consistency Priors
  allParams <- apply(consistencyParams, 2, paramCreate)
  alphas <- allParams[1, ]
  betas <- allParams[2, ]
  
  # grabs estimates for each game remaining in the week
  sleepEsts <- map(matchedNames, pullPredictions, this_week, dotw, data)
  totalEsts <- map_dbl(sleepEsts, length)
  
  #### Parameter Checklist
  # alphas and betas is a vector
  # gamesPlayed and playerDF is also a vector
  # sleeperPoints is a list of vectors
  # sleepEsts and priorMean are a list of vectors
  
  ## Initializing Results Matrix
  ## thetas as a list of matrices
  
  thetaList <- map(seq_along(matchedNames), ~matrix(20, 
                                       nrow = n.iter + burnIn + 1, 
                                       ncol = gamesPlayed[.x], 
                                       byrow = T))
  newY <- map(seq_along(matchedNames), ~matrix(1, 
                                               nrow = n.iter + burnIn + 1, 
                                               ncol = length(sleepEsts[[.x]])))
  
  ## Player Consistency
  sigmaMat <- matrix(1, nrow = n.iter + burnIn + 1, ncol = length(matchedNames))
  
  ## Hierarchical Variance
  hSigma <- rep(1, n.iter + burnIn + 1)
  ## A Global Variable
  nameSequence <- seq_along(matchedNames)
  
  ## Loop
  
  for (j in 2:(n.iter + burnIn + 1)) {
    
    ## Get all sums of squares of previous iteration in a vector
    
    hierarchicalSS <- map_dbl(nameSequence, 
                 ~sum((thetaList[[.x]][j - 1, ] - priorMean[[.x]])^2))
    allSS <- map_dbl(nameSequence, 
                     ~sum((sleeperPoints[[.x]] - thetaList[[.x]][j - 1, ])^2))
    
    ## Hierarchical Variance
    hSigma[j] <- 1/rgamma(1, sum(gamesPlayed)/2, sum(hierarchicalSS)/2)
    
    ## Jumping into player level stuff
    
    for (i in nameSequence) {
      
      sigmaMat[j, i] <- 1/rgamma(1, gamesPlayed[i]/2 + alphas[i], 
                                 allSS[i]/2 + betas[i])
      currentPrecision <- (1/sigmaMat[j, i] + 1/hSigma[j])^(-1)
      currentMean <- (priorMean[[i]]/hSigma[j] + sleeperPoints[[i]]/sigmaMat[j, i]) * currentPrecision
      thetaList[[i]][j, ] <- mvtnorm::rmvnorm(1, currentMean, diag(currentPrecision, gamesPlayed[i]))
      
      ## Posterior Predictive Sampling
      newThetas <- mvtnorm::rmvnorm(1, sleepEsts[[i]], diag(hSigma[j], totalEsts[i]))
      newY[[i]][j, ] <- mvtnorm::rmvnorm(1, newThetas, diag(sigmaMat[j, i], totalEsts[i]))
      
    }
    # End player for
    
  }
  # End observation for
  
  ## Prepare Results
  
  resList <- map(nameSequence, ~cbind(thetaList[[.x]], 
                                                 newY[[.x]],
                                                 sigmaMat[, .x], 
                                                 hSigma))
  for (k in nameSequence) {
    
    colnames(resList[[k]]) <- c(paste0("theta_", 1:gamesPlayed[k]),
                                paste0("newY_", gameDays[[k]]), 
                                "Consistency", "Hierarchical Variance")
    
  }
  resList <- map(resList, mcmcPrep, -1:(-1 * burnIn - 1))
  names(resList) <- matchedNames
  return(resList)
  
}
multiPred(1, playerNames = c("Kyrie Irving", "DeMar DeRozan", "Pippen", "Shai", "Wemb"), this_week = 5, 
          "Saturday")[[1]]

smallRun <- multiPred(5000, playerNames = c("Kyrie Irving", "DeMar DeRozan", "Pippen", "Shai", "Wemb"), this_week = 2, 
                      "Monday", burnIn = 1e+4)
map(smallRun, summary)

smallRun2 <-  multiPred(1, playerNames = c("Kyrie Irving", "DeMar DeRozan", "Pippen", "Shai", "Wemb"), this_week = 2, 
                       "Saturday", burnIn = 1000)

weekPred(5000, "Kyrie Irving", 2, burnIn = 1000) %>% summary
