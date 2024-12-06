library(tidyverse)
library(coda)
library(rjags)

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

jagsPred <- function(n.iter, playerNames, this_week, dotw = c("Monday", "Tuesday", "Wednesday", 
                                                               "Thursday", "Friday", "Saturday", "Sunday"),
                     priorScale = 25, consistencyParams = multiPuller(playerNames),
                      burnIn = 0) {
  
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
  
  ## A Global Variable
  nameCount <- length(matchedNames)
  
  ## Make sleeperPoints and sleeperProjections are matrices
  mostGamesPlayed <- max(gamesPlayed)
  pointsMat <- do.call(rbind, sleeperPoints)
  priorMat <- do.call(rbind, priorMean)
  
  ## JAGS
  
  theCauchyModel <- jags.model(file = "pexModel.txt", 
                               data = list(y = pointsMat, 
                                           playerCount = nameCount,
                                           gamesPlayed = gamesPlayed,
                                           sleeperProjections = priorMat, 
                                           alphas = alphas, 
                                           betas = betas,
                                           priorScale = priorScale))
  update(theCauchyModel, burnIn)
  finalSamples <- coda.samples(theCauchyModel, 
                               c("theta", "Consistency", "hierarchicalVariance"), 
                               n.iter)[[1]]
  
  ## Prettying it Up
  thetaInds <- map(1:nameCount, ~str_which(colnames(finalSamples), 
                                          paste0("theta\\[", .x)))
  thetaList <- map(thetaInds, ~finalSamples[, .x])
  sigmaMat <- finalSamples[, 1:nameCount]
  hSigma <- finalSamples[, nameCount + 1]
  
  ## Posterior Predictive Samples
  newY <- map(seq_along(matchedNames), 
                       ~matrix(1, nrow = n.iter, 
                                  ncol = length(sleepEsts[[.x]])))
  for (j in 1:n.iter) {
    
    for (i in 1:nameCount) {
      
      newThetas <- mvtnorm::rmvnorm(1, sleepEsts[[i]], diag(hSigma[j], totalEsts[i])) # Sample means
      newY[[i]][j, ] <- mvtnorm::rmvnorm(1, newThetas, diag(sigmaMat[j, i], totalEsts[i])) # Sample New Observations
      
    }
    
  }
  
  ## Prepare Results
  
  resList <- map(1:nameCount, ~cbind(thetaList[[.x]], 
                                      newY[[.x]],
                                      sigmaMat[, .x], 
                                      hSigma))
  for (k in 1:nameCount) {
    
    colnames(resList[[k]]) <- c(paste0("theta_", 1:gamesPlayed[k]),
                                paste0("newY_", gameDays[[k]]), 
                                "Consistency", "Hierarchical Variance")
    
  }
  resList <- map(resList, mcmc)
  names(resList) <- matchedNames
  return(resList)
  
}

jagsRun <- jagsPred(1e+4, playerNames = c("Kyrie Irving", "DeMar DeRozan", "Pippen", "Shai", "Wemb"), this_week = 5, 
                      "Monday", burnIn = 5e+3)
map(jagsRun, summary)
map(jagsRun, traceplot)
