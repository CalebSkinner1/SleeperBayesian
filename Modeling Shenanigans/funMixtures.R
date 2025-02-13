# libraries
library("tidyverse")

# read in data
df_2024 <- read_csv("Data Cleaning/2024_stats.R")

wembyData <- df_2024 %>% filter(name == "Victor Wembanyama")

# Fully Conditional Dist for Latent Variable
latentFC <- function(zValue, yValue, probVec, meanVec, sdVec) {
  
  ## Ensure that length of probs, mean, and sd vectors are the same
  clusterCount <- length(probVec)
  gameCount <- length(yValue)
  if (clusterCount != length(meanVec) | clusterCount != length(sdVec)) stop("\n Mean, SD, and Probability Vectors must be the same size")
  
  ## Calculate all possible Likelihood values given latent component

  ## Assuming one y-value, 
  ## This is the only step that cannot be vectorized.
  ## dnorm just won't handle it properly 
  yzMatrix <- sapply(yValue, function(y) {
    
    return(dnorm(y, mean = meanVec, sd = sdVec)) 
    # This will go over both mean and sd at the same time
    
  })
  # rownames(yzMatrix) <- paste0("z = ", 1:clusterCount)
  # colnames(yzMatrix) <- paste0("y = ", yValue)
  # return(yzMatrix)
  
  ## It is the marginal mixture dist, sum of all possible z's
  observationalLikelihood <- apply(probVec * yzMatrix, 2, sum)
  
  ## Numerator Value
  specificCluster <- probVec[zValue] * dnorm(yValue, mean = meanVec[zValue],
                                             sdVec[zValue]) # Will vectorize over y
  return(specificCluster/observationalLikelihood) # will vectorize as well
  ## This is just definition of conditional dist
  
  # There is small bump in Wemby's dist for zero. This could get the probability of being zero. 
  
}

latentFC(1, wembyData$sleeper_points, c(0.01, rep(1/3, 3)), c(0, 25, 30, 35), c(0.001, 5, 5, 5))
sapply(1:4, latentFC, wembyData$sleeper_points, c(0.01, rep(1/3, 3)), c(0, 25, 30, 35), c(0.001, 5, 5, 5))

zCounter <- function(latentVector, clusterNumber) {
  
  ## Make this more flexible
  resVector <- map_dbl(1:clusterNumber, ~max(1, sum(latentVector == .x)))
  return(resVector)
  
}

zMeans <- function(y, latentVector, clusterNumber) {
  
  resVector <- map_dbl(1:clusterNumber, ~mean(y[latentVector == .x]))
  return(resVector)
  
}

zSS <- function(y, latentVector, clusterNumber) {
  
  allMeans <- zMeans(y, latentVector, clusterNumber)
  resVector <- map_dbl(1:clusterNumber, ~sum((y[latentVector == .x] - allMeans[.x])^2))
  return(resVector)
  
}

playerMixture <- function(n.iter, data, meanStarts, sdStarts, 
                          probStarts, hyperProbs, alphas, betas, burnIn = 0) {
  
  # sdStarts is actually variance, I like that name better
  ## Some Checks
  clusterSize <- length(probStarts)
  gamesPlayed <- nrow(data) # data is a data frame
  if (clusterSize != length(meanStarts) | 
      clusterSize != length(sdStarts) | 
      clusterSize != length(hyperProbs) |
      clusterSize != length(alphas) |
      clusterSize != length(betas)) stop("\n There must be enough prior parameters for each cluster!")
  
  if (!all.equal(sum(probStarts), 1)) stop("\n Prior probabilties must sum to 1!")
  
  ## Construct Results Matrix
  muMat <- matrix(meanStarts, nrow = n.iter + burnIn + 1, ncol = clusterSize, byrow = T)
  sdVals <- matrix(sdStarts, nrow = n.iter + burnIn + 1, ncol = clusterSize, byrow = T)
  probVals <- matrix(probStarts, nrow = n.iter + burnIn + 1, ncol = clusterSize, byrow = T)
  zMat <- matrix(1, nrow = n.iter + burnIn + 1, ncol = gamesPlayed)
  
  ## Posterior Predictive Samples
  newZ <- numeric(n.iter + burnIn + 1)
  newY <- numeric(n.iter + burnIn + 1)
  
  ## Do an array for the other one, per game. 
  
  for (i in 2:(n.iter + burnIn + 1)) {
    
    ## Simulate New Z's
    
    for (j in seq_along(data$sleeper_points)) {
      
      sampleWeights <- latentFC(1:clusterSize, data$sleeper_points[j], 
                                probVals[i - 1, ], muMat[i - 1, ],
                                sdVals[i - 1, ])
      zMat[i, j] <- sample.int(clusterSize, 1, prob = sampleWeights)
      
    }
    
    ## Simulate Probabilities
    latentSampleSizes <- zCounter(zMat[i, ], clusterSize)
    probVals[i, ] <- LaplacesDemon::rdirichlet(1, hyperProbs + 
                                                 latentSampleSizes)
    
    ## Simulate Each Individual Distribution
    
    ### Mean
    latentSampleMeans <- zMeans(data$sleeper_points, zMat[i, ], 
                                clusterSize)
    latentSampleMeans[is.nan(latentSampleMeans)] <- 0
    muMat[i, ] <- LaplacesDemon::rmvn(1, mu = latentSampleMeans, 
                        Sigma = diag(sdVals[i - 1, ]/latentSampleSizes))
    
    ### Variance
    latentSS <- zSS(data$sleeper_points, zMat[i, ],
                    clusterSize)
    sdVals[i, ] <- 1/map_dbl(1:clusterSize, ~rgamma(1, latentSampleSizes[.x]/2 + alphas[.x], 
                                   latentSS[.x]/2 + betas[.x]))
    
    ## Posterior Predictive Samples
    newZ[i] <- sample.int(clusterSize, 1, prob = probVals[i, ])
    newY[i] <- rnorm(1, mean = muMat[i, newZ[i]], sd = sqrt(sdVals[i, newZ[i]]))
    
  }
  
  paramMat <- cbind(muMat, sdVals, probVals)
  colnames(paramMat) <- c(paste0("mu_", 1:clusterSize),
                        paste0("sdVal", 1:clusterSize),
                        paste0("pi_", 1:clusterSize))
  ppMat <- cbind(Y = newY, Z = newZ)
  return(list(Parameters = mcmc(paramMat[-1:(-1 * burnIn - 1), ]), 
              `Posterior Predictive Samples` = mcmc(ppMat[-1:(-1 * burnIn - 1), ]), 
              `Latent Samples` = mcmc(zMat[-1:(-1 * burnIn - 1), ])))
  
  
}

testSamples <- playerMixture(2000, wembyData, meanStarts = c(0, 20, 50),
              sdStarts = c(0.001, 10, 5), 
              probStarts = c(0.01, rep((1 - 0.01)/2, 2)), 
              hyperProbs = c(1, 1, 1), alphas = c(500, 10, 10), 
              betas = c(0.0001, 1, 1), burnIn = 3000)

summary(testSamples[[1]])
testSamples[[2]][, 1] %>% density %>% plot()
traceplot(testSamples[[1]])

ADData <- df_2024 %>% filter(name == "Anthony Davis")

adSamples <- playerMixture(2000, ADData, meanStarts = c(0, 30),
                           sdStarts = c(0.001, 10), 
                           probStarts = c(0.05, rep((1 - 0.05)/1, 1)), 
                           hyperProbs = c(1, 1), alphas = c(500, 10), 
                           betas = c(0.0001, 1), burnIn = 3000)
adSamples[[2]][, 1] %>% density %>% plot

## Too many clusters will inflate estimates of injury. 
