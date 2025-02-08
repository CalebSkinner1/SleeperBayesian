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

playerMixture <- function(n.iter, data, meanStarts, sdStarts, 
                          probStarts, hyperProbs, burnIn = 0) {
  
  # sdStarts is actually variance, I like that name better
  ## Some Checks
  clusterSize <- length(probStarts)
  gamesPlayed <- nrow(data) # data is a data frame
  if (clusterSize != length(meanStarts) | 
      clusterSize != length(sdStarts) | 
      clusterSize != length(hyperProbs) ) stop("\n There must be enough prior parameters for each cluster!")
  
  if (sum(probStarts) != 1) stop("\n Prior probabilties must sum to 1!")
  
  ## Construct Results Matrix
  muMat <- matrix(meanStarts, nrow = n.iter + burnIn + 1, ncol = clusterSize, byrow = T)
  sdVals <- matrix(sdStarts, nrow = n.iter + burnIn + 1, ncol = clusterSize, byrow = T)
  probVals <- matrix(probStarts, nrow = n.iter + burnIn + 1, ncol = clusterSize, byrow = T)
  zMat <- matrix(1, nrow = n.iter + burnIn + 1, ncol = clusterSize)
  
  ## Do an array for the other one, per game. 
  
  for (i in 2:(n.iter + burnIn + 1)) {
    
    ## Simulate New Z's
    
    for (j in seq_along(data$sleeper_points)) {
      
      sampleWeights <- latentFC(1:4, data$sleeper_point[j], 
                                probVals[i - 1, ], muMat[i - 1, ],
                                sdVals[i - 1, ])
      zMat[i, j] <- sample.int(clusterSize, 1, prob = sampleWeights)
      
    }
    
    ## Simulate Probabilities
    latentSampleSizes <- as.numeric(table(zMat[i, ]))
    probVals[i, ] <- LaplacesDemon::rdirichlet(1, hyperProbs + 
                                                 latentSampleSizes)
    
    ## Simulate Each Individual Distribution
    
    ### Mean
    latentSampleMeans <- as.numeric(tapply(data$sleeper_point,
                                zMat[i, ], mean))
    muMat[i, ] <- LaplacesDemon::rmvn(1, mu = latentSampleMeans, 
                        Sigma = diag(sdStarts[i - 1, ]/latentSampleSizes))
    
    ### Variance
    sdVals[i, ]
    
    
  }
  
}

playerMixture(10, wembyData, c(0, 25, 30, 35))