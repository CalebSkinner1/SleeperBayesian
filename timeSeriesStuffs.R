## Looking at scores over time

df_2024 %>% filter(name == "Scoot Henderson") %>% pull(sleeper_points) %>% 
  plot(type = "l") # Interesting one here. This one looks to not be stationary


df_2024 %>% filter(name == "Scottie Barnes") %>% pull(sleeper_points) %>% 
  plot(type = "l") # This one too. Some players are definitely not stationary

## Scottie Barnes Change Point? 
scottieBarnes_points <- df_2024 %>% filter(name == "Scottie Barnes") %>% pull(sleeper_points) # 61 obs

## This is a function I made where it will apply a function over some window
nSplit <- function(.x, n, .f, ...) {
  
  lenX <- length(.x)
  startPoints <- seq(1, lenX, by = n) # Loses certain points near the end :(
  endPoints <- c(startPoints[-1] - 1, lenX) # It doesn't?
  .f <- purrr:::as_mapper(.f, ...) # Let's me do the ~ from map
  
  ## I like type safety. So when I do a for loop like this
  ## I like specifying a vector with the exact length and class 
  ## as the thing that I am expecting out. This line does just that
  
  segementCount <- length(startPoints) # How long is the thing we want
  classX <- match.fun(class(.x)) # Find it's class and get its constructor
  finalVec <- classX(segementCount) # Specify final vector with class
  for (i in 1:segementCount) {
    
    curSeq <- (startPoints[i]):(endPoints[i]) # Generate Seqeunce
    finalVec[i] <- .f(.x[curSeq], ...) # Perform function on said sequence
    
  }
  # End for
  
  return(finalVec) # Final Return
  
}

nSplit(scottieBarnes_points, 3, mean) # Mean over groups of three
nSplit(scottieBarnes_points, 5, mean) # Mean over groups of five
nSplit(scottieBarnes_points, 10, mean) # Mean over groups of ten

# It appears to me that he improved by the end of the season
# Interestingly enough, younger players might not have stationary dsitributions. 
