library("tidyverse")
library("coda")

# load in data
source("Data Manipulation.R")
# load in functions
source("Gibbs Sampler Functions.R")

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
firstSamples <- singlePlayerModel(3000, data = current_team %>% filter(name == team[1]),
                  priorMean = 26.2, alpha = naiveAlphasBetas$GP[8]/2, beta = naiveAlphasBetas$Betas[8]/2,
                  burnIn = 1.5e+4)
firstSamples %>% summary
firstSamples %>% effectiveSize()
firstSamples %>% plot

mean(firstSamples[, 10] >= 22) # Probability that he does as good or outperforms his previous game

## Multiple Priors
multiSamples <- singlePlayerMP(1e+4, data = current_team %>% filter(name == team[1]),
                                  alpha = naiveAlphasBetas$GP[8]/2, beta = naiveAlphasBetas$Betas[8]/2,
                                  burnIn = 5e+4)
multiSamples %>% summary
multiSamples %>% effectiveSize()

# Week Function
weekSamples <- week_pred(1e+4, data = current_team %>% filter(name == team[1]),
          week_data = current_team %>% filter(name == team[1]) %>% filter(game_no > 6),
          alpha = naiveAlphasBetas$GP[8]/2, beta = naiveAlphasBetas$Betas[8]/2,
          burnIn = 5e+4)

# initial analysis
weekSamples %>% summary
weekSamples %>% effectiveSize()
weekSamples %>% plot

# function computes probability of exceeding score in future games
weekSamples %>% prob_decision(current_team %>% filter(name == team[1]) %>% filter(game_no > 6))

# different player because I want to (Sengun)
t <- Sys.time()
week_pred(1e+4, data = current_team %>% filter(str_detect(name, "Sengun")),
          week_data = current_team %>% filter(str_detect(name, "Sengun")) %>% filter(game_no > 14),
          alpha = naiveAlphasBetas$GP[8]/2, beta = naiveAlphasBetas$Betas[8]/2,
          burnIn = 5e+4) %>%
  prob_decision(current_team %>% filter(str_detect(name, "Sengun"), game_no > 14))
Sys.time() -t

# function that computes exp
