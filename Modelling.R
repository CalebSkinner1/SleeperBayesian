library(tidyverse)
library(coda)

#### Variance Priors

## Naive Alphas
#### Not Divided by 2 yet :(
naiveAlphasBetas <- df_2024 %>% group_by(name) %>% summarise(GP = length(name), 
                                                             Betas = (GP - 1) * var(sleeper_points)) %>%
  ungroup()
team <- c("Kyrie Irving", "Jalen Suggs", "Miles Bridges", "DeMar DeRozan", "Alperen Sengun",
          "Draymond Green", "Deandre Ayton", "Jordan Poole", "Collin Sexton")

teamParams <- naiveAlphasBetas %>% filter(name %in% team)
