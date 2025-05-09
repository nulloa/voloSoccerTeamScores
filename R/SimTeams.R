# This script will create simulated team and players for multiple seasons
# 5 seasons, 10 teams, 10 players per team 
# 8 players stay in the team across seasons i.e. 2 players switch teams

library(tidyverse)
library(randomNames)

set.seed(808)

seasons <- 1:5
no_teams <- 10
teams <- paste("Team", LETTERS[1:no_teams])
constant_players <- randomNames(no_teams*8) # #teams * #constant players
new_players <- randomNames(no_teams*2)

# Create base of complete dataset
teamsdf <- data.frame(
  season = rep(seasons, each=length(teams)),
  team   = rep(teams, length(seasons))
)

# Create constant player dataset
constantdf <- data.frame(
  team = rep(teams, each=8),
  player = constant_players
)

# Sim new players on teams for each season
newdf <- data.frame(
  season = NULL,
  team = NULL,
  player = NULL
)

for(i in seasons){
  tmp <- data.frame(
    season = i,
    team = rep(teams, each=2),
    player = new_players[sample(1:20, 20)]
  )
  
  newdf <- rbind(newdf, tmp)
}

# Merge them all together
team_data <- teamsdf %>%
  left_join(constantdf) %>%
  bind_rows(., newdf)


# Also we will assign lambda (exp goals) for each team. This will essentially 
# be our belief in the teams ability. Basically, we sim the lambdas from a 
# gamma dist kinda like a prior.
lambdas <- c(rgamma(5, shape=1, scale=2), rgamma(5, shape=7.5, scale=1))[sample(1:10, 10)]

team_data <- team_data %>%
  left_join(data.frame(team = teams, lambda=lambdas))


#save to rda file
save(team_data, file = "~/Github/voloSoccerTeamScores/data/simteamdata.rda")
