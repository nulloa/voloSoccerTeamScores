# Sim Results of teams playing each other

library(tidyverse)

set.seed(808)
load("~/Github/voloSoccerTeamScores/data/simteamdata.rda")


team_data %>% 
  select(team, lambda) %>% 
  unique() %>%
  mutate(lambda = round(lambda, 1)) %>%
  arrange(-lambda)

teams <- unique(team_data$team)

games <- expand.grid(HomeTeam = teams, AwayTeam = teams, stringsAsFactors = FALSE) %>%
  filter(HomeTeam != AwayTeam) %>%
  mutate(Match = paste(HomeTeam, AwayTeam, sep = " - "))

game_res <- NULL
for(i in unique(team_data$season)[1:4]){
  tmp <- games %>%
    group_by(Match) %>%
    mutate(htLambda = unique(team_data$lambda[team_data$team==HomeTeam]),
           htRes = rpois(1, lambda=unique(team_data$lambda[team_data$team==HomeTeam])),
           atLambda = unique(team_data$lambda[team_data$team==AwayTeam]),
           atRes = rpois(1, lambda=unique(team_data$lambda[team_data$team==AwayTeam]))) %>%
    ungroup() %>%
    mutate(Res = htRes - atRes) %>%
    mutate(season = i)
  
  game_res <- rbind(game_res, tmp)
}

#save to rda file
save(game_res, file = "~/Github/voloSoccerTeamScores/data/simgameresults.rda")
