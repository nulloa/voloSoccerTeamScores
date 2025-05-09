# So after simulating team data and game results, I've basically realized 
# that is all is a matching exercise. Then it's not that difficult estimate
# team ability on goals scored.

match_teams <- function(df_new, df_old){
  new_teams <- unique(df_new$team)
  old_teams <- unique(df_old$team)
  bestmatch <- NULL
  
  for(nt in new_teams){
    new_players <- subset(df_new, team==nt)$player
    matches <- NULL
    
    for(ot in old_teams){
      old_players <- subset(df_old, team==ot)$player
      tmp <- data.frame(oldTeam = ot, permatch = mean(new_players %in% old_players))
      matches <- rbind(matches, tmp)
    }
    
    tmp <- data.frame(newTeam = nt, oldTeam = matches[which.max(matches$permatch),"oldTeam"])
    bestmatch <- rbind(bestmatch, tmp)
    
  }
  
  return(bestmatch)
  
}
