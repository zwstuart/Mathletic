baselineWinRankings <- function(games, year, week, momentum = 1,theta = .85){
  games.year <- games %>% 
    filter(Season %in% year) %>% 
    filter(Week <= week)
  teams.year <- games %>% 
    filter(Season %in% year) %>% 
    select(Home) %>% 
    unique() %>% 
    pull()
  teams.year <- teams.year[order(teams.year) ] 
  
  non.ties <- games.year[games.year$HomeWin != 0.5, ]
  ties <- games.year[games.year$HomeWin == 0.5, ]
  win.matrix <- matrix(0, nrow = length(teams.year), ncol = length(teams.year))
  for(i in 1:nrow(non.ties)){
    factor <- pull(momentum^(week - non.ties[i,"Week"]))
    if(non.ties[i, "HomeWin"] == 0){
      win.matrix[which(teams.year == pull(non.ties[i, "Home"])), which(teams.year == pull(non.ties[i, "Visiting"]))] <- factor
    }else{
      win.matrix[which(teams.year == pull(non.ties[i, "Visiting"])), which(teams.year == pull(non.ties[i, "Home"]))] <- factor
    }
  }
  if(nrow(ties) > 0){
    for(i in 1:nrow(ties)){
      factor <- pull(momentum^(week - ties[i,"Week"]))
      win.matrix[which(teams.year == pull(ties[i, "Home"])), which(teams.year == pull(ties[i, "Visiting"]))] <- .5 * factor
      win.matrix[which(teams.year == pull(ties[i, "Visiting"])), which(teams.year == pull(ties[i, "Home"]))] <- .5 * factor
    }
  }
  win.matrix.norm <- win.matrix / rowSums(win.matrix)
  transition.diag <- rep(0,nrow(win.matrix.norm))
  transition.diag[unique(which(is.na(win.matrix.norm), arr.ind=TRUE)[ ,1])] <- 1
  win.matrix.norm[is.na(win.matrix.norm)]<- 0
  win.matrix.norm <- win.matrix.norm + diag(transition.diag, nrow = length(teams.year), ncol = length(teams.year))
  
  p.matrix <- theta * win.matrix.norm + (1 - theta) * matrix(1/length(teams.year), nrow = length(teams.year), ncol = length(teams.year))
  
  p.rankings <- (p.matrix %^% 10000)[1, ]
  team.rankings <- teams.year[order(p.rankings, decreasing = TRUE)]
  p.rankings <- sort(p.rankings, decreasing = TRUE)
  
  return(list(team.rankings, p.rankings))
}

##########################################################################################################################################

spreadWinRankings <- function(games, year, week, momentum = 1, theta = .85){
  games.year <- games %>% 
    filter(Season %in% year) %>% 
    filter(Week <= week)
  teams.year <- games %>% 
    filter(Season %in% year) %>% 
    select(Home) %>% 
    unique() %>% 
    pull()
  teams.year <- teams.year[order(teams.year) ] 
  
  non.ties <- games.year[games.year$HomeWin != 0.5, ]
  ties <- games.year[games.year$HomeWin == 0.5, ]
  win.matrix <- matrix(0, nrow = length(teams.year), ncol = length(teams.year))
  for(i in 1:nrow(non.ties)){
    factor <- pull(momentum^(week - non.ties[i,"Week"]))
    if(non.ties[i, "HomeWin"] == 0){
      win.matrix[which(teams.year == pull(non.ties[i, "Home"])), which(teams.year == pull(non.ties[i, "Visiting"]))] <- factor * (non.ties$VisitingPoints[i] - non.ties$HomePoints[i])
    }else{
      win.matrix[which(teams.year == pull(non.ties[i, "Visiting"])), which(teams.year == pull(non.ties[i, "Home"]))] <- factor * (non.ties$HomePoints[i] - non.ties$VisitingPoints[i])
    }
  }
  win.matrix.norm <- win.matrix / rowSums(win.matrix)
  transition.diag <- rep(0,nrow(win.matrix.norm))
  transition.diag[unique(which(is.na(win.matrix.norm), arr.ind=TRUE)[ ,1])] <- 1
  win.matrix.norm[is.na(win.matrix.norm)]<- 0
  win.matrix.norm <- win.matrix.norm + diag(transition.diag, nrow = length(teams.year), ncol = length(teams.year))
  
  p.matrix <- theta * win.matrix.norm + (1 - theta) * matrix(1/length(teams.year), nrow = length(teams.year), ncol = length(teams.year))
  
  p.rankings <- (p.matrix %^% 10000)[1, ]
  team.rankings <- teams.year[order(p.rankings, decreasing = TRUE)]
  p.rankings <- sort(p.rankings, decreasing = TRUE)
  
  return(list(team.rankings, p.rankings))
}

#############################################################################################################

baselineLossRankings <- function(games, year, week, momentum = 1, theta = .85){
  games.year <- games %>% 
    filter(Season %in% year) %>% 
    filter(Week <= week)
  teams.year <- games %>% 
    filter(Season %in% year) %>% 
    select(Home) %>% 
    unique() %>% 
    pull()
  teams.year <- teams.year[order(teams.year) ] 
  
  non.ties <- games.year[games.year$HomeWin != 0.5, ]
  ties <- games.year[games.year$HomeWin == 0.5, ]
  win.matrix <- matrix(0, nrow = length(teams.year), ncol = length(teams.year))
  for(i in 1:nrow(non.ties)){
    factor <- pull(momentum^(week - non.ties[i,"Week"]))
    if(non.ties[i, "HomeWin"] == 0){
      win.matrix[which(teams.year == pull(non.ties[i, "Visiting"])), which(teams.year == pull(non.ties[i, "Home"]))] <- factor
    }else{
      win.matrix[which(teams.year == pull(non.ties[i, "Home"])), which(teams.year == pull(non.ties[i, "Visiting"]))] <- factor
    }
  }
  if(nrow(ties) > 0){
    for(i in 1:nrow(ties)){
      factor <- pull(momentum^(week - ties[i,"Week"]))
      win.matrix[which(teams.year == pull(ties[i, "Visiting"])), which(teams.year == pull(ties[i, "Home"]))] <- .5 * factor
      win.matrix[which(teams.year == pull(ties[i, "Home"])), which(teams.year == pull(ties[i, "Visiting"]))] <- .5 * factor
    }
  }
  win.matrix.norm <- win.matrix / rowSums(win.matrix)
  transition.diag <- rep(0,nrow(win.matrix.norm))
  transition.diag[unique(which(is.na(win.matrix.norm), arr.ind=TRUE)[ ,1])] <- 1
  win.matrix.norm[is.na(win.matrix.norm)]<- 0
  win.matrix.norm <- win.matrix.norm + diag(transition.diag, nrow = length(teams.year), ncol = length(teams.year))
  
  p.matrix <- theta * win.matrix.norm + (1 - theta) * matrix(1/length(teams.year), nrow = length(teams.year), ncol = length(teams.year))
  
  p.rankings <- (p.matrix %^% 10000)[1, ]
  team.rankings <- teams.year[order(p.rankings, decreasing = FALSE)]
  p.rankings <- sort(p.rankings, decreasing = TRUE)
  
  return(list(team.rankings, p.rankings))
}

#############################################################################################################

spreadLossRankings <- function(games, year, week, momentum = 1, theta = .85){
  games.year <- games %>% 
    filter(Season %in% year) %>% 
    filter(Week <= week)
  teams.year <- games %>% 
    filter(Season %in% year) %>% 
    select(Home) %>% 
    unique() %>% 
    pull()
  teams.year <- teams.year[order(teams.year) ] 
  
  non.ties <- games.year[games.year$HomeWin != 0.5, ]
  ties <- games.year[games.year$HomeWin == 0.5, ]
  win.matrix <- matrix(0, nrow = length(teams.year), ncol = length(teams.year))
  for(i in 1:nrow(non.ties)){
    factor <- pull(momentum^(week - non.ties[i,"Week"]))
    if(non.ties[i, "HomeWin"] == 0){
      win.matrix[which(teams.year == pull(non.ties[i, "Visiting"])), which(teams.year == pull(non.ties[i, "Home"]))] <- factor * (non.ties$VisitingPoints[i] - non.ties$HomePoints[i])
    }else{
      win.matrix[which(teams.year == pull(non.ties[i, "Home"])), which(teams.year == pull(non.ties[i, "Visiting"]))] <- factor * (non.ties$HomePoints[i] - non.ties$VisitingPoints[i])
    }
  }
  win.matrix.norm <- win.matrix / rowSums(win.matrix)
  transition.diag <- rep(0,nrow(win.matrix.norm))
  transition.diag[unique(which(is.na(win.matrix.norm), arr.ind=TRUE)[ ,1])] <- 1
  win.matrix.norm[is.na(win.matrix.norm)]<- 0
  win.matrix.norm <- win.matrix.norm + diag(transition.diag, nrow = length(teams.year), ncol = length(teams.year))
  
  p.matrix <- theta * win.matrix.norm + (1 - theta) * matrix(1/length(teams.year), nrow = length(teams.year), ncol = length(teams.year))
  
  p.rankings <- (p.matrix %^% 10000)[1, ]
  team.rankings <- teams.year[order(p.rankings, decreasing = FALSE)]
  p.rankings <- sort(p.rankings, decreasing = TRUE)
  
  return(list(team.rankings, p.rankings))
}

########################################################################################################

weightRankings <- function(off.rank, def.rank, alpha = 0.5){
  ranks <- rep(0, length(off.rank))
  for(i in 1:length(off.rank)){
    ranks[i] <- i * alpha + (1 - alpha) * which(def.rank == off.rank[i])
  }
  team.ranks <- off.rank[order(ranks, decreasing = FALSE)]
  return(team.ranks)
}

########################################################################################################

strengthReport <- function(games, year, week, f1, f2, alpha = .5, momentum = 1, theta = .85){
  games.year <- games %>% 
    filter(Season %in% year) %>% 
    filter(Week <= week)
  teams.year <- games %>% 
    filter(Season %in% year) %>% 
    select(Home) %>% 
    unique() %>% 
    pull()
  
  power.rankings <- weightRankings(f1(games, year, week, momentum, theta)[[1]], f2(games, year, week, momentum, theta)[[1]], alpha)
  power.rankings <- weightRankings(spreadWinRankings(games, year, week, momentum, theta)[[1]], spreadLossRankings(games, year, week, momentum, theta)[[1]], alpha)
  power.ratings <- round(alpha * spreadWinRankings(games, year, week, momentum, theta)[[2]] + (1 - alpha) * spreadLossRankings(games, year, week, momentum, theta)[[2]], 3)
  
  
  team.ranking <- rep(0,length(teams.year))
  team.wins <- rep(0,length(teams.year))
  team.losses <- rep(0,length(teams.year))
  team.ties <- rep(0,length(teams.year))
  
  
  for(i in 1:length(teams.year)){
    home.games <- games.year %>% 
      filter(Home == teams.year[i])
    away.games <- games.year %>% 
      filter(Visiting == teams.year[i])
    home.ties <- length(which(home.games$HomeWin == .5))
    away.ties <- length(which(away.games$HomeWin == .5))
    team.ties[i] <- home.ties + away.ties
    home.wins <- length(which(home.games$HomeWin == 1))
    away.wins <- length(which(away.games$HomeWin == 0))
    team.wins[i] <- home.wins + away.wins
    home.losses <- length(which(home.games$HomeWin == 0))
    away.losses <- length(which(away.games$HomeWin == 1))
    team.losses[i] <- home.losses + away.losses
    
    team.ranking[i] <- which(power.rankings == teams.year[i])
  }
  
  standings.frame <- cbind.data.frame(team.ranking, teams.year, team.wins, team.losses, team.ties)
  standings.frame <- standings.frame[order(standings.frame$team.ranking), ]
  standings.frame <- standings.frame[, -1]
  standings.frame$Strength <- power.ratings
  names(standings.frame) <- c("Team", "Wins", "Losses", "Ties", "Strength")
  rownames(standings.frame) <- 1:nrow(standings.frame)
  
  return(standings.frame)
}

