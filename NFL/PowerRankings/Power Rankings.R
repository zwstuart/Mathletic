library(tidyverse)
library(expm)

games <- read_csv("C:\\Users\\Zach\\Desktop\\Football\\Power Rankings\\Score Data.csv")
games$HomeWin <- ifelse(games$HomePoints > games$VisitingPoints, 1, ifelse(games$HomePoints < games$VisitingPoints, 0, .5))

########### Power Rankings ###########

year <- 2017
week <- 17

baselineWinRankings(games, year, week)
baselineLossRankings(games, year, week)
spreadWinRankings(games, year,week)
spreadLossRankings(games, year, week)

weightRankings(spreadWinRankings(games, year,week)[[1]], spreadLossRankings(games, year,week)[[1]])

team.standings <- strengthReport(games, year, week, spreadWinRankings, spreadLossRankings, momentum = 1 ,alpha = .5)

########### Final Rankings ###########

seasons <- unique(games$Season)
final.standings <- c()
for(i in seasons){
  max.week <- games %>% 
    filter(Season == i) %>% 
    select(Week) %>% 
    pull() %>% 
    max()
  new.season <- strengthReport(games, i, max.week, spreadWinRankings, spreadLossRankings, momentum = 1 ,alpha = .5)
  new.season$Season <- rep(i,nrow(new.season))
  new.season <- new.season[, c("Season", "Team", "Wins", "Losses", "Ties", "Strength")]
  final.standings <- rbind.data.frame(final.standings, new.season)
}

