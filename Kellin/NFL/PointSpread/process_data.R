#Include library's
library(tidyverse)
library(RColorBrewer)
library(rstan)

#Get data set
play_data <- rbind(read_csv('data/PLAY1.csv'), read_csv('data/PLAY2.csv'))
game_data <- read_csv('data/GAME.csv')

#Get home and away scores
ptsh <- ptsv <- rep(NA, dim(play_data)[1])
for(i in 1:dim(play_data)[1]){
  g <- play_data$gid[i]
  home_team <- game_data$h[g]
  away_team <- game_data$v[g]
  if(play_data$off[i] == home_team){
    ptsh[i] <- play_data$ptso[i]
    ptsv[i] <- play_data$ptsd[i]
  }
  if(play_data$off[i] == away_team){
    ptsh[i] <- play_data$ptsd[i]
    ptsv[i] <- play_data$ptso[i]
  }
  #Check if last play of the game is a feild goal (dumb)
  if(i < dim(play_data)[1]){
    if(play_data$gid[i+1] != g){
      ptsh[i] <- as.numeric(game_data[g,'ptsh'])
      ptsv[i] <- as.numeric(game_data[g,'ptsv'])
    }
  }else{
    ptsh[i] <- as.numeric(game_data[g,'ptsh'])
    ptsv[i] <- as.numeric(game_data[g,'ptsv'])
  }
}
play_data$ptsh <- ptsh
play_data$ptsv <- ptsv

#nailed it
temp <- aggregate(play_data$ptsh, list(play_data$gid), function(z) z[length(z)])$x
plot(temp, game_data$ptsh, col=c(rep(1,1920), 2, rep(1, 1000)))

#Remove ghost scores from data (not sure whats happening here)
i <- 1
while(i < (dim(play_data)[1])){
  if(ptsh[i] > ptsh[i+1] & play_data$gid[i] == play_data$gid[i+1]){
    ptsh[i] <- ptsh[i+1]
    i <- i - 1
    print('Ghostbusted!')
  }else{
    i <- i + 1
  }
}
while(i < (dim(play_data)[1])){
  if(ptsv[i] > ptsv[i+1] & play_data$gid[i] == play_data$gid[i+1]){
    ptsv[i] <- ptsv[i+1]
    i <- i - 1
    print('Ghostbusted!')
  }else{
    i <- i + 1
  }
}
play_data$ptsh <- ptsh
play_data$ptsv <- ptsv

#nailed it for real
temp <- aggregate(play_data$ptsh, list(play_data$gid), max)$x
plot(temp, game_data$ptsh)

#Get data in the format that I want
home_events <- aggregate(play_data$ptsh, list(play_data$gid), diff)$x
away_events <- aggregate(play_data$ptsv, list(play_data$gid), diff)$x


data <- tibble(gid=game_data$gid, home_team=game_data$h, away_team=game_data$v, seas=game_data$seas,
               home_td=unlist(lapply(home_events, function(z) sum(z>3))), 
               away_td=unlist(lapply(away_events, function(z) sum(z>3))),
               home_fg=unlist(lapply(home_events, function(z) sum(z==3))),
               away_fg=unlist(lapply(away_events, function(z) sum(z==3))))
data <- mutate(data, total_td=home_td+away_td, total_fg=home_fg+away_fg,
             diff_td=home_td-away_td, diff_fg=home_fg-away_fg,
             s=seas-min(seas)+1)

#Make some plots
phist <- function(z, ...){
  hist(z, freq=F, breaks=-1:max(z),...)
  points(0:max(z), dpois(0:max(z), mean(z)), col='firebrick', pch=16)
}

par(mfrow=c(3,2))
phist(data$total_td, main='Total TD')
phist(data$total_fg, main='Total FG')
phist(data$home_td, main='Home TD')
phist(data$home_fg, main='Home FG')
phist(data$away_td, main='Away TD')
phist(data$away_fg, main='Away FG')


