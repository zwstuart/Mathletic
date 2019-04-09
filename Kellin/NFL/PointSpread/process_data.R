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
jplot <- function(x, y, sigma=c(.01,.01), ...){
  plot(x+rnorm(x, 0, sigma[1]), y+rnorm(y, 0, sigma[2]), ...)
}


par(mfrow=c(3,2))
phist(data$total_td, main='Total TD')
phist(data$total_fg, main='Total FG')
phist(data$home_td, main='Home TD')
phist(data$home_fg, main='Home FG')
phist(data$away_td, main='Away TD')
phist(data$away_fg, main='Away FG')

par(mfrow=c(2,2))
jplot(data$home_td, data$home_fg, sigma=c(.1, .1), pch=16, col=adjustcolor('dodgerblue', alpha.f=0.4), xlab='Home TD', ylab='Home FG', main=paste('Home TD vs FG: ', round(cor(data$home_td, data$home_fg),3)))
jplot(data$away_td, data$away_fg, sigma=c(.1, .1), pch=16, col=adjustcolor('dodgerblue', alpha.f=0.4), xlab='Away TD', ylab='Away FG', main=paste('Away TD vs FG: ', round(cor(data$away_td, data$away_fg),2)))
jplot(data$home_td, data$away_td, sigma=c(.1, .1), pch=16, col=adjustcolor('dodgerblue', alpha.f=0.4), xlab='Home TD', ylab='Away TD', main=paste('TD Home vs Away', round(cor(data$home_td, data$away_td),2)))
jplot(data$home_fg, data$away_fg, sigma=c(.1, .1), pch=16, col=adjustcolor('dodgerblue', alpha.f=0.4), xlab='Home FG', ylab='Away FG', main=paste('FG Home vs Away', round(cor(data$home_fg, data$away_fg), 2)))

#Bootstrap for correlations (Damn all significant)
x1 <- data$home_td
x2 <- data$away_td
x3 <- data$home_fg
x4 <- data$away_fg

B <- 10000
boot1 <- boot2 <- boot3 <- boot4 <- rep(NA, B)
for(b in 1:B){
  ind <- sample(length(x1), length(x1), replace=TRUE)
  boot1[b] <- cor(x1[ind], x3[ind])
  boot2[b] <- cor(x2[ind], x4[ind])
  boot3[b] <- cor(x1[ind], x2[ind])
  boot4[b] <- cor(x3[ind], x4[ind])
}
hist(boot1, col=adjustcolor('dodgerblue', alpha.f=0.4), xlab='Home TD', ylab='Home FG', main=paste('Home TD vs FG: ', round(cor(data$home_td, data$home_fg),3)))
hist(boot2, col=adjustcolor('dodgerblue', alpha.f=0.4), main=paste('Away TD vs FG: ', round(cor(data$away_td, data$away_fg),2)))
hist(boot3, col=adjustcolor('dodgerblue', alpha.f=0.4), main=paste('TD Home vs Away', round(cor(data$home_td, data$away_td),2)))
hist(boot4, col=adjustcolor('dodgerblue', alpha.f=0.4), main=paste('FG Home vs Away', round(cor(data$home_fg, data$away_fg), 2)))

#Bivariate poisson distributions
rbpois_pos <- function(n, pars){
  z0 <- rpois(n, pars[1])
  z1 <- rpois(n, pars[2])
  z2 <- rpois(n, pars[3])
  return(cbind(z0+z1, z0+z2))
}

rbpois_neg <- function(n, pars){
  z0 <- rpois(n, pars[1])
  z1 <- rpois(n, pars[2])
  z2 <- rpois(n, pars[3])
  x1 <- rbinom(1, z0, pars[4]) 
  return(cbind(x1+z1, z0-x1+z2))
}
