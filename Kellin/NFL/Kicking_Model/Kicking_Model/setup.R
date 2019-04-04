library(tidyverse)
library(ggplot2)
library(dplyr)
library(RColorBrewer)
library(rstan)

#########################
#GET EXPECTED POINTS DATA
#########################
pbp_data <- rbind(readr::read_csv("data/season_play_by_play/pbp_2009.csv"),
                  readr::read_csv("data/season_play_by_play/pbp_2010.csv"),
                  readr::read_csv("data/season_play_by_play/pbp_2011.csv"),
                  readr::read_csv("data/season_play_by_play/pbp_2012.csv"),
                  readr::read_csv("data/season_play_by_play/pbp_2013.csv"),
                  readr::read_csv("data/season_play_by_play/pbp_2014.csv"),
                  readr::read_csv("data/season_play_by_play/pbp_2015.csv"),
                  readr::read_csv("data/season_play_by_play/pbp_2016.csv"))

pbp_chart_data <- pbp_data %>% filter(!is.na(ExpPts))  %>% rename(EP = ExpPts, YardsAway = yrdline100) %>%
  mutate(Model = "nflscrapR")

bob = brewer.pal(4, 'Set1')
plot(NULL, xlim=c(0,100), ylim=c(-3, 7))
phi <- list()
for(i in 1:4){
  ind <- which(pbp_chart_data$down == i)
  x <- pbp_chart_data$YardsAway[ind]
  ep <- pbp_chart_data$EP[ind]
  
  temp <- data.frame(x=x, ep=ep)
  phi[[i]] <- aggregate(ep~x, data=temp, mean)
  
  lines(phi[[i]], col=bob[i], lwd=2)
  curve(6 - 8/100*x, add=TRUE)
}

##############################################
#READ FG KICK DATA AND DO PRELIMINARY ANALYSIS
##############################################
data <- read_csv('data/fgKicks.csv')
data$Name[which(data$Name == 'Stephen Hauschka')] <- 'Steven Hauschka'

#Get data into a usable form
data_by_yard   <- aggregate(data$Good, list(data$Distance), FUN=sum)
data_by_yard$n <- apply(table(data$Good, data$Distance), 2, sum)
data_by_yard$prob <- data_by_yard$x/data_by_yard$n
names(data_by_yard) <- c('dist', 'make', 'n', 'prob')

#Some link functions to consider
logistic <- function(x, theta){
  1/(1+exp(-theta[1]-theta[2]*x))
}
cloglog <- function(x, theta){
  exp(-exp(theta[1] + theta[2]*x))
}
cloglog3 <- function(x, theta){
  exp(-exp(theta[1] + theta[2]*x + theta[3]*x^2))
}
# probistic <- function(x, theta){
#   pnorm(-theta[1]-theta[2]*x)
# }
# weibull <- function(x, theta){
#   1 - pweibull(x, exp(theta[1]), exp(theta[2]))
# }
# gamma <- function(x, theta){
#   1 - pgamma(x, exp(theta[1]), exp(theta[2]))
# }

#Negative Log-Likelihood function
target <- function(theta, data, link){
  p <- link(data$dist, theta)
  lik <- -sum(data$make*log(p) + (data$n-data$make)*log(1-p))
}

#Compare logistic and cloglog link functions
#(other link functions didn't perform very well)
plot(data_by_yard$dist, data_by_yard$prob, xlab='distance', ylab='probability')

fit1 <- optim(c(0,0), fn=target, data=data_by_yard, link=logistic)
curve(logistic(x, fit1$par), add=TRUE, col='red', lwd=2)

fit2 <- optim(c(-5, .08, 0), fn=target, data=data_by_yard, link=cloglog3, method='SANN', control=list(maxit=100000))
curve(cloglog3(x, fit2$par), add=TRUE, col='blue', lwd=2)

fit3 <- optim(c(0,0), fn=target, data=data_by_yard, link=cloglog, method='SANN')
curve(cloglog(x, fit3$par), add=TRUE, col='orange', lwd=2)
legend('bottomleft', c('Logistic', 'Cloglog', 'Cloglog order-2'), lwd=2, col=c('red', 'orange', 'blue'))


#Collect data for STAN model
N <- dim(data)[1]
y <- data$Good
x <- data$Distance 
stadiums <- unique(data$Stadium)
numJ <- length(stadiums)
J <- rep(NA, N)
for(j in 1:N){
  J[j] <- which(data$Stadium[j] == stadiums)
}

kickers <- unique(data$Name)
numI <- length(kickers)
I <- rep(NA, N)
for(j in 1:N){
  I[j] <- which(data$Name[j] == kickers)
}

