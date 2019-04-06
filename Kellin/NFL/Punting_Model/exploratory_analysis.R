#Librarys
library('tidyverse')
library('RColorBrewer')
library('rstan')

#Read data
punt_data <- read.csv('Data/PUNT.csv')
play_data <- read.csv('Data/PLAY1.csv')

data <- select(play_data, yfog, pid) %>%
  mutate(d = 100-yfog) %>%
  right_join(punt_data, by="pid")
  
#Make a simple scatterplot of net
plot(data$d, data$pnet, pch=16, cex=0.25, main='Net yards by distance')
abline(0, 1, lty=3, col='red', lwd=2)
abline(-100, 1, lty=3, col='blue', lwd=2)

#Plot pnet mean as function of distance
data_by_distance <- aggregate(data$pnet, list(data$d), mean)
colnames(data_by_distance) <- c('d', 'net')
lines(data_by_distance$d, data_by_distance$net, col='black', lwd=3)

#Examine variance of pnet as a function of distance
var_by_distance <- aggregate(data$pnet, list(data$d), var)
plot(var_by_distance$Group.1, sqrt(var_by_distance$x), main='Variance of average net yards by distance')

#Examine kick yard return by distance
plot(data$d, data$pry, pch=16, cex=0.5, main='Return yards by distance')
kr_mean_by_distance <- aggregate(data$pry, list(data$d), mean)
kr_var_by_distance <- aggregate(data$pry, list(data$d), var)
par(mfrow=c(1,2))
plot(kr_mean_by_distance, main='Average return yards by distance')
plot(kr_var_by_distance, main='Variance of return yards by distance')
par(mfrow=c(1,1))

#'This exploratory analysis motivates the following model
#' 
#' NET(d) = mu(d) + delta(d) - epsilon(d)
#' 
#' mu is a deterministic mean function i.e.
#' for a suitable CDF (maybe pareto)
#' 
#' mu(d) = theta*F(d | phi) 
#' 
#' delta represents variability in the kick where
#' 
#' delta(d) ~ N(0, sigma(d))
#' 
#' The functional form of sigma(d) can be learned
#' from the corresponding plot
#' 
#' epsilon represents the return yards should there be one
#' 
#' epsilon(d) = R(d)chi(d)
#' 
#' where
#' 
#' R(d) is an Over-dispersed Poisson model (ignoring negative returns)
#' chi(d) ~ Bernoulli(gamma(d)) indicates a fair catch
#' 
#' theta and phi are kicker dependent and should be fit hierarchicaly
#' sigma(d) should be given a global functional form with kicker specific deviation if necesarry
#' R(d) should be global, at least until a better kicking model becomes available
#' chi(d) can be modeled with some form of logistic regression, hierarchicaly. 
#' 