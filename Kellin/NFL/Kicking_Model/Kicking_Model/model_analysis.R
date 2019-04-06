#Get parameter estimates
alpha_hat <- apply(e$alpha, 2, mean)
beta_hat <- apply(e$beta, 2, mean)
theta_hat <- apply(e$theta, 2, mean)

#Rank Stadiums by Effect
stadiums[order(theta_hat)]

#Get Predicted Fulcrum Scores
fulcrum <- rep(NA, numI)
for(i in 1:numI){
  fulcrum[i] <- (log(log(2))-alpha_hat[i])/beta_hat[i]
}

#Rank Kickers by Fulcrum
kickers[order(fulcrum)]

#Plot probability vs distance for Justin Tucker
name_list <- c('Justin Tucker', 'Adam Vinatieri', 'Roberto Aguayo', 'Wes Welker')
par(mfrow=c(2,2))
for(name in name_list){
  ind <- which(kickers == name)
  temp <- subset(data, Name==name)
  temp <- aggregate(temp$Good, list(temp$Distance), mean)
  full <- aggregate(data$Good, list(data$Distance), mean)
  plot(temp, pch=16, xlab='Distance', ylab='Probability', main=name, xlim=c(18, 80), ylim=c(0,1))
  points(full, pch=16, col=adjustcolor('black', alpha.f=0.2))
  curve(exp(-exp(mean(e$alpha0) + mean(e$beta0)*x)), add=TRUE, lwd=2, lty=2, col=adjustcolor('black', alpha.f=0.5))
  curve(exp(-exp(alpha_hat[ind] + beta_hat[ind]*x)), add=TRUE, lwd=2, col='orange')
  abline(h=0.5, lty=3, col='red')
  abline(v=fulcrum[ind], lty=3, col='red')
}


#Calculate Breakpoint and Breakpoint LB 
epa_from_kick <- function(x, a, b, linear=FALSE){
  p <- exp(-exp(a+b*x))
  if(linear){
    res <- p*(3-ep_linear(75)) - (1-p)*ep_linear(110-x)    
  }else{
    res <- p*(3-ep(75)) - (1-p)*ep(110-x)    
  }
  return(res)
}

target <- function(x, a, b, linear=FALSE){
  p <- exp(-exp(a + b*x))
  epa_from_kick(x, a, b, linear)^2
}

M <- length(e$alpha0) #MCMC Chain length
bp_linear <- bplb_linear <- rep(NA, numI)
bp <- bplb <- rep(NA, numI)
for(i in 1:numI){
  bp_temp1 <- bp_temp2 <- rep(NA, M)
  for(j in 1:M){
    opt <- optim(fulcrum[i], target, a=e$alpha[j,i], b=e$beta[j,i], linear=TRUE, method='Brent', lower=18, upper=80)
    if(opt$convergence != 0){
      warning(paste('failure to converge for ', kickers[i], ' on sample ', j))
    }
    bp_temp1[j] <- opt$par
    
    #Non-linear case
    opt <- optim(fulcrum[i], target, a=e$alpha[j,i], b=e$beta[j,i], linear=FALSE, method='Brent', lower=18, upper=80)
    if(opt$convergence != 0){
      warning(paste('failure to converge for ', kickers[i], ' on sample ', j))
    }
    bp_temp2[j] <- opt$par
  }
  bp_linear[i] <- mean(bp_temp1)
  bplb_linear[i] <- quantile(bp_temp1, .01)
  bp[i] <- mean(bp_temp2)
  bplb[i] <- quantile(bp_temp2, .01)
}

par(mfrow=c(2,2))
hist(bp_linear, main='Breakpoint (linear)')
hist(bp, main='Breakpoint (regression)')
hist(bplb_linear, main='Breakpoint LB (linear)')
hist(bplb, main='Breakpoint LB (regression)')


#Plot expected points added vs distance for different kickers
par(mfrow=c(2,2))
for(name in name_list){
  ind <- which(kickers == name)
  plot(NULL, pch=16, xlab='Distance', ylab='EPA from Kick', main=name, xlim=c(18, 80), ylim=c(-4, 3))
  curve(epa_from_kick(x, a=mean(e$alpha0), b=mean(e$beta0)), add=TRUE, lwd=2, lty=2, col=adjustcolor('black', alpha.f=0.5))
  curve(epa_from_kick(x, a=alpha_hat[ind], b=beta_hat[ind]), add=TRUE, lwd=2, col='orange')
  abline(h=0, lty=3, col='red')
  abline(v=bp[ind], lty=3, col='red')
  abline(v=bplb[ind], lty=3, col='red')
}
