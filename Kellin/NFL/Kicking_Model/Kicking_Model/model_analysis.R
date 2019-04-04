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
ind <- 103
temp <- subset(data, Name='Justin Tucker')
temp <- aggregate(temp$Good, list(temp$Distance), mean)
plot(temp, pch=16)
curve(exp(-exp(alpha_hat[ind] + beta_hat[ind]*x)), add=TRUE, lwd=2, col='orange')

#Plot expected points vs distance for Justin Tucker



#Calculate Breakpoint and Breakpoint LB using Linear EPA model
par(mfrow=c(2,2))
target <- function(d, pars, shift=0){
  p <- exp(-exp(pars[1]+pars[2]*(d-shift)))
  res <- (2*p - (1-p)*(6-2*(110-d)/25))^2
}
bp_guess <- bp_lb <- rep(NA, numI)
for(i in 1:numI){
  bp <- rep(NA, 3000)
  for(j in 1:3000){
    opt <- optim(33, target, pars=c(e$alpha[j,i], e$beta[j,i]), method='Brent', lower=18, upper=76)
    bp[j] <- opt$par
    if(opt$convergence!=0) warning('no convergence')
  }
  bp_guess[i] <- mean(bp)
  bp_lb[i] <- quantile(bp, .05)
}
hist(bp_guess, main='Breakpoint (linear)')
hist(bp_lb, main='Breakpoint LB (linear)')


#Calculate Breakpoint and Breakpoint LB using 
#multinomial logistic regression EPA model



#WITH Multinomial Logistic Regression EPA
target2 <- function(d, pars, phi, shift=0){
  p <- exp(-exp(pars[1]+pars[2]*(d-shift)))
  res <- ((3-phi[75])*p - (1-p)*phi[110-d])^2
}
bp_guess2 <- bp_lb2 <- rep(NA, numI)
for(i in 1:numI){
  bp <- rep(NA, 3000)
  for(j in 1:3000){
    opt <- optim(33, target2, pars=c(e$alpha[j,i], e$beta[j,i]), phi=phi[[1]][,2], method='Brent', lower=18, upper=80)
    bp[j] <- opt$par
    if(opt$convergence!=0) warning('no convergence')
  }
  bp_guess2[i] <- mean(bp)
  bp_lb2[i] <- quantile(bp, .05)
}
hist(bp_guess2, main='Breakpoint (regression)')
hist(bp_lb2, main='Breakpoint LB (regression)')




