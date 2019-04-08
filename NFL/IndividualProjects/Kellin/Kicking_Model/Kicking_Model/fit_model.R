#Prepare Stan data
dat <- list(N=N, I=I, J=J, numJ=numJ, numI=numI, y=y, x=x); 
#Run 2 chains in parallel
fit <- stan(file='model1.stan', 
            data = dat, iter = 3000, chains = 2, sample_file = 'stan_out2.csv',
            verbose = TRUE, init="0", cores=2) 

e <- extract(fit, permuted=TRUE)
#save(fit, file='stan_out1.rda')

