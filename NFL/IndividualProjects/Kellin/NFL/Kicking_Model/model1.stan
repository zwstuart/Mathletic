data {
  int<lower=0> N;
  int<lower=1> I[N];
  int<lower=1> J[N]; 
  int<lower=1> numJ;
  int<lower=1> numI;
  int<lower=0,upper=1> y[N];
  real<lower=0> x[N];
}
parameters {
  real alpha0;
  real beta0;
  real<lower=0> sigma_a;
  real<lower=0> sigma_b;
  real<lower=0> sigma_t;
  real alpha[numI];
  real beta[numI];
  real theta[numJ];
}
model {
  vector[N] p;
  
  alpha0 ~ normal(0, 100);
  beta0 ~ normal(0, 100);
  sigma_a ~ cauchy(0, .1);
  sigma_b ~ cauchy(0, .1);
  sigma_t ~ normal(0, .025);
  
  for (j in 1:numJ) {
    theta[j] ~ normal(1, sigma_t);
  }
  
  for (i in 1:numI) {
    alpha[i] ~ normal(alpha0, sigma_a);
    beta[i] ~ normal(beta0, sigma_b);
  }
  
  for (n in 1:N){
    p[n] = exp(-exp(alpha[I[n]] + theta[J[n]]*beta[I[n]]*x[n]));
  }
  
  y ~ bernoulli(p);
}