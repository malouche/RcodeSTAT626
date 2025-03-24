data {
  int<lower=0> N;         // number of observations
  vector[N] lx;           // log-transformed age
  vector[N] Y;            // weight
}
parameters {
  real beta0;             // intercept
  real beta1;             // slope
  real<lower=0> sigma;    // error standard deviation
}
model {
  // Flat priors
  beta0 ~ normal(0, 1e6);
  beta1 ~ normal(0, 1e6);
  sigma ~ cauchy(0, 5);
  
  // Likelihood
  Y ~ normal(beta0 + beta1 * lx, sigma);
}