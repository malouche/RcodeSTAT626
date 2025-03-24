data {
  int<lower=0> N;      // number of observations
  vector[N] x;         // age
  vector[N] Y;         // weight
}
parameters {
  real alpha;
  real beta;
  real<lower=0.5, upper=1> gamma;  // restrict gamma between 0.5 and 1
  real<lower=0> sigma;
}
model {
  // Flat priors
  alpha ~ normal(0, 1e6);
  beta ~ normal(0, 1e6);
  sigma ~ cauchy(0, 5);
  
  // Likelihood
  for (i in 1:N)
    Y[i] ~ normal(alpha - beta * pow(gamma, x[i]), sigma);
}