data {
  int<lower=1> N;              // total mice
  int<lower=1> M;              // number of groups
  vector[N] time;              // observed times
  int<lower=0,upper=1> event[N]; // 1 = death, 0 = censored
  int<lower=1,upper=M> group[N];
}
parameters {
  vector[M] beta;              // log-scale parameters
  real<lower=0> shape;         // Weibull shape r
}
transformed parameters {
  vector[M] scale = exp(beta); // group scales mu_i
}
model {
  // priors
  beta  ~ normal(0, 100);      // precision 0.0001^{-1}
  shape ~ gamma(1, 0.0001);

  // likelihood
  for (n in 1:N) {
    real mu = scale[group[n]];
    if (event[n] == 1)
      target += weibull_lpdf(time[n] | shape, mu);
    else
      target += weibull_lccdf(time[n] | shape, mu); // right censor
  }
}
generated quantities {
  vector[M] median;
  vector[3] contrast;          // vs. group 1
  for (i in 1:M)
    median[i] = pow(log(2) * exp(-beta[i]), 1 / shape);
  contrast[1] = beta[2] - beta[1];
  contrast[2] = beta[3] - beta[1];
  contrast[3] = beta[4] - beta[1];
}