data {
  int<lower=0> N;         // number of observations
  vector[N] x;            // predictor
  vector[N] y;            // response
}

parameters {
  real alpha;             // intercept
  real beta;              // slope
  real<lower=0> sigma;  // residual standard deviation
}

model {
  // Priors
  alpha ~ normal(17.39, 100);
  beta ~ normal(0.106, 100);
  sigma ~ normal(23.74,100);
  // Likelihood
  y ~ normal(alpha + beta * x, sigma);
}


generated quantities {
  vector[N] y_pred;
  for (i in 1:N) {
    y_pred[i] = normal_rng(alpha + beta * x[i], sigma);
  }
}
