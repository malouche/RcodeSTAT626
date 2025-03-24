data {
  int<lower=0> N;      // number of observations
  vector[N] x;         // age
  vector[N] Y;         // weight
  
  // New data for prediction
  int<lower=0> N_new;  // number of new points
  vector[N_new] x_new; // new predictor values
}
parameters {
  real alpha;
  real beta;
  real<lower=0.5, upper=1> gamma;
  real<lower=0> sigma;
}
model {
  // Priors and likelihood as before
  alpha ~ normal(0, 1e6);
  beta ~ normal(0, 1e6);
  sigma ~ cauchy(0, 5);
  
  for (i in 1:N)
    Y[i] ~ normal(alpha - beta * pow(gamma, x[i]), sigma);
}
generated quantities {
  // Point predictions
  vector[N_new] y_pred;
  
  // Predictive distribution (includes uncertainty)
  vector[N_new] y_pred_rng;
  
  for (i in 1:N_new) {
    // Mean prediction
    y_pred[i] = alpha - beta * pow(gamma, x_new[i]);
    
    // Random draw from predictive distribution
    y_pred_rng[i] = normal_rng(y_pred[i], sigma);
  }
}