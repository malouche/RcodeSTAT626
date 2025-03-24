data {
  int<lower=0> N;             // number of observations
  int<lower=0> n_rats;        // number of rats
  int<lower=1, upper=n_rats> rat[N];  // rat id for each observation
  vector[N] age_centered;     // centered age variable
  vector[N] weight;           // observed weights
}

parameters {
  // population-level parameters
  real a_c;                   // population intercept at mean age
  real b_c;                   // population slope
  
  // variance parameters
  real<lower=0> sigma_a;      // standard deviation for rat intercepts
  real<lower=0> sigma_b;      // standard deviation for rat slopes
  real<lower=0> sigma_y;      // residual standard deviation
  
  // individual rat parameters (non-centered parameterization)
  vector[n_rats] a_raw;       // standardized rat-specific intercepts
  vector[n_rats] b_raw;       // standardized rat-specific slopes
  
  // correlation between intercepts and slopes
  real<lower=-1, upper=1> rho;// correlation coefficient
}

transformed parameters {
  // individual rat parameters
  vector[n_rats] a;           // rat-specific intercepts
  vector[n_rats] b;           // rat-specific slopes
  
  // non-centered parameterization with correlation
  for (i in 1:n_rats) {
    a[i] = a_c + sigma_a * a_raw[i];
    b[i] = b_c + sigma_b * (rho * a_raw[i] + sqrt(1 - rho^2) * b_raw[i]);
  }
}

model {
  // priors for population parameters
  a_c ~ normal(200, 50);      // prior for population intercept
  b_c ~ normal(5, 5);         // prior for population slope
  
  // priors for variance parameters
  sigma_a ~ cauchy(0, 20);    // prior for intercept SD
  sigma_b ~ cauchy(0, 5);     // prior for slope SD
  sigma_y ~ cauchy(0, 20);    // prior for residual SD
  
  // priors for standardized random effects
  a_raw ~ normal(0, 1);
  b_raw ~ normal(0, 1);
  
  // prior for correlation
  rho ~ uniform(-1, 1);
  
  // likelihood
  for (i in 1:N) {
    weight[i] ~ normal(a[rat[i]] + b[rat[i]] * age_centered[i], sigma_y);
  }
}

generated quantities {
  // population intercept at birth
  real a_0 = a_c - b_c * 22;  // 22 is xbar
  
  // predicted values for model checking
  vector[N] y_pred;
  for (i in 1:N) {
    y_pred[i] = normal_rng(a[rat[i]] + b[rat[i]] * age_centered[i], sigma_y);
  }
}