
data {
  int<lower=0> N;
  int<lower=0,upper=1> Survived[N] ; // Target variable
  vector[N] Age;
  vector[N] Sex;
}

parameters {
  real b0;
  real b1;
  real b2;
}


model {
  // priors
  b0 ~ normal(0,100);
  b1 ~ normal(0,100);
  b2 ~ normal(0,100);
  // likelihood
  Survived ~ bernoulli_logit(b0+b1*Sex+b2*Age);
}

generated quantities {
  vector[N] prob_survive;
  prob_survive = inv_logit(b0+b1*Sex+b2*Age);
}

