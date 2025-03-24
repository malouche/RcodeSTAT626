//
// This Stan program defines a simple model, with a
// vector of values 'y' modeled as normally distributed
// with mean 'mu' and standard deviation 'sigma'.
data {
  int<lower=0> N;
   vector[N] x;
  vector[N] y;
}

// The parameters accepted by the model. Our model
// accepts two parameters 'mu' and 'sigma'.
parameters {
  real beta0;
  real beta1;
  real<lower=0> sigma;
}

// The model to be estimated. We model the output
// 'y' to be normally distributed with mean 'mu'
// and standard deviation 'sigma'.
model {
  beta0 ~ normal(0,1e6);
  beta1 ~ normal(0,1e6);
  sigma ~ cauchy(0,5);
  y ~ normal(beta0+beta1*x , sigma);
}

