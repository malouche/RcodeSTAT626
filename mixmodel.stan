//
// This Stan program defines a simple model, with a
// vector of values 'y' modeled as normally distributed
// with mean 'mu' and standard deviation 'sigma'.
//
// Learn more about model development with Stan at:
//
//    http://mc-stan.org/users/interfaces/rstan.html
//    https://github.com/stan-dev/rstan/wiki/RStan-Getting-Started
//

// The input data is a vector 'y' of length 'N'.
data {
  int<lower=0> N;
  vector[N] y;
}

// The parameters accepted by the model. Our model
// accepts two parameters 'mu' and 'sigma'.
parameters {
  real mu1;
  real mu2;
  real<lower=0,upper=1> pi;
  real<lower=0> sigma;
}

// The model to be estimated. We model the output
// 'y' to be normally distributed with mean 'mu'
// and standard deviation 'sigma'.
model {
  mu1 ~ normal(0,1);
  mu2 ~ normal(0,1);
  sigma ~ gamma(1,1);
  pi ~ beta(1,1);
  for(n in 1:N)
  target += log_sum_exp(log(pi)+normal_lpdf(y[n]|mu1,sigma),
                   log(1-pi)+normal_lpdf(y[n]|mu2,sigma));
}

