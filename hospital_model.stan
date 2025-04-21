data {
  int <lower=0> N;
  int <lower=0> n[N]; 
  int <lower=0> r[N];
}

parameters {
  real <lower=0, upper=1> p[N];
}

model {
  p ~ beta(1,1);
  r ~ binomial(n,p);
}

generated quantities {
  int future_deaths[N];
  
  for (i in 1:N) {
    future_deaths[i] = binomial_rng(50, p[i]);  // simulate 50 future operations
  }
}
