library(dplyr)      # For data manipulation
library(ggpubr)     # For creating publication-ready plots
library(ggplot2)    # For data visualization
library(caret)      # For model training and LOOCV

# Data provided from Ratkowsky (1983)
dataList <- list(
  x = c(1.0, 1.5, 1.5, 1.5, 2.5, 4.0, 5.0, 5.0, 7.0,
        8.0, 8.5, 9.0, 9.5, 9.5, 10.0, 12.0, 12.0, 13.0,
        13.0, 14.5, 15.5, 15.5, 16.5, 17.0, 22.5, 29.0, 31.5),
  Y = c(1.80, 1.85, 1.87, 1.77, 2.02, 2.27, 2.15, 2.26, 2.47,
        2.19, 2.26, 2.40, 2.39, 2.41, 2.50, 2.32, 2.32, 2.43,
        2.47, 2.56, 2.65, 2.47, 2.64, 2.56, 2.70, 2.72, 2.57)
)

library(rstan)
# Set options to optimize RStan performance
rstan_options(auto_write = TRUE)  # Cache compiled Stan programs
options(mc.cores = parallel::detectCores())  # Use multiple cores for parallel processing

# Prepare data for the linear Bayesian model in Stan format
stan_data_lin <- list(
  N = nrow(dt),  # Number of observations
  lx = dt$lx,    # Log-transformed age values
  Y = dt$y       # Weight values
)

# Fit the linear Bayesian model using Stan
# - iter = 2000: Number of iterations for each chain
# - chains = 4: Number of Markov chains to run
# - seed = 123: For reproducibility
fit_lin <- stan(file = 'dugongActivity/loglin.stan', data = stan_data_lin,
                iter = 2000, chains = 4, seed = 123)

# Print summary statistics for the key parameters
print(fit_lin, pars = c("beta0", "beta1", "sigma"))

## Convergence Diagnosis

library(coda)
library(mcmcplots)
library(ggmcmc)

fit_lin.gg<-ggs(fit_lin)
fit_lin.gg%>%head()

rhat1<-ggs_Rhat(fit_lin.gg,family='beta')
rhat1$data

ggs_geweke(fit_lin.gg)


ggs_density(fit_lin.gg,family='beta',hpd = T)+theme_bw()+theme(legend.position = 'top')
