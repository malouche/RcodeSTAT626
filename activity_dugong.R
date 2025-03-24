# Load required libraries
library(dplyr)      # For data manipulation
library(ggpubr)     # For creating publication-ready plots
library(ggplot2)    # For data visualization

# Data provided from Ratkowsky (1983) - Dataset contains measurements from 27 dugongs
# x = age in years, Y = weight in meters
dataList <- list(
  x = c(1.0, 1.5, 1.5, 1.5, 2.5, 4.0, 5.0, 5.0, 7.0,
        8.0, 8.5, 9.0, 9.5, 9.5, 10.0, 12.0, 12.0, 13.0,
        13.0, 14.5, 15.5, 15.5, 16.5, 17.0, 22.5, 29.0, 31.5),
  Y = c(1.80, 1.85, 1.87, 1.77, 2.02, 2.27, 2.15, 2.26, 2.47,
        2.19, 2.26, 2.40, 2.39, 2.41, 2.50, 2.32, 2.32, 2.43,
        2.47, 2.56, 2.65, 2.47, 2.64, 2.56, 2.70, 2.72, 2.57),
  N = 27
)

# Convert the list to a data frame for easier manipulation
dt <- data.frame(x = dataList$x, y = dataList$Y)

# Create log-transformed age variable (lx)
# Log transformation can help linearize non-linear relationships and stabilize variance
dt <- dt %>% mutate(lx = log(x))

# Display first few rows to verify data structure
head(dt)

# Create a scatter plot with regression line to visualize relationship between log-age and weight
# This helps to visually assess if a linear relationship exists after log transformation
ggscatter(data = dt, x = 'lx', y = 'y', add = 'reg.line', 
          xlab = 'log-age', ylab = 'Weight')

# Fit the linear model using ordinary least squares (OLS)
# This is the frequentist approach modeling weight as a function of log-age
freq_model <- lm(y ~ lx, data = dt)
summary(freq_model)
# The summary provides coefficient estimates, standard errors, t-values, p-values,
# R-squared, adjusted R-squared, and residual standard error

# Perform Leave-One-Out Cross-Validation (LOOCV) using caret package
# LOOCV helps assess model performance and generalizability
library(caret)
set.seed(123)  # For reproducibility of random processes
loo_fit <- train(y ~ lx, method = "lm", data = dt, 
                 trControl = trainControl(method = "LOOCV"))
print(loo_fit)
# This output provides RMSE (Root Mean Square Error), R-squared, and MAE (Mean Absolute Error)

# Load RStan package for Bayesian modeling
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

# Stan code for linear model would be stored in stan_lin_code variable
# Fit the linear Bayesian model using Stan
# - iter = 2000: Number of iterations for each chain
# - chains = 4: Number of Markov chains to run
# - seed = 123: For reproducibility
fit_lin <- stan(file  = "stan_lin_code.stan", data = stan_data_lin,
                iter = 2000, chains = 4, seed = 123)

# Print summary statistics for the key parameters
print(fit_lin, pars = c("beta0", "beta1", "sigma"))

# Generate trace plots to assess convergence of MCMC chains
# Trace plots should show good mixing with no trends or patterns
traceplot(fit_lin, pars = c("beta0", "beta1", "sigma"))

# Extract log-likelihood for LOO cross-validation computation
log_lik <- extract_log_lik(fit_lin, merge_chains = FALSE)
library(loo)
loo_result <- loo(log_lik)
print(loo_result)
# This provides LOOIC (LOO Information Criterion) and effective sample size

# Prepare data for the nonlinear Bayesian model
# Now using original age values rather than log-transformed
stan_data_nonlin <- list(
  N = nrow(dt),
  x = dt$x,
  Y = dt$y
)

# Fit the nonlinear model with RStan
# The model is: Weight = alpha - beta * gamma^age
fit_nonlin <- stan(file = "stan_nonlin_code.stan", data = stan_data_nonlin,
                   iter = 2000, chains = 4, seed = 123)

# Print summary statistics for the nonlinear model parameters
print(fit_nonlin, pars = c("alpha", "beta", "gamma", "sigma"))

# Generate trace plots for nonlinear model parameters to assess convergence
traceplot(fit_nonlin, pars = c("alpha", "beta", "gamma", "sigma"))

# Prepare data including new prediction points
stan_data_pred <- list(
  N = nrow(dt),
  x = dt$x,
  Y = dt$y,
  N_new = length(xx),
  x_new = xx
)

# Fit the model with prediction code
fit_pred <- stan(model_code = stan_pred_code, data = stan_data_pred,
                 iter = 2000, chains = 4, seed = 123)

# Extract predictions
predictions <- extract(fit_pred)
y_pred_mean <- colMeans(predictions$y_pred)
y_pred_intervals <- apply(predictions$y_pred, 2, quantile, probs = c(0.025, 0.975))

# Create a data frame for plotting
df_pred <- data.frame(Age = xx, Weight = Y_pred)

# Plot the fitted nonlinear growth curve with the observed data points
ggplot() +
  # Add the fitted growth curve as a blue line
  geom_line(data = df_pred, aes(x = Age, y = Weight), color = "blue") +
  # Add the observed data points
  geom_point(data = dt, aes(x = x, y = y)) +
  # Add labels and title
  labs(x = "Age", y = "Weight", title = "Nonlinear Dugong Growth Curve") +
  # Use the black and white theme for a cleaner look
  theme_bw()