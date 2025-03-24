data<- list(x = c(8.0, 15.0, 22.0, 29.0, 36.0), xbar = 22, N = 30, T = 5,   
            Y = structure(
              .Data = c(151, 199, 246, 283, 320,
                        145, 199, 249, 293, 354,
                        147, 214, 263, 312, 328,
                        155, 200, 237, 272, 297,
                        135, 188, 230, 280, 323,
                        159, 210, 252, 298, 331,
                        141, 189, 231, 275, 305,
                        159, 201, 248, 297, 338,
                        177, 236, 285, 350, 376,
                        134, 182, 220, 260, 296,
                        160, 208, 261, 313, 352,
                        143, 188, 220, 273, 314,
                        154, 200, 244, 289, 325,
                        171, 221, 270, 326, 358,
                        163, 216, 242, 281, 312,
                        160, 207, 248, 288, 324,
                        142, 187, 234, 280, 316,
                        156, 203, 243, 283, 317,
                        157, 212, 259, 307, 336,
                        152, 203, 246, 286, 321,
                        154, 205, 253, 298, 334,
                        139, 190, 225, 267, 302,
                        146, 191, 229, 272, 302,
                        157, 211, 250, 285, 323,
                        132, 185, 237, 286, 331,
                        160, 207, 257, 303, 345,
                        169, 216, 261, 295, 333,
                        157, 205, 248, 289, 316,
                        137, 180, 219, 258, 291,
                        153, 200, 244, 286, 324),
              .Dim = c(30,5)))

library(dplyr)
library(tidyr)
library(ggplot2)
df <- as.data.frame(data$Y)
colnames(df) <- paste0("Week_", data$x)
df$Rat <- factor(1:data$N)

df_long <- pivot_longer(df, cols = starts_with("Week"), names_to = "Week", values_to = "Weight") %>%
  mutate(Week = as.numeric(sub("Week_", "", Week)))

# Plot growth curves with ggplot
ggplot(df_long, aes(x = Week, y = Weight, group = Rat, color = Rat)) +
  geom_line(alpha = 0.7) +
  labs(title = "Growth Curves of Young Rats",
       x = "Age (days)",
       y = "Weight (grams)") +
  theme_minimal() +
  theme(legend.position = "none")

## Hierarchical Model (frequentist approach)


xbar<-22
# center the age variable
df_long$age_centered <- df_long$Week - xbar

# Install required packages if needed
# install.packages("lme4")

# Load necessary libraries
library(lme4)
library(broom.mixed)
library(ggplot2)

# Assuming df_long is your data frame with columns:
# Rat (factor), Week (numeric), Weight (numeric), age_centered (numeric)

# Fit the linear mixed-effects model
rat_lmm <- lmer(Weight ~ age_centered + (1 + age_centered | Rat), 
                data = df_long, 
                REML = TRUE)

# View the model summary
summary(rat_lmm)

### Bayesian model 

library(rstan)
options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)

# Data for Stan model
# Load necessary libraries
library(rstan)
library(ggplot2)
library(bayesplot)
library(tidybayes)

# Set Stan options for faster computation
options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)

# Define the Stan model
stan_code <- "
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
"

# Prepare data for Stan
stan_data <- list(
  N = nrow(df_long),
  n_rats = length(unique(df_long$Rat)),
  rat = as.numeric(df_long$Rat),  # Convert factor to numeric
  age_centered = df_long$age_centered,
  weight = df_long$Weight
)

# Fit the model
rat_fit <- stan(
  model_code = stan_code,
  data = stan_data,
  iter = 2000,
  warmup = 1000,
  chains = 4,
  seed = 123
)

# Check the model
print(rat_fit, pars = c("a_c", "b_c", "a_0", "sigma_a", "sigma_b", "sigma_y", "rho"))

# Check convergence

# Load required packages
library(rstan)
library(coda)
library(ggmcmc)
library(bayesplot)
library(gridExtra)

# Convert Stan fit to mcmc.list for coda
stan_mcmc <- As.mcmc.list(rat_fit)

# Convert to ggmcmc format
ggfit <- ggs(rat_fit)

# 1. Traceplots with ggmcmc
# Filter to key parameters
key_params <- c("a_c", "b_c", "a_0", "sigma_a", "sigma_b", "sigma_y", "rho")
ggfit_key <- ggfit[ggfit$Parameter %in% key_params,]

# Traceplot
trace_plot <- ggs_traceplot(ggfit_key) + 
  labs(title = "Traceplots for Key Parameters") +
  theme_minimal()
print(trace_plot)

# 2. Density plots
density_plot <- ggs_density(ggfit_key) + 
  labs(title = "Posterior Density Plots") +
  theme_minimal()
print(density_plot)

# 3. Autocorrelation plots
acf_plot <- ggs_autocorrelation(ggfit_key) + 
  labs(title = "Autocorrelation Plots") +
  theme_minimal()
print(acf_plot)

# 4. Running mean plots
running_plot <- ggs_running(ggfit_key) + 
  labs(title = "Running Mean Plots") +
  theme_minimal()
print(running_plot)

# 5. Cross-correlation plot
# For population-level parameters
crosscorr_plot <- ggs_crosscorrelation(ggfit_key) +
  labs(title = "Parameter Cross-Correlations") +
  theme_minimal()
print(crosscorr_plot)

# 6. Geweke diagnostics
geweke_stats <- geweke.diag(stan_mcmc, frac1=0.1, frac2=0.5)
geweke_plot <- ggs_geweke(ggfit_key%>%filter(Parameter=='a_c')) +
  labs(title = "Geweke Diagnostics") +
  theme_minimal()
print(geweke_plot)

# Print summary of Geweke test
cat("Geweke test summaries:\n")
print(geweke_stats)

# 7. Gelman-Rubin diagnostic (Rhat)
gelman_stats <- gelman.diag(stan_mcmc)
print(gelman_stats)

# Visual representation of Gelman-Rubin statistics
rhat_values <- summary(rat_fit)$summary[, "Rhat"]
rhat_params <- names(rhat_values)

# Create a dataframe for plotting
rhat_df <- data.frame(
  parameter = rhat_params,
  rhat = rhat_values
)

# Filter to just key parameters for readability
rhat_df_key <- rhat_df[rhat_df$parameter %in% key_params,]

# Plot Rhat values
rhat_plot <- ggplot(rhat_df_key, aes(x = parameter, y = rhat)) +
  geom_point(size = 3) +
  geom_hline(yintercept = 1.1, linetype = "dashed", color = "red") +
  theme_minimal() +
  coord_flip() +
  labs(title = "Gelman-Rubin Statistics (Rhat)",
       subtitle = "Values below 1.1 indicate convergence",
       x = "Parameter",
       y = "Rhat Value")
print(rhat_plot)



# 9. Summary statistics of posterior distributions
summary_stats <- summary(rat_fit, pars = key_params)$summary
print(round(summary_stats, 3))

# 10. Effective sample size (ESS)
n_eff <- summary(rat_fit, pars = key_params)$summary[, "n_eff"]
print(data.frame(parameter = key_params, n_eff = n_eff))

# Extract posterior samples
posterior_samples <- extract(rat_fit)

# Population parameters
mean(posterior_samples$a_c)        # Average weight at mean age (22 weeks)
mean(posterior_samples$b_c)        # Average growth rate
mean(posterior_samples$a_0)        # Extrapolated weight at birth
mean(posterior_samples$rho)        # Correlation between intercepts and slopes

# Plot posterior distributions of key parameters
mcmc_areas(rat_fit, pars = c("a_c", "b_c", "a_0", "rho"))

# Extract individual rat parameters
a_rat <- colMeans(posterior_samples$a)
b_rat <- colMeans(posterior_samples$b)

# Create data frame for predicted curves
rat_ids <- unique(df_long$Rat)
pred_data <- expand.grid(
  Rat = rat_ids,
  Week = seq(0, 40, by = 1)
)
pred_data$age_centered <- pred_data$Week - 22

# Add predictions
for (i in 1:nrow(pred_data)) {
  rat_idx <- which(rat_ids == pred_data$Rat[i])
  pred_data$pred_weight[i] <- a_rat[rat_idx] + b_rat[rat_idx] * pred_data$age_centered[i]
}

# Plot individual growth curves with population average
ggplot() +
  # Raw data points
  geom_point(data = df_long, aes(x = Week, y = Weight, group = Rat), alpha = 0.5) +
  # Individual predicted curves
  geom_line(data = pred_data, aes(x = Week, y = pred_weight, group = Rat), alpha = 0.3) +
  # Population average curve
  geom_abline(
    intercept = mean(posterior_samples$a_c) - mean(posterior_samples$b_c) * 22,
    slope = mean(posterior_samples$b_c),
    color = "red", linewidth = 1.5
  ) +
  labs(
    title = "Rat Growth Curves (Bayesian Hierarchical Model)",
    subtitle = paste0(
      "Population intercept at birth: ", 
      round(mean(posterior_samples$a_0), 1),
      " g"
    ),
    x = "Age (weeks)",
    y = "Weight (g)"
  ) +
  theme_minimal()

# Check predictive accuracy
ppc_dens_overlay(df_long$Weight, posterior_samples$y_pred[1:50, ])
