library(dplyr)      # For data manipulation
library(ggpubr)     # For creating publication-ready plots
library(ggplot2)    # For data visualization
# Load RStan package for Bayesian modeling
library(rstan)
# Set options to optimize RStan performance
rstan_options(auto_write = TRUE)  # Cache compiled Stan programs
options(mc.cores = parallel::detectCores())  # Use multiple cores for parallel processing


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

# Create a sequence of age values for prediction
# This will be used to generate a smooth growth curve
xx <- seq(0.5, 32, by = 0.5)  # From 0.5 to 32 years in 0.5 year increments
N_pred <- length(xx)  # Number of prediction points


# Prepare data including new prediction points
stan_data_pred <- list(
  N = nrow(dt),
  x = dt$x,
  Y = dt$y,
  N_new = length(xx),
  x_new = xx
)

# Fit the model with prediction code
fit_pred <- stan(file = 'dugongActivity/nonlin_pred.stan', data = stan_data_pred,
                 iter = 2000, chains = 4, seed = 123)

# Extract predictions
predictions <- extract(fit_pred)
y_pred_mean <- colMeans(predictions$y_pred)
y_pred_intervals <- apply(predictions$y_pred, 2, quantile, probs = c(0.025, 0.975))


# Create a data frame for plotting predictions and intervals
plot_data <- data.frame(
  Age = xx,                            # Our sequence of age values
  Mean = y_pred_mean,                  # Mean predictions
  Lower = y_pred_intervals[1, ],       # Lower 2.5% interval
  Upper = y_pred_intervals[2, ]        # Upper 97.5% interval
)

# Original data points
original_data <- data.frame(Age = dt$x, Weight = dt$y)

# Create the plot
ggplot() +
  # Add the prediction interval ribbon
  geom_ribbon(data = plot_data, 
              aes(x = Age, ymin = Lower, ymax = Upper),
              fill = "lightblue", alpha = 0.5) +
  
  # Add the mean prediction line
  geom_line(data = plot_data,
            aes(x = Age, y = Mean),
            color = "blue", size = 1) +
  
  # Add the original data points
  geom_point(data = original_data,
             aes(x = Age, y = Weight),
             size = 3, alpha = 0.7) +
  
  # Add labels and title
  labs(x = "Age (years)",
       y = "Weight (m)",
       title = "Dugong Growth Curve with 95% Prediction Intervals",
       subtitle = "Bayesian Nonlinear Regression Model") +
  
  # Customize theme for better appearance
  theme_bw() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 12, color = "darkblue"),
    axis.title = element_text(face = "bold"),
    panel.grid.minor = element_blank(),
    legend.position = "bottom"
  )
