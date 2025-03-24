dataList <- list(
  x = c(1.0, 1.5, 1.5, 1.5, 2.5, 4.0, 5.0, 5.0, 7.0,
        8.0, 8.5, 9.0, 9.5, 9.5, 10.0, 12.0, 12.0, 13.0,
        13.0, 14.5, 15.5, 15.5, 16.5, 17.0, 22.5, 29.0, 31.5),
  Y = c(1.80, 1.85, 1.87, 1.77, 2.02, 2.27, 2.15, 2.26, 2.47,
        2.19, 2.26, 2.40, 2.39, 2.41, 2.50, 2.32, 2.32, 2.43,
        2.47, 2.56, 2.65, 2.47, 2.64, 2.56, 2.70, 2.72, 2.57),
  N = 27
)

dt <- data.frame(x = dataList$x, y = dataList$Y)

# Create the log-transformed age variable (lx)
dt <- dt %>% mutate(lx = log(x))

# Display first few rows
head(dt)

library(ggpubr)
library(ggplot2)

ggscatter(data = dt,x="x",y="y")+xlab("age")+ylab("length")

dt$lx=log(dt$x)

ggscatter(data = dt,x="lx",y="y")+xlab("log(age)")+ylab("length")

model0<-lm(y~x, data=dt)
summary(model0)
model1<-lm(y~lx,data=dt)

summary(model1)

dt$ly<-log(dt$y)

model2<-lm(ly~x,data=dt)

summary(model2)

library(caret)

loo_model0 <- train(y ~ x, method = "lm", data = dt, 
                    trControl = trainControl(method = "LOOCV"))

loo_model1 <- train(y ~ lx, method = "lm", data = dt, 
                    trControl = trainControl(method = "LOOCV"))

loo_model2 <- train(ly ~ x, method = "lm", data = dt, 
                    trControl = trainControl(method = "LOOCV"))

results<-rbind(loo_model0$results[,2:4],
               loo_model1$results[,2:4],
               loo_model2$results[,2:4])

rownames(results)=c("model0","model1","model2")

results


new_data <- data.frame(x = seq(min(dt$x), max(dt$x), length.out = 100))
pred <- predict(model2, newdata = new_data, interval = "confidence")
pred_df <- cbind(new_data, pred)

exp(pred_df$fit)

# Plot using ggplot2
ggplot(dt, aes(x = x, y = y)) +
  geom_point() +
  geom_line(data = pred_df, aes(x = x, y = exp(fit)), color = "blue") +
  geom_ribbon(data = pred_df, aes(x = x, ymin = exp(lwr), ymax = exp(upr)), alpha = 0.2,inherit.aes = FALSE) +
  labs(x = "x", y = "y", title = "Scatter plot with fitted curve and 95% CI") +
  theme_minimal()


### Bayesian Model 0

library (rstan)
rstan_options(auto_write = TRUE )
options(mc.cores = parallel::detectCores())
# Prepare data for Stan
data_model0 <-list(
  N = nrow (dt),
  x = dt$x,
  y = dt$y
)


fit_model0 <- stan ( file = "model0.stan", data = data_model0 ,
                   iter = 20000 ,warmup = 15000, chains = 4, seed = 123)
print(fit_model0)

library(ggmcmc)

ggs_traceplot(ggs(fit_model0),"beta0")+theme_bw()
ggs_traceplot(ggs(fit_model0),"beta1")+theme_bw()
ggs_traceplot(ggs(fit_model0),"sigma")+theme_bw()

ggs_running(ggs(fit_model0),"beta0")+theme_bw()
ggs_running(ggs(fit_model0),"beta1")+theme_bw()
ggs_running(ggs(fit_model0),"sigma")+theme_bw()

ggs_autocorrelation(ggs(fit_model0),"beta0")+theme_bw()
ggs_autocorrelation(ggs(fit_model0),"beta1")+theme_bw()
ggs_autocorrelation(ggs(fit_model0),"sigma")+theme_bw()

ggs_Rhat(ggs(fit_model0),"beta1")+theme_bw()
ggs_Rhat(ggs(fit_model0),"beta0")+theme_bw()
ggs_Rhat(ggs(fit_model0),"sigma")+theme_bw()

ggs_crosscorrelation(ggs(fit_model0))+theme_bw()

ggs_geweke(ggs(fit_model0))+theme_bw()


fit_model1 <- stan ( file = "model1.stan", data = data_model0 ,
                     iter = 20000 ,warmup = 15000, chains = 4, seed = 123)
print(fit_model1)


ggs_traceplot(ggs(fit_model1),"beta0")+theme_bw()
ggs_traceplot(ggs(fit_model1),"beta1")+theme_bw()
ggs_traceplot(ggs(fit_model1),"sigma")+theme_bw()

ggs_running(ggs(fit_model1),"beta0")+theme_bw()
ggs_running(ggs(fit_model1),"beta1")+theme_bw()
ggs_running(ggs(fit_model1),"sigma")+theme_bw()

ggs_autocorrelation(ggs(fit_model1),"beta0")+theme_bw()
ggs_autocorrelation(ggs(fit_model1),"beta1")+theme_bw()
ggs_autocorrelation(ggs(fit_model1),"sigma")+theme_bw()

ggs_Rhat(ggs(fit_model1),"beta1")+theme_bw()
ggs_Rhat(ggs(fit_model1),"beta0")+theme_bw()
ggs_Rhat(ggs(fit_model1),"sigma")+theme_bw()

ggs_crosscorrelation(ggs(fit_model1))+theme_bw()

ggs_geweke(ggs(fit_model1))+theme_bw()


data_model2 <-list(
  N = nrow (dt),
  x = dt$x,
  ly = dt$ly
)


fit_model2 <- stan ( file = "model2.stan", data = data_model2 ,
                     iter = 20000 ,warmup = 15000, chains = 4, seed = 123)
print(fit_model2)


ggs_traceplot(ggs(fit_model2),"beta0")+theme_bw()
ggs_traceplot(ggs(fit_model2),"beta1")+theme_bw()
ggs_traceplot(ggs(fit_model2),"sigma")+theme_bw()

ggs_running(ggs(fit_model2),"beta0")+theme_bw()
ggs_running(ggs(fit_model2),"beta1")+theme_bw()
ggs_running(ggs(fit_model2),"sigma")+theme_bw()

ggs_autocorrelation(ggs(fit_model2),"beta0")+theme_bw()
ggs_autocorrelation(ggs(fit_model2),"beta1")+theme_bw()
ggs_autocorrelation(ggs(fit_model2),"sigma")+theme_bw()

ggs_Rhat(ggs(fit_model2),"beta1")+theme_bw()
ggs_Rhat(ggs(fit_model2),"beta0")+theme_bw()
ggs_Rhat(ggs(fit_model2),"sigma")+theme_bw()

ggs_crosscorrelation(ggs(fit_model2))+theme_bw()

ggs_geweke(ggs(fit_model2))+theme_bw()

data_list <-list(
  N = nrow (dt),
  x = dt$x,
  y = dt$y
)


fit_model3 <- stan ( file = "model3.stan", data = data_list ,
                     iter = 20000 ,warmup = 15000, chains = 4, seed = 123)
print(fit_model3)


ggs_traceplot(ggs(fit_model3),"alpha")+theme_bw()
ggs_traceplot(ggs(fit_model3),"beta")+theme_bw()
ggs_traceplot(ggs(fit_model3),"gamma")+theme_bw()
ggs_traceplot(ggs(fit_model3),"sigma")+theme_bw()

ggs_running(ggs(fit_model3),"alpha")+theme_bw()
ggs_running(ggs(fit_model3),"beta")+theme_bw()
ggs_running(ggs(fit_model3),"gamma")+theme_bw()
ggs_running(ggs(fit_model3),"sigma")+theme_bw()

ggs_autocorrelation(ggs(fit_model3),"alpha")+theme_bw()
ggs_autocorrelation(ggs(fit_model3),"beta")+theme_bw()
ggs_autocorrelation(ggs(fit_model3),"gamma")+theme_bw()
ggs_autocorrelation(ggs(fit_model3),"sigma")+theme_bw()

ggs_Rhat(ggs(fit_model3),"alpha")+theme_bw()
ggs_Rhat(ggs(fit_model3),"beta")+theme_bw()
ggs_Rhat(ggs(fit_model3),"gamma")+theme_bw()
ggs_Rhat(ggs(fit_model3),"sigma")+theme_bw()

ggs_crosscorrelation(ggs(fit_model3))+theme_bw()

ggs_geweke(ggs(fit_model3))+theme_bw()


data_list <-list(
  N = nrow (dt),
  x = dt$x,
  y = dt$y
)

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
fit_pred <- stan(file = 'model3_w_pred.stan', data = stan_data_pred,
                 iter = 20000,warmup = 15000, chains = 4, seed = 123)

print(fit_pred)


# Extract predictions
predictions <-ggs(fit_pred)

predictions<-predictions%>%group_by(Parameter)%>%
  summarise(lwr=quantile(value,0.025),
            upr=quantile(value,0.975),
            est=mean(value))

predictions<-predictions[69:132,]


# Create a data frame for plotting predictions and intervals
plot_data <- data.frame(
  Age = xx,                            # Our sequence of age values
  Mean = predictions$est,                  # Mean predictions
  Lower = predictions$lwr,       # Lower 2.5% interval
  Upper = predictions$upr        # Upper 97.5% interval
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

