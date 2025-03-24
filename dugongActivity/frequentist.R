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

# Convert list to data frame
dt <- data.frame(x = dataList$x, y = dataList$Y)

# Create log-transformed age variable (lx)
dt <- dt %>% mutate(lx = log(x))

# Verify data structure
head(dt)

# Scatter plot with regression line for log-age vs weight
ggscatter(data = dt, x = 'lx', y = 'y', add = 'reg.line', 
          xlab = 'log-age', ylab = 'Weight')

# Model 1: Linear model using log-age (existing model)
freq_model <- lm(y ~ lx, data = dt)
summary(freq_model)
set.seed(123)  # For reproducibility
loo_loglin <- train(y ~ lx, method = "lm", data = dt, 
                    trControl = trainControl(method = "LOOCV"))
print(loo_loglin)

# Model 2: Simple linear model using untransformed age (x)
simple_model <- lm(y ~ x, data = dt)
summary(simple_model)
set.seed(123)
loo_simple <- train(y ~ x, method = "lm", data = dt, 
                    trControl = trainControl(method = "LOOCV"))
print(loo_simple)

# Model 3: Smooth model using loess on log-age (lx)
smooth_model <- loess(y ~ lx, data = dt)
# Note: Caret's "loess" method fits a local regression model.
set.seed(123)
loo_smooth <- train(y ~ lx, method = "gamLoess", data = dt, 
                    trControl = trainControl(method = "LOOCV"))
print(loo_smooth)


results<-rbind(loo_loglin$results[,2:4],
                loo_simple$results[,2:4],
                loo_smooth$results[,3:5])
rownames(results)=c("LogLin","Simple","Smooth")
results
