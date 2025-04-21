## -------- 0.  Define data ------------------------------------------------
t <- matrix(c(
  12,  1, 21, 25, 11, 26, 27, 30, 13, 12,
  21, 20, 23, 25, 23, 29, 35, NA, 31, 36,
  32, 27, 23, 12, 18, NA, NA, 38, 29, 30,
  NA, 32, NA, NA, NA, NA, 25, 30, 37, 27,
  22, 26, NA, 28, 19, 15, 12, 35, 35, 10,
  22, 18, NA, 12, NA, NA, 31, 24, 37, 29,
  27, 18, 22, 13, 18, 29, 28, NA, 16, 22,
  26, 19, NA, NA, 17, 28, 26, 12, 17, 26),
  nrow = 4, byrow = TRUE)
t
t.cen <- matrix(c(
  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,
  0,  0,  0,  0,  0,  0,  0, 40,  0,  0,
  0,  0,  0,  0,  0, 40, 40,  0,  0,  0,
  40,  0, 40, 40, 40, 40,  0,  0,  0,  0,
  0,  0, 10,  0,  0,  0,  0,  0,  0,  0,
  0,  0, 24,  0, 40, 40,  0,  0,  0,  0,
  0,  0,  0,  0,  0,  0,  0, 20,  0,  0,
  0,  0, 29, 10,  0,  0,  0,  0,  0,  0),
  nrow = 4, byrow = TRUE)
t.cen
M <- 4 ;  N <- 20        # group count and mice per group

library(rstan)
library(tidyverse)
library(ggmcmc)

# original 4 x 20 matrices t and t.cen already in workspace
df <- as.data.frame(as.table(t)) |>
  rename(group = Var1, idx = Var2, time = Freq) |>
  mutate(cen   = as.vector(t.cen),
         event = ifelse(is.na(time), 0, 1),
         time  = ifelse(is.na(time), cen, time)) |>
  select(group, time, event) 


stan_data <- list(
  N     = nrow(df),
  M     = length(unique(df$group)),
  time  = df$time,
  event = df$event,
  group = as.numeric(df$group))


library(rstan)
library(ggmcmc)

## -------- 1.  Fit Stan model ------------------------------------
options(mc.cores = parallel::detectCores(), auto_write = TRUE)
fit <- stan(file = "mice_weibull.stan",
            data = stan_data,
            chains = 4, iter = 5000, warmup = 1000, seed = 123)

## -------- 2.  Convergence diagnostics ---------------------------
fit
ggs_obj <- ggs(fit)
ggs_traceplot(ggs_obj, family = c("beta"))
ggs_density(ggs_obj,   family = c("beta"))
ggs_Rhat(ggs_obj);  
ggs_effective(ggs_obj)

## -------- 3.  Posterior summaries -------------------------------
print(fit, pars = c("beta","median","contrast","shape"))
\begin{verbatim}
