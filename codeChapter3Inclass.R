y<-rnorm(10,5,3)

library(rstan)

stan_data <- list(N = length(y), y = y)
fit <- stan(file = "nonIdentif.stan", data = stan_data, iter = 2000,warmup = 1000,
            chains = 4,control=list(stepsize=0.01))
print(fit)


theta1<-c(fit@sim$samples[[1]][[1]],
           fit@sim$samples[[2]][[1]],
           fit@sim$samples[[3]][[1]],
           fit@sim$samples[[4]][[1]])
theta1
length(theta1)

theta2<-c(fit@sim$samples[[1]][[2]],
          fit@sim$samples[[2]][[2]],
          fit@sim$samples[[3]][[2]],
          fit@sim$samples[[4]][[2]])

cor(theta1,theta2)

cor.test(theta1,theta2)

sigma<-c(fit@sim$samples[[1]][[3]],
  fit@sim$samples[[2]][[3]],
  fit@sim$samples[[3]][[3]],
  fit@sim$samples[[4]][[3]])

cor(sigma,theta1)


fit_mix <- stan(file = "mixmodel.stan", data = stan_data, iter = 2000,warmup = 1000,
            chains = 4,control=list(stepsize=0.01))
print(fit_mix)

y1<-rnorm(20,1,.5)

y2<-rnorm(20,10,.5)

z<-rbinom(20,1,.3)

y<-z*y1+(1-z)*y2
y

stan_data <- list(N = length(y), y = y)

fit_mix <- stan(file = "mixmodel.stan", data = stan_data, iter = 2000,warmup = 1000,
                chains = 4,control=list(stepsize=0.01))
print(fit_mix)

mu2<-c(fit@sim$samples[[1]][[2]],
         fit@sim$samples[[2]][[2]],
         fit@sim$samples[[3]][[2]],
         fit@sim$samples[[4]][[2]])
mu1<-c(fit@sim$samples[[1]][[1]],
         fit@sim$samples[[2]][[1]],
         fit@sim$samples[[3]][[1]],
         fit@sim$samples[[4]][[1]])

cor.test(mu1,mu2)
library(bayesplot)
color_scheme_set("viridis")
mcmc_trace(fit_mix, pars = c("mu2"))

library(ggmcmc)
library(ggplot2)
ggs_traceplot(ggs(fit_mix),family = 'mu2')++theme_bw()

# how to add an horizontal line with ggplot2
ggs_traceplot(ggs(fit_mix),family = 'mu2')+geom_hline(yintercept = 1.56, linetype = "dashed", color = "red")+theme_bw()


mcmc_dens_overlay(fit_mix, pars = c("mu1"))

y_ber=rbinom(300,1,.5)

stan_data_bern <- list(N = length(y_ber), y = y_ber)

fit_ber <- stan(file = "bernoulli_model.stan", data = stan_data_bern, iter = 20000,warmup = 18000,
                chains = 4,control=list(stepsize=0.001,adapt_delta=0.9, max_treedepth = 15))
print(fit_ber)

ggs_traceplot(ggs(fit_ber),family = 'p')+geom_hline(yintercept = 0.44, linetype = "dashed", color = "red")+theme_bw()

mcmc_dens_ov
erlay(fit_ber, pars = c("p"))

ggs_density(ggs(fit_ber),family = 'p')+geom_vline(xintercept = 0.17, linetype = "dashed", color = "red")+theme_bw()


mcmc_acf(fit_ber, lags = 10,pars = 'p')



mcmc_acf(fit_mix, lags = 10,pars = 'mu1')


ggs_Rhat(ggs(fit_ber),family = 'p')+theme_bw()

ggs_Rhat(ggs(fit_mix),family = 'mu2')+theme_bw()


p<-c(fit_ber@sim$samples[[1]][[1]],
     fit_ber@sim$samples[[2]][[1]],
     fit_ber@sim$samples[[3]][[1]],
     fit_ber@sim$samples[[4]][[1]])
mcmc_neff(p)

print(fit_ber)

ggs_effective(ggs(fit_ber),family = 'p')+theme_bw()

ggs_effective(ggs(fit_mix),family = 'mu2')+theme_bw()

ggs_effective(ggs(fit_mix),family = 'mu1')+theme_bw()


ggs_geweke(ggs(fit_mix))+theme_bw()

ggs_geweke(ggs(fit_ber))+theme_bw()


ggs_crosscorrelation (ggs(fit_mix) ) + theme_bw ()

ggs_running(ggs(fit_ber),family = 'p')+theme_bw()
