y<-rnorm(40,10.3,3.5)

data_stan<-list(N=40,y=y)

library(rstan)

fit<-stan(file="normal.stan",data=data_stan,iter=20000,warmup = 19000,chains=4,
          control=list(stepsize=0.001,adapt_delta=0.9, max_treedepth = 15))

print(fit)

library(ggmcmc)

ggs_traceplot(ggs(fit),'mu')+theme_minimal()

ggs_density(ggs(fit),'mu')+theme_minimal()

ggs_running(ggs(fit),'mu')+theme_minimal()

ggs_autocorrelation(ggs(fit),'mu',nLags =10)+theme_minimal()

ggs_crosscorrelation(ggs(fit))+theme_minimal()

ggs_pairs(ggs(fit))+theme_minimal()

ggs_Rhat(ggs(fit))+theme_minimal()

ggs_effective(ggs(fit))+theme_minimal()

ggs_geweke(ggs(fit))+theme_minimal()
