library(rstan)
library(tidyverse)
library(ggmcmc)


titanic<-read.csv("titanic.csv")

titanic<-titanic%>%select(Survived,Sex,Age)%>%na.omit()

titanic<-titanic%>%mutate(Sex=ifelse(Sex=='male',1,0))

data<-list(
  N=nrow(titanic),
  Survived=titanic$Survived,
  Sex=titanic$Sex,
  Age=titanic$Age
)

fit_logitic <- stan(file = "logistic_titanic.stan", data = data, iter = 20000, 
                    warmup = 15000,chains = 4,
                    control = list(adapt_delta = 0.95, max_treedepth = 15))

summary(fit_logitic)

fit_ggs<-ggs(fit_logitic)

ggs_traceplot(fit_ggs, family = "b0")
ggs_traceplot(fit_ggs, family = "b1")
ggs_traceplot(fit_ggs, family = "b2")

ggs_Rhat(fit_ggs, family = "b0")
ggs_Rhat(fit_ggs, family = "b1")
ggs_Rhat(fit_ggs, family = "b2")

ggs_autocorrelation(fit_ggs, family = "b0",nLags = 30)
ggs_autocorrelation(fit_ggs, family = "b1",nLags = 30)
ggs_autocorrelation(fit_ggs, family = "b2",nLags = 30)

ggs_running(fit_ggs, family = "b0")
ggs_running(fit_ggs, family = "b1")
ggs_running(fit_ggs, family = "b2")

ggs_geweke(fit_ggs, family = "b0")
ggs_geweke(fit_ggs, family = "b1")
ggs_geweke(fit_ggs, family = "b2")

ggs_crosscorrelation(fit_ggs%>%filter(Parameter%in%c("b0","b1","b2")))

ggs_density(fit_ggs, family = "b0")+theme_bw()
ggs_density(fit_ggs, family = "b1")+
  geom_vline(xintercept = 0,linetype="dashed",color="red")+theme_bw()

ggs_density(fit_ggs, family = "b2")+
  geom_vline(xintercept = 0,linetype="dashed",color="red")+theme_bw()


fit_prob<-fit_ggs%>%filter(Parameter%in%grep("prob",Parameter,value=T))

fit_prob<-fit_prob%>%group_by(Parameter)%>%
  summarise(mean=mean(value),sd=sd(value),lower=quantile(value,0.025),upper=quantile(value,0.975))%>%
  mutate(Parameter=gsub("prob_survive","obs",Parameter))

fit_prob<-fit_prob%>%mutate(Survived=data$Survived)

library(pROC)
roc_obj <- roc(fit_prob$Survived, fit_prob$mean)

plot(roc_obj, col = "blue", lwd = 2, main = "ROC Curve")
auc(roc_obj)

roc_obj$thresholds
roc_obj$sensitivities
roc_obj$specificities


###
data_noAge<-list(
  N=nrow(titanic),
  Survived=titanic$Survived,
  Sex=titanic$Sex
)

fit_logitic_noAge <- stan(file = "logistic_titanic_noAge.stan", data = data_noAge, iter = 20000, 
                    warmup = 15000,chains = 4,
                    control = list(adapt_delta = 0.95, max_treedepth = 15))
print(fit_logitic_noAge)
fit_ggs_noAge<-ggs(fit_logitic_noAge)
fit_prob_noAge<-fit_ggs_noAge%>%filter(Parameter%in%grep("prob",Parameter,value=T))

fit_prob_noAge<-fit_prob_noAge%>%group_by(Parameter)%>%
  summarise(mean=mean(value),sd=sd(value),lower=quantile(value,0.025),upper=quantile(value,0.975))%>%
  mutate(Parameter=gsub("prob_survive","obs",Parameter))

fit_prob_noAge<-fit_prob_noAge%>%mutate(Survived=data$Survived)
library(pROC)
roc_obj_noAge <- roc(fit_prob_noAge$Survived, fit_prob_noAge$mean)
plot(roc_obj_noAge, col = "blue", lwd = 2, main = "ROC Curve")
auc(roc_obj_noAge)
