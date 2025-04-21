data_hospital<-list(
  N=12,
  n=c(47,148,199,810,211,196,148,215,207,97,256,360),
  r=c(0,18,8,46,8,13,9,31,14,8,29,24)
)

library(rstan)

fit_hospital <- stan(file = "hospital_model.stan", data = data_hospital, iter = 20000, 
                    warmup = 15000,chains = 4,
                    control = list(adapt_delta = 0.95, max_treedepth = 15))

print(fit_hospital)

library(ggmcmc)

fit_ggs<-ggs(fit_hospital)

library(dplyr)

ggs_traceplot(fit_ggs%>%filter(Parameter=='p[1]'))+theme_bw()
ggs_traceplot(fit_ggs%>%filter(Parameter=='p[2]'))+theme_bw()
ggs_traceplot(fit_ggs%>%filter(Parameter=='p[3]'))+theme_bw()
ggs_traceplot(fit_ggs%>%filter(Parameter=='p[4]'))+theme_bw()
ggs_traceplot(fit_ggs%>%filter(Parameter=='p[5]'))+theme_bw()
ggs_traceplot(fit_ggs%>%filter(Parameter=='p[6]'))+theme_bw()
ggs_traceplot(fit_ggs%>%filter(Parameter=='p[7]'))+theme_bw()
ggs_traceplot(fit_ggs%>%filter(Parameter=='p[8]'))+theme_bw()
ggs_traceplot(fit_ggs%>%filter(Parameter=='p[9]'))+theme_bw()
ggs_traceplot(fit_ggs%>%filter(Parameter=='p[10]'))+theme_bw()
ggs_traceplot(fit_ggs%>%filter(Parameter=='p[11]'))+theme_bw()
ggs_traceplot(fit_ggs%>%filter(Parameter=='p[12]'))+theme_bw()

ggs_autocorrelation(fit_ggs%>%filter(Parameter=='p[1]'),nLags = 20)+theme_bw()
ggs_autocorrelation(fit_ggs%>%filter(Parameter=='p[2]'),nLags = 20)+theme_bw()
ggs_autocorrelation(fit_ggs%>%filter(Parameter=='p[3]'),nLags = 20)+theme_bw()
ggs_autocorrelation(fit_ggs%>%filter(Parameter=='p[4]'),nLags = 20)+theme_bw()
ggs_autocorrelation(fit_ggs%>%filter(Parameter=='p[5]'),nLags = 20)+theme_bw()
ggs_autocorrelation(fit_ggs%>%filter(Parameter=='p[6]'),nLags = 20)+theme_bw()
ggs_autocorrelation(fit_ggs%>%filter(Parameter=='p[7]'),nLags = 20)+theme_bw()
ggs_autocorrelation(fit_ggs%>%filter(Parameter=='p[8]'),nLags = 20)+theme_bw()
ggs_autocorrelation(fit_ggs%>%filter(Parameter=='p[9]'),nLags = 20)+theme_bw()
ggs_autocorrelation(fit_ggs%>%filter(Parameter=='p[10]'),nLags = 20)+theme_bw()
ggs_autocorrelation(fit_ggs%>%filter(Parameter=='p[11]'),nLags = 20)+theme_bw()
ggs_autocorrelation(fit_ggs%>%filter(Parameter=='p[12]'),nLags = 20)+theme_bw()

ggs_Rhat(fit_ggs%>%filter(Parameter=='p[1]'))+theme_bw()
ggs_Rhat(fit_ggs%>%filter(Parameter=='p[2]'))+theme_bw()
ggs_Rhat(fit_ggs%>%filter(Parameter=='p[3]'))+theme_bw()
ggs_Rhat(fit_ggs%>%filter(Parameter=='p[4]'))+theme_bw()
ggs_Rhat(fit_ggs%>%filter(Parameter=='p[5]'))+theme_bw()
ggs_Rhat(fit_ggs%>%filter(Parameter=='p[6]'))+theme_bw()
ggs_Rhat(fit_ggs%>%filter(Parameter=='p[7]'))+theme_bw()
ggs_Rhat(fit_ggs%>%filter(Parameter=='p[8]'))+theme_bw()
ggs_Rhat(fit_ggs%>%filter(Parameter=='p[9]'))+theme_bw()
ggs_Rhat(fit_ggs%>%filter(Parameter=='p[10]'))+theme_bw()
ggs_Rhat(fit_ggs%>%filter(Parameter=='p[11]'))+theme_bw()
ggs_Rhat(fit_ggs%>%filter(Parameter=='p[12]'))+theme_bw()

ggs_effective(fit_ggs%>%filter(Parameter=='p[1]'))+theme_bw()
ggs_effective(fit_ggs%>%filter(Parameter=='p[2]'))+theme_bw()
ggs_effective(fit_ggs%>%filter(Parameter=='p[3]'))+theme_bw()
ggs_effective(fit_ggs%>%filter(Parameter=='p[4]'))+theme_bw()
ggs_effective(fit_ggs%>%filter(Parameter=='p[5]'))+theme_bw()
ggs_effective(fit_ggs%>%filter(Parameter=='p[6]'))+theme_bw()
ggs_effective(fit_ggs%>%filter(Parameter=='p[7]'))+theme_bw()
ggs_effective(fit_ggs%>%filter(Parameter=='p[8]'))+theme_bw()
ggs_effective(fit_ggs%>%filter(Parameter=='p[9]'))+theme_bw()
ggs_effective(fit_ggs%>%filter(Parameter=='p[10]'))+theme_bw()
ggs_effective(fit_ggs%>%filter(Parameter=='p[11]'))+theme_bw()
ggs_effective(fit_ggs%>%filter(Parameter=='p[12]'))+theme_bw()

ggs_geweke(fit_ggs%>%filter(Parameter=='p[1]'))+theme_bw()
ggs_geweke(fit_ggs%>%filter(Parameter=='p[2]'))+theme_bw()
ggs_geweke(fit_ggs%>%filter(Parameter=='p[3]'))+theme_bw()
ggs_geweke(fit_ggs%>%filter(Parameter=='p[4]'))+theme_bw()
ggs_geweke(fit_ggs%>%filter(Parameter=='p[5]'))+theme_bw()
ggs_geweke(fit_ggs%>%filter(Parameter=='p[6]'))+theme_bw()
ggs_geweke(fit_ggs%>%filter(Parameter=='p[7]'))+theme_bw()
ggs_geweke(fit_ggs%>%filter(Parameter=='p[8]'))+theme_bw()
ggs_geweke(fit_ggs%>%filter(Parameter=='p[9]'))+theme_bw()
ggs_geweke(fit_ggs%>%filter(Parameter=='p[10]'))+theme_bw()
ggs_geweke(fit_ggs%>%filter(Parameter=='p[11]'))+theme_bw()
ggs_geweke(fit_ggs%>%filter(Parameter=='p[12]'))+theme_bw()

ggs_crosscorrelation(fit_ggs)+theme_bw()

ggs_density(fit_ggs%>%filter(Parameter=='p[1]'))+theme_bw()
ggs_density(fit_ggs%>%filter(Parameter=='p[2]'))+theme_bw()            
ggs_density(fit_ggs%>%filter(Parameter=='p[3]'))+theme_bw()
ggs_density(fit_ggs%>%filter(Parameter=='p[4]'))+theme_bw()
ggs_density(fit_ggs%>%filter(Parameter=='p[5]'))+theme_bw()
ggs_density(fit_ggs%>%filter(Parameter=='p[6]'))+theme_bw()
ggs_density(fit_ggs%>%filter(Parameter=='p[7]'))+theme_bw()
ggs_density(fit_ggs%>%filter(Parameter=='p[8]'))+theme_bw()
ggs_density(fit_ggs%>%filter(Parameter=='p[9]'))+theme_bw()
ggs_density(fit_ggs%>%filter(Parameter=='p[10]'))+theme_bw()
ggs_density(fit_ggs%>%filter(Parameter=='p[11]'))+theme_bw()
ggs_density(fit_ggs%>%filter(Parameter=='p[12]'))+theme_bw()

ggs_caterpillar(fit_ggs)+theme_bw()



fit_hospital <- stan(file = "hospital_model.stan", data = data_hospital, iter = 20000, 
                     warmup = 15000,chains = 4,
                     control = list(adapt_delta = 0.95, max_treedepth = 15))

fit_ggs<-ggs(fit_hospital)
ggs_density(fit_ggs%>%filter(Parameter=='diff[1,2]'))+geom_vline(xintercept=0,col='red')+theme_bw()
ggs_density(fit_ggs%>%filter(Parameter=='diff[1,5]'))+geom_vline(xintercept=0,col='red')+theme_bw()
ggs_density(fit_ggs%>%filter(Parameter=='diff[1,3]'))+geom_vline(xintercept=0,col='red')+theme_bw()
ggs_density(fit_ggs%>%filter(Parameter=='diff[1,8]'))+geom_vline(xintercept=0,col='red')+theme_bw()
ggs_density(fit_ggs%>%filter(Parameter=='diff[6,9]'))+geom_vline(xintercept=0,col='red')+theme_bw()


fit_hospital <- stan(file = "hospital_model.stan", data = data_hospital, iter = 20000, 
                     warmup = 15000,chains = 4,
                     control = list(adapt_delta = 0.95, max_treedepth = 15))
print(fit_hospital)
fit_ggs<-ggs(fit_hospital)

dt1<-fit_ggs%>%filter(Parameter=='future_deaths[1]')%>%select(Iteration,value)

dt1<-dt1%>%group_by(value)%>%summarise(n=n())%>%mutate(p=n/sum(n))%>%arrange(value)

library(ggplot2)
ggplot(dt1,aes(x=value,y=p))+geom_bar(stat='identity')+theme_bw()+
  xlab('Future deaths')+ylab('Probability')+ggtitle('Future deaths distribution')+
  theme(plot.title = element_text(hjust = 0.5))

dt2<-fit_ggs%>%filter(Parameter=='future_deaths[2]')%>%select(Iteration,value)
dt2<-dt2%>%group_by(value)%>%summarise(n=n())%>%mutate(p=n/sum(n))%>%arrange(value)
ggplot(dt2,aes(x=value,y=p))+geom_bar(stat='identity')+theme_bw()+
  xlab('Future deaths')+ylab('Probability')+ggtitle('Future deaths distribution')+
  theme(plot.title = element_text(hjust = 0.5))

