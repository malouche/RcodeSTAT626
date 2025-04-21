library(datasets)
data("Orange")
Orange

m<-lm(circumference ~ age, data = Orange)

summary(m)

data_orange<-list(
  N= nrow(Orange),
  x = Orange$age,
  y = Orange$circumference
)

fit_model1 <- stan(file = "normal_orange.stan", data = data_orange, iter = 20000, 
                     warmup = 15000,chains = 4,
                     control = list(adapt_delta = 0.95, max_treedepth = 15))

fit_model2 <- stan(file = "normal_orange2.stan", data = data_orange, iter = 20000, 
                   warmup = 15000,chains = 4,
                   control = list(adapt_delta = 0.95, max_treedepth = 15))



print(fit_model1)
print(fit_model2)

fit_ggs1<-ggs(fit_model1)
fit_ggs2<-ggs(fit_model2)


ggs_traceplot(fit_ggs1%>%filter(Parameter=='alpha'))+theme_bw()
ggs_traceplot(fit_ggs1%>%filter(Parameter=='beta'))+theme_bw()
ggs_traceplot(fit_ggs1%>%filter(Parameter=='sigma'))+theme_bw()

ggs_autocorrelation(fit_ggs1%>%filter(Parameter=='alpha'),nLags = 20)+theme_bw()
ggs_autocorrelation(fit_ggs1%>%filter(Parameter=='beta'),nLags = 20)+theme_bw()
ggs_autocorrelation(fit_ggs1%>%filter(Parameter=='sigma'),nLags = 20)+theme_bw()

ggs_Rhat(fit_ggs1%>%filter(Parameter=='alpha'))+theme_bw()
ggs_Rhat(fit_ggs1%>%filter(Parameter=='beta'))+theme_bw()
ggs_Rhat(fit_ggs1%>%filter(Parameter=='sigma'))+theme_bw()

ggs_effective(fit_ggs1%>%filter(Parameter=='alpha'))+theme_bw()
ggs_effective(fit_ggs1%>%filter(Parameter=='beta'))+theme_bw()
ggs_effective(fit_ggs1%>%filter(Parameter=='sigma'))+theme_bw()

ggs_geweke(fit_ggs1%>%filter(Parameter=='alpha'))+theme_bw()
ggs_geweke(fit_ggs1%>%filter(Parameter=='beta'))+theme_bw()
ggs_geweke(fit_ggs1%>%filter(Parameter=='sigma'))+theme_bw()


fit_ggs1<-fit_ggs1%>%filter(Parameter%in%grep(pattern = 'y_',fit_ggs1$Parameter,value=T))

dt1<-fit_ggs1%>%group_by(Parameter)%>%summarise(mean=mean(value))

fit_ggs2<-fit_ggs2%>%filter(Parameter%in%grep(pattern = 'y_',fit_ggs2$Parameter,value=T))

dt2<-fit_ggs2%>%group_by(Parameter)%>%summarise(mean=mean(value))

y_pred_frq<-predict(m,data=Orange)

all_predictions<-data.frame(frq=y_pred_frq,bayes_m1=dt1$mean,bayes_m2=dt2$mean,y=Orange$circumference)

rmse<-function(x,y){
  sqrt(mean((x-y)^2))
}

b_m1<-rmse(all_predictions$y,all_predictions$bayes_m1)
b_m2<-rmse(all_predictions$y,all_predictions$bayes_m2)
frq<-rmse(all_predictions$y,all_predictions$frq)
b_m1
b_m2
frq
