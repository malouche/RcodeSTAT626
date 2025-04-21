p<-c(.05,.15,.25,.35,.45,.55,.65,.75,.85,.95)

prior<-c(1,5.2,8,7.2,4.6,2.1,0.7,0.1,0,0)
prior<-prior/sum(prior)

likelihood<-function(p){
  return(p^11*(1-p)^16)
}

curve(likelihood,from=0,to=1,col="blue",lwd=2)

optimize(likelihood,interval=c(0,1),maximum=TRUE) # Maximum likelihood estimate

posterior<-likelihood(p)*prior

posterior<-posterior/sum(posterior)
posterior
names(posterior)<-p
posterior
which.max(posterior)
plot(p,posterior,type="l",col="red",lwd=2)
abline(v=p[which.max(posterior)],col="green",lwd=2)


## calculate the posterior distribution with LearnBayes package

library(LearnBayes)
posterior<-pdisc(p,prior,c(11,16))

posterior

cbind(p,prior,posterior)

### Activity 1

p<-c(.1,.2,.3,.4,.5)

prior<-c(2,4,6,4,2)

prior<-prior/sum(prior)

prior

posterior<-pdisc(p,prior,c(15,5))

cbind(p,prior,round(posterior,4))


