# Example from http://www.r-bloggers.com/likelihood-free-inference-in-high-dimensional-models/

N=10
data=rnorm(N,mean=3,sd=.5)

#ABC with insufficient statistics
medata=median(data)
madata=mad(data)

varamad=rep(0,100)
for (i in 1:100)
  varamad[i]=mad(sample(data,N,rep=TRUE))
tol=c(.01*mad(data),.05*mad(varamad))

T=1e6
mu=rep(median(data),T)
sig=rep(mad(data),T)
for (t in 2:T){
  mu[t]=rnorm(1)
  psudo=rnorm(N,mean=mu[t],sd=sig[t-1])
  if (abs(medata-median(psudo))>tol[1])
    mu[t]=mu[t-1]
  
  sig[t]=1/rexp(1)
  psudo=rnorm(N,mean=mu[t],sd=sig[t])
  if (abs(madata-mad(psudo))>tol[2])
    sig[t]=sig[t-1]
}
#ABC with more sufficient statistics
meaata=mean(data)
sddata=sd(data)

varamad=rep(0,100)
for (i in 1:100)
  varamad[i]=sd(sample(data,N,rep=TRUE))
tol=c(.1*sd(data),.1*sd(varamad))

for (t in 2:T){
  mu[t]=rnorm(1)
  psudo=rnorm(N,mean=mu[t],sd=sig[t-1])
  if (abs(meaata-mean(psudo))>tol[1])
    mu[t]=mu[t-1]
  
  sig[t]=1/rexp(1)
  psudo=rnorm(N,mean=mu[t],sd=sig[t])
  if (abs(sddata-sd(psudo))>tol[2])
    sig[t]=sig[t-1]
}

#MCMC with false sufficient
sig=1/sqrt(rgamma(T,shape=.5*N,rate=1+.5*var(data)))
mu=rnorm(T,mean(data)/(1+sig^2/N),sd=1/sqrt(1+N/sig^2))