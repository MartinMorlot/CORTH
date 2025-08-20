library(dplyr);library(tidyr);library(lubridate)
library(ggplot2);library(patchwork)

# Read and plot raw data
X=read.csv('combined_Data.csv')
X=X %>% mutate(date=as.Date(DateTime),year=as.numeric(substr(date,1,4)),
               month=as.numeric(substr(date,6,7)),day=as.numeric(substr(date,9,10)),
               calendar=as.Date(paste0('2004-',month,'-',day)))
ggplot(X)+geom_point(aes(x=calendar,y=deltaH,color=Station))+
  scale_color_discrete(guide=NULL)+
  facet_wrap(.~Station,scales='free_y')+
  theme_bw()
# Remove a few stations that look dubious, 
# and define 'positived' deltas
bin=c('A634101001','K649251001','A242020001')
X=X %>% filter(!(Station %in% bin)) %>% 
  mutate(delta=-1*deltaH,iStation=as.integer(factor(Station)))
# Look at data by year 
ggplot(X)+geom_point(aes(x=calendar,y=deltaH,color=Station))+
  scale_color_discrete(guide=NULL)+
  facet_wrap(.~year)+
  theme_bw()

# ANNUAL INDICES -------------------------------------------
# Compute annual indices for each station
Y=X %>% group_by(year,Station) %>% 
  summarise(maxi=max(delta), # max delta
            average=mean(delta*(delta>0)), # average positive deltas only
            start=yday(min(date[which(delta==0)])), # day of first zero
            end=yday(max(date[which(delta==0)])) # day of last zero
            )
# Compute pairwise correlation if enough years in common
stations=unique(Y$Station)
nS=length(stations)
Cs=list() # correlation matrix for each annual index
nMin=10 # minimum number of common years before computing a correlation
for(ind in names(Y)[3:NCOL(Y)]){
  Cs[[ind]]=matrix(1,nS,nS)
  for(i in 2:nS){
    message(paste0(ind,'-',i))
    d1=Y %>% filter(Station==stations[i]) %>% select(year,val1=all_of(ind))
    for(j in 1:(i-1)){
      d2=Y %>% filter(Station==stations[j]) %>% select(year,val2=all_of(ind))
      DF=inner_join(d1,d2,by='year')
      if(NROW(DF) >= nMin){Cs[[ind]][i,j]=cor(DF$val1,DF$val2)} else {Cs[[ind]][i,j]=NA}
      Cs[[ind]][j,i]=Cs[[ind]][i,j]
    }
  }
}
# Visualize correlation matrices
pdf(file='correlations.pdf',width=24,height=24)
par(mfrow=c(2,2))
image(Cs[['maxi']],main='maxi',col=hcl.colors(20,palette='RdYlBu'),breaks=seq(-1,1,length.out=21))
image(Cs[['average']],main='average',col=hcl.colors(20,palette='RdYlBu'),breaks=seq(-1,1,length.out=21))
image(Cs[['start']],main='start',col=hcl.colors(20,palette='RdYlBu'),breaks=seq(-1,1,length.out=21))
image(Cs[['end']],main='end',col=hcl.colors(20,palette='RdYlBu'),breaks=seq(-1,1,length.out=21))
dev.off()

# FIT CURVE -------------------------------------------
corTH_model <- function(theta,X){
  # The fitted curve is a beta density, https://en.wikipedia.org/wiki/Beta_distribution
  # Parameters of the curve are its mean (theta[2]), its concentration (theta[3]), 
  # and a multiplicative coefficient (theta[1])
    if(any(theta<0)){
    pred=rep(NA,NROW(X))
  } else{
    shape1=theta[2]*theta[3]
    shape2=(1-theta[2])*theta[3]
    tau=dbeta((X$month-0.5)/12,shape1=shape1,shape2=shape2) # beta density
    pred=theta[1]*tau
  }
  out=cbind(X,pred=pred)
  return(out)
}

logLkh <- function(pars,X){
  # log-likelihood function
  sigma=pars[length(pars)] # residual standard deviation
  if(sigma<=0){return(-Inf)}
  mod=corTH_model(pars[1:(length(pars)-1)],X)
  if(any(is.na(mod$pred))){return(-Inf)}
  # compute log-likelihood to be maximized
  out=sum(dnorm(mod$delta,mean=mod$pred,sd=sigma,log=TRUE))
  return(out)
}

nS=length(unique(X$Station))
gs=vector('list',nS) # plots
conv=rep(NA,nS) # convergence of the optimizer ?
pars=matrix(NA,nS,4) # Optimized parameters
for(i in 1:nS){
  message(paste0(i,'/',nS))
  DF=X %>% filter(iStation==i)
  # DF=X %>% filter(iStation==6,year==1974) # to play around with single-year fits
  start=c(mean(DF$delta),0.6,4,0.1)
  foo=optim(par=start,fn=logLkh,X=DF,control=list(maxit=10000,fnscale=-1))
  conv[i]=foo$convergence
  pars[i,]=foo$par
  mod=corTH_model(foo$par,DF)
  mod2=corTH_model(foo$par,
                   data.frame(month=1:12,Station=DF$Station[1],
                              calendar=as.Date(paste0('2004-',1:12,'-15'))))
  gs[[i]]=ggplot(mod)+
    geom_line(aes(calendar,delta,group=year),alpha=0.5)+
    geom_point(aes(calendar,delta,group=year),alpha=0.2)+
    geom_line(data=mod2,aes(calendar,pred),linewidth=2,color='red')+
    scale_color_discrete(guide=NULL)+
    facet_wrap(.~Station,scales='free_y')+
    xlim(as.Date('2004-01-01'),as.Date('2004-12-31'))+
    theme_bw()
}
pdf(file='seasonnality.pdf',width=24,height=36)
wrap_plots(grobs=gs,ncol=8)
dev.off()
