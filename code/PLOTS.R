#Importing the regionalization functionality
source('D:/Proyectos_GitHub/Bartlet-Lewis_Regionalization/code/RMBLRP.R')
library(HyetosMinute)
####################################
###Model Statistics#################
####################################
#Mean
meanMBLRPM<-function(a,l,v,k,f,mx,h=1) {
  x<-(h*l*mx*v*(1+k/f))/(a-1) 
  return(x)
}
#Variance
varMBLRPM<-function(a,l,v,k,f,mx,h=1) {
  A<-(2*l*(1+k/f)*(mx^2)*(v^a))/((f^2)*((f^2)-1)*(a-1)*(a-2)*(a-3))
  B<-(2*(f^2)-2+k*f)*(f^2)*((a-3)*h*(v^(2-a))-(v^(3-a))+((v+h)^(3-a)))
  C<-k*(f*(a-3)*h*(v^(2-a))-(v^(3-a))+((v+f*h)^(3-a)))
  D<-A*(B-C)
  return(D)
}
#Covariance
covarMBLRPM<-function(a,l,v,k,f,mx,h=1,lag=1) {
  A<-(l*(1+k/f)*(mx^2)*(v^a))/((f^2)*((f^2)-1)*(a-1)*(a-2)*(a-3))
  B<-(2*(f^2)-2+k*f)*(f^2)*(((v+(lag+1)*h)^(3-a))-2*((v+lag*h)^(3-a))+((v+(lag-1)*h)^(3-a)))
  C<-k*(((v+(lag+1)*h*f)^(3-a))-(2*((v+h*lag*f)^(3-a)))+((v+(lag-1)*h*f)^(3-a))) 
  D<-A*(B-C)
  return(D)
}
#Dry probabilities
pdrMBLRPM<-function(a,l,v,k,f,h=1) {
  mt<-((1+(f*(k+f))-(0.25*f*(k+f)*(k+4*f))+((f/72)*(k+f)*(4*(k^2)+27*k*f+72*(f^2))))*v)/(f*(a-1))
  G00<-((1-k-f+1.5*k*f+(f^2)+0.5*(k^2))*v)/(f*(a-1))
  A<-(f+(k*(v/(v+(k+f)*h))^(a-1)))/(f+k)
  D<-exp(l*(-h-mt+G00*A)) 
  return(D)
}


SimStats= function(parameters){
  stats=matrix(data=NA,nrow = dim(parameters)[1],ncol = 16)
  for (i in 1:dim(parameters)[1]){
    par=parameters[i,]
    par[2]<-par[2]*24
    par[3]<-par[3]/24
    par[6]=par[6]*24
    
    est=numeric(16)
    iter=0
    for (j in c(24,3,6,12)){
      m=meanMBLRPM(par[1],par[2],par[3],par[4],par[5],par[6],h=j/24)
      v=varMBLRPM(par[1],par[2],par[3],par[4],par[5],par[6],h=j/24)
      cov=covarMBLRPM(par[1],par[2],par[3],par[4],par[5],par[6],h=j/24)
      pdr=pdrMBLRPM(par[1],par[2],par[3],par[4],par[5],h=j/24)
      est[(1+iter*4):(4+iter*4)]=c(m,v,cov,pdr)
      iter=iter+1
    }
    
    stats[i,]=as.numeric(est)
  }
  stats
}



#Gauges statistics
gauge_stats=read.csv('D:/Proyectos_GitHub/Bartlet-Lewis_Regionalization/data/gauge_stats.csv')
gauge_stats=kickOutliers(gauge_stats)
gauge_stats=filter_Neigbors(gauge_stats)

#Model CV parameters
validation_parameters=read.csv('D:/Proyectos_GitHub/Bartlet-Lewis_Regionalization/output/CV_validation/CrossValidationParameters.csv')
#Simulated Stats
simulated_stats=data.frame(gauge_stats[,c(1,2)],SimStats(validation_parameters))

dev.new()
par(mfrow=c(4,4)) 

labelsy=c(rep('CV Simulated 24-Hourly Acc Level',4),
          rep('CV Simulated 3-Hourly Acc Level',4),
          rep('CV Simulated 6-Hourly Acc Level',4),
          rep('CV Simulated 12-Hourly Acc Level',4))

titulos=c('Mean Rainfall (mm)','STDE Rainfall (mm)','Lag-1 Autocorrelation','Probability of 0 Rain')



for (i in 1:16){
  if (i %in% 1:4){
    if( i==1){
      plot(gauge_stats[[i+2]],simulated_stats[[i+2]],xlab="Observed",ylab='Simulated 24-Hourly Acc Level',main=titulos[i])
    }else{
      y=simulated_stats[[i+2]]
      plot(x=gauge_stats[[i+2]],y,xlab="Observed",ylab=NULL,main=titulos[i])
    }
  }else{
    if (i %in% c(5,9,13)){
      plot(x=gauge_stats[[i+2]],y=simulated_stats[[i+2]],xlab="TRMM bias corrected",ylab=labelsy[i])
    }else{
      y=simulated_stats[[i+2]]
      plot(x=gauge_stats[[i+2]],y,xlab="TRMM bias corrected",ylab=NULL)
    }
    
  }
  abline(coef = c(0,1))
}




