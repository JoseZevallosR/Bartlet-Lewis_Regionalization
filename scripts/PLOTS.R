#Importing the regionalization functionality
source('D:/Proyectos_GitHub/Bartlet-Lewis_Regionalization/src/RMBLRP.R')


#Gauges statistics
gauge_stats=read.csv('D:/Proyectos_GitHub/Bartlet-Lewis_Regionalization/data/gauge_stats.csv')
gauge_stats=kickOutliers(gauge_stats)
gauge_stats=filter_Neigbors(gauge_stats)

#Model CV parameters
#validation_parameters=read.csv('D:/Proyectos_GitHub/Bartlet-Lewis_Regionalization/output/CV_validation/CrossValidationParameters.csv')
validation_parameters=read.csv('D:/Proyectos_GitHub/Bartlet-Lewis_Regionalization/output/CV_parameters/parameters01.csv')
#Simulated Stats
simulated_stats=data.frame(gauge_stats[,c(1,2)],SimStats(validation_parameters[,-c(1,2)]))

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




