#Importing the regionalization functionality
source('D:/Proyectos_GitHub/Bartlet-Lewis_Regionalization/code/RMBLRP.R')
#Mixed stats from gauge stations and corrected TRMM
gauge_stats=read.csv('D:/Proyectos_GitHub/Bartlet-Lewis_Regionalization/data/gauge_stats.csv')
gauge_stats=kickOutliers(gauge_stats)
gauge_stats=filter_Neigbors(gauge_stats)

n=dim(gauge_stats)[1]

#validation_parameters=matrix(data=NA,nrow=n,ncol=6)
validation_parameters=read.csv('D:/Proyectos_GitHub/Bartlet-Lewis_Regionalization/output/CV_validation/CrossValidationParameters.csv')

gauge_help=gauge_stats
coordinates(gauge_help) <- ~x+y
proj4string(gauge_help)='+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0'
mdist=distm(gauge_help)

try(for (i in 100:n){



  stats=gauge_stats[-i,]
  station=gauge_stats[i,]
  

  print(paste('Cross Validation: ',as.character(i)))
  CV_parameters=try(run(stats,path="D:/Proyectos_GitHub/Bartlet-Lewis_Regionalization/output/CV_validation/",iterations=5))
  for (j in 1:6){
      info=CV_parameters[[c('a','l','v','k','f','mx')[j]]]
      denominador=sum((1/mdist[i,-i])^2)
      validation_parameters[i,j]=sum(info/mdist[i,-i]^2/denominador)
  }
  
  
  
  write.table(validation_parameters,'D:/Proyectos_GitHub/Bartlet-Lewis_Regionalization/output/CV_validation/CrossValidationParameters.csv',sep=',',row.names = F)
})
