
#Importing the regionalization functionality
rm(list = ls())
source('D:/Proyectos_GitHub/Bartlet-Lewis_Regionalization/src/RMBLRP.R')

opt='TRMM'

if (opt=='TRMM'){
  #Mixed stats from gauge stations and corrected TRMM
  gauge_stats=read.csv('D:/Proyectos_GitHub/Bartlet-Lewis_Regionalization/data/feb_gauge_stat.csv')
  gauge_stats=kickOutliers(gauge_stats)
  gauge_stats=filter_Neigbors(gauge_stats)
  c(nrow,ncol) := dim(gauge_stats)
  #Lmin=matrix(c(0.1,0.001,0.001,0.001,0.0854,1),nrow = 6,ncol = nrow)
  #Lmax=matrix(c(4,0.1,0.1,0.1,0.1,20),nrow=6,ncol=nrow)
  maps=run(rain_stats=gauge_stats,path="D:/Proyectos_GitHub/Bartlet-Lewis_Regionalization/output/CV_parameters/",iterations=5,FILE_NAME='parameters_ago.csv')
}else if(opt =='GPM'){
  #Mixed stats from gauge stations and corrected TRMM
  gauge_stats=read.csv('D:/Proyectos_GitHub/Bartlet-Lewis_Regionalization
                       /data/gauge_stats.csv')
  gauge_stats=kickOutliers(gauge_stats)
  gauge_stats=filter_Neigbors(gauge_stats)
  dim(gauge_stats)

  maps=run(rain_stats=gauge_stats,path="D:/Proyectos_GitHub/Bartlet-Lewis_Regionalization
           /output/CV_parameters/",iterations=20)
}
