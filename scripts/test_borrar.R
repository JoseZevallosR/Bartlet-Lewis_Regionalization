#Importing the regionalization functionality
source('D:/Proyectos_GitHub/Bartlet-Lewis_Regionalization/src/RMBLRP.R')
source('D:/Proyectos_GitHub/Bartlet-Lewis_Regionalization/src/plotHelpers.R')

#Gauges statistics
gauge_stats=read.csv('D:/Proyectos_GitHub/Bartlet-Lewis_Regionalization/data/agost_gauge_stat.csv')
gauge_stats=kickOutliers(gauge_stats)
gauge_stats=filter_Neigbors(gauge_stats)
#gauge_stats=gauge_stats[-c(78,137),]


#Model CV parameters
validation_parameters=read.csv('D:/Proyectos_GitHub/Bartlet-Lewis_Regionalization/output/CV_parameters/parameters_ago.csv')#[-c(78,137),]
#validation_parameters=read.csv('D:/Proyectos_GitHub/Bartlet-Lewis_Regionalization/output/CV_parameters/parameters01.csv')
#Simulated Stats
simulated_stats=data.frame(gauge_stats[,c(1,2)],SimStats(validation_parameters[,-c(1,2)]))
names(simulated_stats)=names(gauge_stats)[1:18]

cv_iter_files=validation_parameters

idx=simulated_stats$mean24>0
simulated_stats=simulated_stats[idx,]
gauge_stats=gauge_stats[idx,]
cv_iter_files=cv_iter_files[idx,]

idx=simulated_stats$mean24<15
simulated_stats=simulated_stats[idx,]
gauge_stats=gauge_stats[idx,]
cv_iter_files=cv_iter_files[idx,]

################################################################
####GGPLOT PART##################################################
#################################################################


summary(simulated_stats)
file_save_name="D:/Proyectos_GitHub/Bartlet-Lewis_Regionalization/output/img/cv_stats_ago_test_borrar.png"
plot_comparison(gauge_stat = gauge_stat,simulated_stats = simulated_stats,file_save_name = file_save_name)




source('D:/Proyectos_GitHub/Bartlet-Lewis_Regionalization/src/InterpolationFunctions.R')


cv_iter_files <-'D:/Proyectos_GitHub/Bartlet-Lewis_Regionalization/output/CV_parameters/parameters_jan.csv' 
#cv_iter_files <- list.files(path = "D:/Proyectos_GitHub/Bartlet-Lewis_Regionalization/output/CV_parameters/iteraciones/",pattern = "\\.csv",full.names = T)
formulas=c(formula('a~1'),formula('l~1'),formula('v~1'),formula('k~1'),formula('f~1'),formula('mx~1'))
iter=1
for (file in cv_iter_files){
  parameters = read.csv(file)
  grd=readRDS('D:/Proyectos_GitHub/Bartlet-Lewis_Regionalization/data/grilla.rds')
  data=parameters
  coordinates(data) <- ~x+y
  proj4string(data)=crs(grd)
  
  for (k in 1:6){
    mapa=OK_interpolation(obs_point_data = data,model_grid_data = grd,var =formulas[[k]] )
    #writeRaster(mapa,paste0('D:/Proyectos_GitHub/Bartlet-Lewis_Regionalization/output/CV_parameters/iteraciones/','parametros','_',as.character(k),'_iter_',as.character(iter),'.tif'),overwrite=TRUE)
    writeRaster(mapa,paste0('D:/Proyectos_GitHub/Bartlet-Lewis_Regionalization/output/CV_maps/','parametros_agost','-',as.character(k),'.tif'),overwrite=TRUE)
  }
  iter=iter+1
}

source('D:/Proyectos_GitHub/Bartlet-Lewis_Regionalization/src/plotHelpers.R')


file_name="D:/Proyectos_GitHub/Bartlet-Lewis_Regionalization/output/img/parameters_march_borrar.png"
file1='d:/Proyectos_GitHub/Bartlet-Lewis_Regionalization/output/CV_maps/parametros_january-1.tif'
file2='d:/Proyectos_GitHub/Bartlet-Lewis_Regionalization/output/CV_maps/parametros_january-2.tif'
file3='d:/Proyectos_GitHub/Bartlet-Lewis_Regionalization/output/CV_maps/parametros_january-3.tif'
file4='d:/Proyectos_GitHub/Bartlet-Lewis_Regionalization/output/CV_maps/parametros_january-4.tif'
file5='d:/Proyectos_GitHub/Bartlet-Lewis_Regionalization/output/CV_maps/parametros_january-5.tif'
file6='d:/Proyectos_GitHub/Bartlet-Lewis_Regionalization/output/CV_maps/parametros_january-6.tif'

maps_plot(file1,file2,file3,file4,file5,file6,intervals = 10,file_save_name = file_name)
  



file_name="D:/Proyectos_GitHub/Bartlet-Lewis_Regionalization/output/img/parameters_agost_borrar.png"
file1='d:/Proyectos_GitHub/Bartlet-Lewis_Regionalization/output/CV_maps/parametros_agost-1.tif'
file2='d:/Proyectos_GitHub/Bartlet-Lewis_Regionalization/output/CV_maps/parametros_agost-2.tif'
file3='d:/Proyectos_GitHub/Bartlet-Lewis_Regionalization/output/CV_maps/parametros_agost-3.tif'
file4='d:/Proyectos_GitHub/Bartlet-Lewis_Regionalization/output/CV_maps/parametros_agost-4.tif'
file5='d:/Proyectos_GitHub/Bartlet-Lewis_Regionalization/output/CV_maps/parametros_agost-5.tif'
file6='d:/Proyectos_GitHub/Bartlet-Lewis_Regionalization/output/CV_maps/parametros_agost-6.tif'

maps_plot(file1,file2,file3,file4,file5,file6,intervals = 10,file_save_name = file_name)




