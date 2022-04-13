source('D:/Proyectos_GitHub/Bartlet-Lewis_Regionalization/src/InterpolationFunctions.R')
library(raster)


grd=readRDS('D:/Proyectos_GitHub/Bartlet-Lewis_Regionalization/data/grilla.rds')
mapas_mes = function(path_to_file,grd,month='feb'){
  parameters = read.csv(path_to_file)
  
  data=parameters
  coordinates(data) <- ~x+y
  proj4string(data)=crs(grd)
  
  formulas=c(formula('a~1'),formula('l~1'),formula('v~1'),formula('k~1'),formula('f~1'),formula('mx~1'))
 

    
  for (k in 1:6){
    mapa=OK_interpolation(data_points = data,model_grid_data = grd,var =formulas[[k]] )
    proj4string(mapa)=crs(grd)
    
    writeRaster(mapa,paste0('D:/Proyectos_GitHub/Bartlet-Lewis_Regionalization/output/CV_maps/','parametros_',as.character(k),'_',month,'.tif'),overwrite=TRUE)
  }
}


path_to_file='D:/Proyectos_GitHub/Bartlet-Lewis_Regionalization/output/CV_parameters/parameters_jan.csv' 
mapas_mes(path_to_file = path_to_file,grd,month = 'jan')

path_to_file='D:/Proyectos_GitHub/Bartlet-Lewis_Regionalization/output/CV_parameters/parameters_feb.csv' 
mapas_mes(path_to_file = path_to_file,grd,month = 'feb')

path_to_file='D:/Proyectos_GitHub/Bartlet-Lewis_Regionalization/output/CV_parameters/parameters_mar.csv' 
mapas_mes(path_to_file = path_to_file,grd,month = 'mar')

path_to_file='D:/Proyectos_GitHub/Bartlet-Lewis_Regionalization/output/CV_parameters/parameters_jul.csv' 
mapas_mes(path_to_file = path_to_file,grd,month = 'jul')

path_to_file='D:/Proyectos_GitHub/Bartlet-Lewis_Regionalization/output/CV_parameters/parameters_ago.csv' 
mapas_mes(path_to_file = path_to_file,grd,month = 'ago')












cv_iter_files <-'D:/Proyectos_GitHub/Bartlet-Lewis_Regionalization/output/CV_parameters/' 
cv_iter_files <- list.files(path = "D:/Proyectos_GitHub/Bartlet-Lewis_Regionalization/output/CV_parameters/iteraciones/",pattern = "\\.csv",full.names = T)
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
    writeRaster(mapa,paste0('D:/Proyectos_GitHub/Bartlet-Lewis_Regionalization/output/CV_maps/','parametros_january','-',as.character(k),'.tif'),overwrite=TRUE)
	}
	iter=iter+1
}






