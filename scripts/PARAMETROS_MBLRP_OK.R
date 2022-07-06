source('D:/Proyectos_GitHub/Bartlet-Lewis_Regionalization/src/InterpolationFunctions.R')
library(raster)


grd=readRDS('D:/Proyectos_GitHub/Bartlet-Lewis_Regionalization/data/grilla.rds')



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