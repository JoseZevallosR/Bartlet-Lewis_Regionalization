
source('D:/Proyectos_GitHub/Bartlet-Lewis_Regionalization/src/RMBLRP.R')
source('D:/Proyectos_GitHub/Bartlet-Lewis_Regionalization/src/plotHelpers.R')

setwd("D:/Proyectos_GitHub/Bartlet-Lewis_Regionalization/data/Area_estudio")

peru=shapefile('Limite Perú250_gcs.shp')
gauge=points_wgs84(read.csv('gauges.csv',sep = ';'))
i_gauge=read.csv('independent_gauges.csv',sep = ';')
names(i_gauge)=c('x','y','gauge')
i_gauge=points_wgs84(i_gauge)


plot(peru)
points(gauge, add=T)
