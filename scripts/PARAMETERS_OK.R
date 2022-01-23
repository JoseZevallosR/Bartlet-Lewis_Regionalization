source('D:/Proyectos_GitHub/Bartlet-Lewis_Regionalization/src/InterpolationFunctions.R')


cv_iter_files <-'D:/Proyectos_GitHub/Bartlet-Lewis_Regionalization/output/CV_parameters/parameters01.csv' #list.files(path = "D:/Proyectos_GitHub/Bartlet-Lewis_Regionalization/output/CV_parameters/iteraciones/",pattern = "\\.csv",full.names = T)

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
    writeRaster(mapa,paste0('D:/Proyectos_GitHub/Bartlet-Lewis_Regionalization/output/CV_parameters/iteraciones/','parametros','_',as.character(k),'_iter_',as.character(iter),'.tif'),overwrite=TRUE)
	}
	iter=iter+1
}






