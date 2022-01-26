source('D:/Proyectos_GitHub/Bartlet-Lewis_Regionalization/src/plotHelpers.R')

a=expression(alpha)
l=expression(paste(lambda,'(1/hr)'))
v=expression(paste(upsilon,'(hr)'))
k=expression(kappa)
phi=expression(phi)
u=expression(paste(mu,'(mm/hr)'))



c(df.map,legenda):=raster_to_df('d:/Proyectos_GitHub/Bartlet-Lewis_Regionalization/output/CV_maps/parametros-1.tif',10)
mapa1=plot_map(df.map,legenda,titulo = a)

c(df.map,legenda):=raster_to_df('d:/Proyectos_GitHub/Bartlet-Lewis_Regionalization/output/CV_maps/parametros-2.tif',10)
mapa2=plot_map(df.map,legenda,titulo = l)

c(df.map,legenda):=raster_to_df('d:/Proyectos_GitHub/Bartlet-Lewis_Regionalization/output/CV_maps/parametros-3.tif',10)
mapa3=plot_map(df.map,legenda,titulo = v)

c(df.map,legenda):=raster_to_df('d:/Proyectos_GitHub/Bartlet-Lewis_Regionalization/output/CV_maps/parametros-4.tif',10)
mapa4=plot_map(df.map,legenda,titulo = k)

c(df.map,legenda):=raster_to_df('d:/Proyectos_GitHub/Bartlet-Lewis_Regionalization/output/CV_maps/parametros-5.tif',10)
mapa5=plot_map(df.map,legenda,titulo = phi)

c(df.map,legenda):=raster_to_df('d:/Proyectos_GitHub/Bartlet-Lewis_Regionalization/output/CV_maps/parametros-6.tif',10)
mapa6=plot_map(df.map,legenda,titulo = u)


grid.arrange(arrangeGrob(mapa1,mapa2,mapa3,mapa4,mapa5,mapa6,nrow = 2))

#Storm characteristics

c(df.map,legenda):=raster_to_df('d:/Proyectos_GitHub/Bartlet-Lewis_Regionalization/output/CV_maps/storm_characteristics/average_number_cell_per_storn.tif',6)
mapa1=plot_map(df.map,legenda,titulo = '(a) Average number of rain cell per storm')

c(df.map,legenda):=raster_to_df('d:/Proyectos_GitHub/Bartlet-Lewis_Regionalization/output/CV_maps/storm_characteristics/average_rain_cell_duration.tif',6)
mapa2=plot_map(df.map,legenda,titulo = '(b) Average duration of rain cell (hr)')

c(df.map,legenda):=raster_to_df('d:/Proyectos_GitHub/Bartlet-Lewis_Regionalization/output/CV_maps/storm_characteristics/average_storm_duration.tif',6)
mapa3=plot_map(df.map,legenda,titulo = '(c) Average rainfall duration (hr)')

c(df.map,legenda):=raster_to_df('d:/Proyectos_GitHub/Bartlet-Lewis_Regionalization/output/CV_maps/storm_characteristics/average_rainfall_deph_storm.tif',6)
mapa4=plot_map(df.map,legenda,titulo = '(d) Average rain deph per storm (mm)')



grid.arrange(arrangeGrob(mapa1,mapa2,mapa3,mapa4,nrow = 2))


cv_iter_files=files <- list.files(path = "D:/Proyectos_GitHub/Bartlet-Lewis_Regionalization/output/CV_parameters/iteraciones/",pattern = "\\.tif",full.names = T)

plot(stack(cv_iter_files[1:9]))

maps=list()
counter=1

for (i in 46:54){
	c(df.map,legenda):=raster_to_df(cv_iter_files[i],10)
	maps[[counter]]=plot_map(df.map,legenda,titulo = paste('interation',as.character(counter)))
	counter=counter+1
}

dev.new()
grid.arrange(arrangeGrob(maps[[1]],maps[[2]],maps[[3]],maps[[4]],maps[[5]],maps[[6]],maps[[7]],maps[[8]],maps[[9]],nrow = 3))
