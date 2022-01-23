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

c(df.map,legenda):=raster_to_df('d:/Proyectos_GitHub/Bartlet-Lewis_Regionalization/output/CV_maps/average_number_cell_per_storn.tif',5)
mapa1=plot_map(df.map,legenda,titulo = '(a) Average number of rain cell per storm')

c(df.map,legenda):=raster_to_df('d:/Proyectos_GitHub/Bartlet-Lewis_Regionalization/output/CV_maps/average_rain_cell_duration.tif',5)
mapa2=plot_map(df.map,legenda,titulo = '(b) Average duration of rain cell (hr)')

c(df.map,legenda):=raster_to_df('d:/Proyectos_GitHub/Bartlet-Lewis_Regionalization/output/CV_maps/average_storm_duration.tif',5)
mapa3=plot_map(df.map,legenda,titulo = '(c) Average rainfall duration (hr)')

c(df.map,legenda):=raster_to_df('d:/Proyectos_GitHub/Bartlet-Lewis_Regionalization/output/CV_maps/average_rainfall_deph_storm.tif',5)
mapa4=plot_map(df.map,legenda,titulo = '(d) Average rain deph per storm (mm)')



grid.arrange(arrangeGrob(mapa1,mapa2,mapa3,mapa4,nrow = 2))


cv_iter_files=files <- list.files(path = "D:/Proyectos_GitHub/Bartlet-Lewis_Regionalization/output/CV_parameters/iteraciones/",pattern = "\\.tif",full.names = T)

plot(stack(cv_iter_files[1:9]))

maps=list()


for (i in 1:9){
	c(df.map,legenda):=raster_to_df(cv_iter_files[i+45],10)
	maps[[i]]=plot_map(df.map,legenda,titulo = u)
}

dev.new()
grid.arrange(arrangeGrob(maps[[1]],maps[[2]],maps[[3]],maps[[4]],maps[[6]],maps[[9]],nrow = 2))
