source('D:/Proyectos_GitHub/Bartlet-Lewis_Regionalization/src/plotHelpers.R')

a=expression(alpha)
l=expression(paste(lambda,'(1/hr)'))
v=expression(paste(upsilon,'(hr)'))
k=expression(kappa)
phi=expression(phi)
u=expression(paste(mu,'(mm/hr)'))


c(df.map,legenda):=raster_to_df('d:/Proyectos_GitHub/Bartlet-Lewis_Regionalization/output/CV_maps/parametros-1.tif',11)
mapa1=plot_map(df.map,legenda,titulo = a)

c(df.map,legenda):=raster_to_df('d:/Proyectos_GitHub/Bartlet-Lewis_Regionalization/output/CV_maps/parametros-2.tif',11)
mapa2=plot_map(df.map,legenda,titulo = l)

c(df.map,legenda):=raster_to_df('d:/Proyectos_GitHub/Bartlet-Lewis_Regionalization/output/CV_maps/parametros-3.tif',11)
mapa3=plot_map(df.map,legenda,titulo = v)

c(df.map,legenda):=raster_to_df('d:/Proyectos_GitHub/Bartlet-Lewis_Regionalization/output/CV_maps/parametros-4.tif',11)
mapa4=plot_map(df.map,legenda,titulo = k)

c(df.map,legenda):=raster_to_df('d:/Proyectos_GitHub/Bartlet-Lewis_Regionalization/output/CV_maps/parametros-5.tif',11)
mapa5=plot_map(df.map,legenda,titulo = phi)

c(df.map,legenda):=raster_to_df('d:/Proyectos_GitHub/Bartlet-Lewis_Regionalization/output/CV_maps/parametros-6.tif',11)
mapa6=plot_map(df.map,legenda,titulo = u)


grid.arrange(arrangeGrob(mapa1,mapa2,mapa3,mapa4,mapa5,mapa6,nrow = 2))



c(df.map,legenda):=raster_to_df('d:/Proyectos_GitHub/Bartlet-Lewis_Regionalization/output/CV_maps/average_hours_between_storm.tif',11)
mapa1=plot_map(df.map,legenda,titulo = 'Average hours between storm')

c(df.map,legenda):=raster_to_df('d:/Proyectos_GitHub/Bartlet-Lewis_Regionalization/output/CV_maps/average_storm_duration.tif',30)
mapa2=plot_map(df.map,legenda,titulo = 'Average storm duration')

c(df.map,legenda):=raster_to_df('d:/Proyectos_GitHub/Bartlet-Lewis_Regionalization/output/CV_maps/average_rainfall_deph_storm.tif',11)
mapa3=plot_map(df.map,legenda,titulo = 'Average rain deph per storm')


grid.arrange(arrangeGrob(mapa1,mapa2,mapa3,nrow = 1))


