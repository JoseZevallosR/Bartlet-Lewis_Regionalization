source('D:/Proyectos_GitHub/Bartlet-Lewis_Regionalization/src/plotHelpers.R')

Peru <- shapefile("d:/Proyectos_GitHub/Bartlet-Lewis_Regionalization/data/Area_estudio/Limite Perú250_gcs.shp")
# Next the shapefile has to be converted to a dataframe for use in ggplot2
shapefile_df <- fortify(Peru)


a=expression(alpha)
l=expression(paste(lambda,'(1/hr)'))
v=expression(paste(upsilon,'(hr)'))
k=expression(kappa)
phi=expression(phi)
u=expression(paste(mu,'(mm/hr)'))


#feb
dir1='d:/Proyectos_GitHub/Bartlet-Lewis_Regionalization/output/CV_maps/feb/parametros_1_feb.tif'
dir2='d:/Proyectos_GitHub/Bartlet-Lewis_Regionalization/output/CV_maps/feb/parametros_2_feb.tif'
dir3='d:/Proyectos_GitHub/Bartlet-Lewis_Regionalization/output/CV_maps/feb/parametros_3_feb.tif'
dir4='d:/Proyectos_GitHub/Bartlet-Lewis_Regionalization/output/CV_maps/feb/parametros_4_feb.tif'
dir5='d:/Proyectos_GitHub/Bartlet-Lewis_Regionalization/output/CV_maps/feb/parametros_5_feb.tif'
dir6='d:/Proyectos_GitHub/Bartlet-Lewis_Regionalization/output/CV_maps/feb/parametros_6_feb.tif'

c(df.map,legenda):=raster_to_df(dir1,10)
mapa1=plot_map(df.map,legenda,titulo = a,shapefile_df)
c(df.map,legenda):=raster_to_df(dir2,10)
mapa2=plot_map(df.map,legenda,titulo = l,shapefile_df)
c(df.map,legenda):=raster_to_df(dir3,10)
mapa3=plot_map(df.map,legenda,titulo = v,shapefile_df)
c(df.map,legenda):=raster_to_df(dir4,10)
mapa4=plot_map(df.map,legenda,titulo = k,shapefile_df)
c(df.map,legenda):=raster_to_df(dir5,10)
mapa5=plot_map(df.map,legenda,titulo = phi,shapefile_df)
c(df.map,legenda):=raster_to_df(dir6,10)
mapa6=plot_map(df.map,legenda,titulo = u,shapefile_df)
figure1=grid.arrange(arrangeGrob(mapa1,mapa2,mapa3,mapa4,mapa5,mapa6,nrow = 3))
ggsave("D:/Proyectos_GitHub/Bartlet-Lewis_Regionalization/output/img/parameters_february.png",figure1,dpi=1200,units = 'cm',width =20 ,height =25 )


#march
dir1='d:/Proyectos_GitHub/Bartlet-Lewis_Regionalization/output/CV_maps/mar/parametros_1_mar.tif'
dir2='d:/Proyectos_GitHub/Bartlet-Lewis_Regionalization/output/CV_maps/mar/parametros_2_mar.tif'
dir3='d:/Proyectos_GitHub/Bartlet-Lewis_Regionalization/output/CV_maps/mar/parametros_3_mar.tif'
dir4='d:/Proyectos_GitHub/Bartlet-Lewis_Regionalization/output/CV_maps/mar/parametros_4_mar.tif'
dir5='d:/Proyectos_GitHub/Bartlet-Lewis_Regionalization/output/CV_maps/mar/parametros_5_mar.tif'
dir6='d:/Proyectos_GitHub/Bartlet-Lewis_Regionalization/output/CV_maps/mar/parametros_6_mar.tif'

c(df.map,legenda):=raster_to_df(dir1,10)
mapa1=plot_map(df.map,legenda,titulo = a,shapefile_df)
c(df.map,legenda):=raster_to_df(dir2,10)
mapa2=plot_map(df.map,legenda,titulo = l,shapefile_df)
c(df.map,legenda):=raster_to_df(dir3,10)
mapa3=plot_map(df.map,legenda,titulo = v,shapefile_df)
c(df.map,legenda):=raster_to_df(dir4,10)
mapa4=plot_map(df.map,legenda,titulo = k,shapefile_df)
c(df.map,legenda):=raster_to_df(dir5,10)
mapa5=plot_map(df.map,legenda,titulo = phi,shapefile_df)
c(df.map,legenda):=raster_to_df(dir6,10)
mapa6=plot_map(df.map,legenda,titulo = u,shapefile_df)
figure1=grid.arrange(arrangeGrob(mapa1,mapa2,mapa3,mapa4,mapa5,mapa6,nrow = 3))
ggsave("D:/Proyectos_GitHub/Bartlet-Lewis_Regionalization/output/img/parameters_march.png",figure1,dpi=1200,units = 'cm',width =20 ,height =25 )

#jan
dir1='d:/Proyectos_GitHub/Bartlet-Lewis_Regionalization/output/CV_maps/jan/parametros_1_jan.tif'
dir2='d:/Proyectos_GitHub/Bartlet-Lewis_Regionalization/output/CV_maps/jan/parametros_2_jan.tif'
dir3='d:/Proyectos_GitHub/Bartlet-Lewis_Regionalization/output/CV_maps/jan/parametros_3_jan.tif'
dir4='d:/Proyectos_GitHub/Bartlet-Lewis_Regionalization/output/CV_maps/jan/parametros_4_jan.tif'
dir5='d:/Proyectos_GitHub/Bartlet-Lewis_Regionalization/output/CV_maps/jan/parametros_5_jan.tif'
dir6='d:/Proyectos_GitHub/Bartlet-Lewis_Regionalization/output/CV_maps/jan/parametros_6_jan.tif'

c(df.map,legenda):=raster_to_df(dir1,10)
mapa1=plot_map(df.map,legenda,titulo = a,shapefile_df)
c(df.map,legenda):=raster_to_df(dir2,10)
mapa2=plot_map(df.map,legenda,titulo = l,shapefile_df)
c(df.map,legenda):=raster_to_df(dir3,10)
mapa3=plot_map(df.map,legenda,titulo = v,shapefile_df)
c(df.map,legenda):=raster_to_df(dir4,10)
mapa4=plot_map(df.map,legenda,titulo = k,shapefile_df)
c(df.map,legenda):=raster_to_df(dir5,10)
mapa5=plot_map(df.map,legenda,titulo = phi,shapefile_df)
c(df.map,legenda):=raster_to_df(dir6,10)
mapa6=plot_map(df.map,legenda,titulo = u,shapefile_df)
figure1=grid.arrange(arrangeGrob(mapa1,mapa2,mapa3,mapa4,mapa5,mapa6,nrow = 3))
ggsave("D:/Proyectos_GitHub/Bartlet-Lewis_Regionalization/output/img/parameters_january.png",figure1,dpi=1200,units = 'cm',width =20 ,height =25 )

#jul
dir1='d:/Proyectos_GitHub/Bartlet-Lewis_Regionalization/output/CV_maps/jul/parametros_1_jul.tif'
dir2='d:/Proyectos_GitHub/Bartlet-Lewis_Regionalization/output/CV_maps/jul/parametros_2_jul.tif'
dir3='d:/Proyectos_GitHub/Bartlet-Lewis_Regionalization/output/CV_maps/jul/parametros_3_jul.tif'
dir4='d:/Proyectos_GitHub/Bartlet-Lewis_Regionalization/output/CV_maps/jul/parametros_4_jul.tif'
dir5='d:/Proyectos_GitHub/Bartlet-Lewis_Regionalization/output/CV_maps/jul/parametros_5_jul.tif'
dir6='d:/Proyectos_GitHub/Bartlet-Lewis_Regionalization/output/CV_maps/jul/parametros_6_jul.tif'

c(df.map,legenda):=raster_to_df(dir1,10)
mapa1=plot_map(df.map,legenda,titulo = a,shapefile_df)
c(df.map,legenda):=raster_to_df(dir2,10)
mapa2=plot_map(df.map,legenda,titulo = l,shapefile_df)
c(df.map,legenda):=raster_to_df(dir3,10)
mapa3=plot_map(df.map,legenda,titulo = v,shapefile_df)
c(df.map,legenda):=raster_to_df(dir4,10)
mapa4=plot_map(df.map,legenda,titulo = k,shapefile_df)
c(df.map,legenda):=raster_to_df(dir5,10)
mapa5=plot_map(df.map,legenda,titulo = phi,shapefile_df)
c(df.map,legenda):=raster_to_df(dir6,10)
mapa6=plot_map(df.map,legenda,titulo = u,shapefile_df)
figure1=grid.arrange(arrangeGrob(mapa1,mapa2,mapa3,mapa4,mapa5,mapa6,nrow = 3))
ggsave("D:/Proyectos_GitHub/Bartlet-Lewis_Regionalization/output/img/parameters_july.png",figure1,dpi=1200,units = 'cm',width =20 ,height =25 )

#agost
dir1='d:/Proyectos_GitHub/Bartlet-Lewis_Regionalization/output/CV_maps/ago/parametros_1_ago.tif'
dir2='d:/Proyectos_GitHub/Bartlet-Lewis_Regionalization/output/CV_maps/ago/parametros_2_ago.tif'
dir3='d:/Proyectos_GitHub/Bartlet-Lewis_Regionalization/output/CV_maps/ago/parametros_3_ago.tif'
dir4='d:/Proyectos_GitHub/Bartlet-Lewis_Regionalization/output/CV_maps/ago/parametros_4_ago.tif'
dir5='d:/Proyectos_GitHub/Bartlet-Lewis_Regionalization/output/CV_maps/ago/parametros_5_ago.tif'
dir6='d:/Proyectos_GitHub/Bartlet-Lewis_Regionalization/output/CV_maps/ago/parametros_6_ago.tif'

c(df.map,legenda):=raster_to_df(dir1,10)
mapa1=plot_map(df.map,legenda,titulo = a,shapefile_df)
c(df.map,legenda):=raster_to_df(dir2,10)
mapa2=plot_map(df.map,legenda,titulo = l,shapefile_df)
c(df.map,legenda):=raster_to_df(dir3,10)
mapa3=plot_map(df.map,legenda,titulo = v,shapefile_df)
c(df.map,legenda):=raster_to_df(dir4,10)
mapa4=plot_map(df.map,legenda,titulo = k,shapefile_df)
c(df.map,legenda):=raster_to_df(dir5,10)
mapa5=plot_map(df.map,legenda,titulo = phi,shapefile_df)
c(df.map,legenda):=raster_to_df(dir6,10)
mapa6=plot_map(df.map,legenda,titulo = u,shapefile_df)
figure1=grid.arrange(arrangeGrob(mapa1,mapa2,mapa3,mapa4,mapa5,mapa6,nrow = 3))
ggsave("D:/Proyectos_GitHub/Bartlet-Lewis_Regionalization/output/img/parameters_agost.png",figure1,dpi=1200,units = 'cm',width =20 ,height =25 )


########################
#Storm characteristics##
########################

c(df.map,legenda):=raster_to_df('d:/Proyectos_GitHub/Bartlet-Lewis_Regionalization/output/storm_characteristics/average_number_cell_per_storn.tif',6)
mapa1=plot_map(df.map,legenda,titulo = '(a) Average number \n of rain cell per storm',shapefile_df)

c(df.map,legenda):=raster_to_df('d:/Proyectos_GitHub/Bartlet-Lewis_Regionalization/output/storm_characteristics/average_rain_cell_duration.tif',6)
mapa2=plot_map(df.map,legenda,titulo = '(b) Average duration \n of rain cell (hr)',shapefile_df)

c(df.map,legenda):=raster_to_df('d:/Proyectos_GitHub/Bartlet-Lewis_Regionalization/output/storm_characteristics/average_storm_duration.tif',6)
mapa3=plot_map(df.map,legenda,titulo = '(c) Average rainfall \n duration (hr)',shapefile_df)

c(df.map,legenda):=raster_to_df('d:/Proyectos_GitHub/Bartlet-Lewis_Regionalization/output/storm_characteristics/average_rainfall_deph_storm.tif',6)
mapa4=plot_map(df.map,legenda,titulo = '(d) Average rain deph \n per storm (mm)',shapefile_df)



figure2=grid.arrange(arrangeGrob(mapa1,mapa2,mapa3,mapa4,nrow = 2))
ggsave("D:/Proyectos_GitHub/Bartlet-Lewis_Regionalization/output/img/storm_characteristics.png",figure2,dpi=1200,units = 'cm',width =20 ,height =20 )



#español

c(df.map,legenda):=raster_to_df('d:/Proyectos_GitHub/Bartlet-Lewis_Regionalization/output/storm_characteristics/average_number_cell_per_storn.tif',6)
mapa1=plot_map(df.map,legenda,titulo = '(a) Número promedio \n de celdas por tormenta',shapefile_df)

c(df.map,legenda):=raster_to_df('d:/Proyectos_GitHub/Bartlet-Lewis_Regionalization/output/storm_characteristics/average_rain_cell_duration.tif',6)
mapa2=plot_map(df.map,legenda,titulo = '(b) Duración promedio \n de celdas de lluvia (hr)',shapefile_df)

c(df.map,legenda):=raster_to_df('d:/Proyectos_GitHub/Bartlet-Lewis_Regionalization/output/storm_characteristics/average_storm_duration.tif',6)
mapa3=plot_map(df.map,legenda,titulo = '(c) Duración promedio \n de precipitacion (hr)',shapefile_df)

c(df.map,legenda):=raster_to_df('d:/Proyectos_GitHub/Bartlet-Lewis_Regionalization/output/storm_characteristics/average_rainfall_deph_storm.tif',6)
mapa4=plot_map(df.map,legenda,titulo = '(d) Altura promedio de precipitacion \n por tormenta (mm)',shapefile_df)



figure2=grid.arrange(arrangeGrob(mapa1,mapa2,mapa3,mapa4,nrow = 2))
ggsave("D:/Proyectos_GitHub/Bartlet-Lewis_Regionalization/output/img/tormentas_caracteristicas.png",figure2,dpi=1200,units = 'cm',width =20 ,height =20 )



##################################
###### RCV - iterations##########
################################

cv_iter_files=files <- list.files(path = "D:/Proyectos_GitHub/Bartlet-Lewis_Regionalization/output/CV_parameters/iteraciones/",pattern = "\\.tif",full.names = T)

maps=list()
counter=1
#check language
for (i in 46:54){
	c(df.map,legenda):=raster_to_df(cv_iter_files[i],10)
	maps[[counter]]=plot_map(df.map,legenda,titulo = paste('interación',as.character(counter-1)),shapefile_df)
	counter=counter+1
}


figure3=grid.arrange(arrangeGrob(maps[[1]],maps[[2]],maps[[3]],maps[[4]],maps[[5]],maps[[6]],nrow = 3))
ggsave("D:/Proyectos_GitHub/Bartlet-Lewis_Regionalization/output/img/iteracion_mux.png",figure3,dpi=1200,units = 'cm',width =20 ,height =25 )
