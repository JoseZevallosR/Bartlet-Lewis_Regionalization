library(raster)

a=raster('d:/Proyectos_GitHub/Bartlet-Lewis_Regionalization/output/CV_maps/parametros-1.tif')
v=raster('d:/Proyectos_GitHub/Bartlet-Lewis_Regionalization/output/CV_maps/parametros-3.tif')
fi=raster('d:/Proyectos_GitHub/Bartlet-Lewis_Regionalization/output/CV_maps/parametros-5.tif')
k=raster('d:/Proyectos_GitHub/Bartlet-Lewis_Regionalization/output/CV_maps/parametros-4.tif')
l=raster('d:/Proyectos_GitHub/Bartlet-Lewis_Regionalization/output/CV_maps/parametros-2.tif')
mu=raster('d:/Proyectos_GitHub/Bartlet-Lewis_Regionalization/output/CV_maps/parametros-6.tif')


average_rain_cell_duration=v*a
average_number_cell_per_storn=1+k/fi
average_storm_duration=v*a*(fi)#
average_rainfall_deph_storm=mu*v*a*(1+k/fi)

writeRaster(average_rain_cell_duration,'d:/Proyectos_GitHub/Bartlet-Lewis_Regionalization/output/CV_maps/average_rain_cell_duration.tif',overwrite=T)
writeRaster(average_storm_duration,'d:/Proyectos_GitHub/Bartlet-Lewis_Regionalization/output/CV_maps/average_storm_duration.tif',overwrite=T)
writeRaster(average_rainfall_deph_storm,'d:/Proyectos_GitHub/Bartlet-Lewis_Regionalization/output/CV_maps/average_rainfall_deph_storm.tif',overwrite=T)
writeRaster(average_number_cell_per_storn,'d:/Proyectos_GitHub/Bartlet-Lewis_Regionalization/output/CV_maps/average_number_cell_per_storn.tif',overwrite=T)



#plot(average_rainfall_deph_storm)

#plot(average_storm_duration)

#plot(average_hours_between_storm)

