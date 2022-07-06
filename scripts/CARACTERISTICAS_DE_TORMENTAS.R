library(raster)

a=raster('d:/Proyectos_GitHub/Bartlet-Lewis_Regionalization/output/CV_maps/feb/parametros_1_feb.tif')
v=raster('d:/Proyectos_GitHub/Bartlet-Lewis_Regionalization/output/CV_maps/feb/parametros_3_feb.tif')
fi=raster('d:/Proyectos_GitHub/Bartlet-Lewis_Regionalization/output/CV_maps/feb/parametros_5_feb.tif')
k=raster('d:/Proyectos_GitHub/Bartlet-Lewis_Regionalization/output/CV_maps/feb/parametros_4_feb.tif')
l=raster('d:/Proyectos_GitHub/Bartlet-Lewis_Regionalization/output/CV_maps/feb/parametros_2_feb.tif')
mu=raster('d:/Proyectos_GitHub/Bartlet-Lewis_Regionalization/output/CV_maps/feb/parametros_6_feb.tif')


average_rain_cell_duration=v/a
average_number_cell_per_storn=1+k/fi
average_storm_duration=v/(fi*a)#
average_rainfall_deph_storm=mu*(v/a)*(1+k/fi)

writeRaster(average_rain_cell_duration,'d:/Proyectos_GitHub/Bartlet-Lewis_Regionalization/output/storm_characteristics/average_rain_cell_duration.tif',overwrite=T)
writeRaster(average_storm_duration,'d:/Proyectos_GitHub/Bartlet-Lewis_Regionalization/output/storm_characteristics/average_storm_duration.tif',overwrite=T)
writeRaster(average_rainfall_deph_storm,'d:/Proyectos_GitHub/Bartlet-Lewis_Regionalization/output/storm_characteristics/average_rainfall_deph_storm.tif',overwrite=T)
writeRaster(average_number_cell_per_storn,'d:/Proyectos_GitHub/Bartlet-Lewis_Regionalization/output/storm_characteristics/average_number_cell_per_storn.tif',overwrite=T)