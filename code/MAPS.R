library(raster)
library(ggplot2)
library(gridExtra)
library(gtable)
library(grid)
library(tmap)


':=' <- function(lhs, rhs) {
  frame <- parent.frame()
  lhs <- as.list(substitute(lhs))
  if (length(lhs) > 1)
    lhs <- lhs[-1]
  if (length(lhs) == 1) {
    do.call(`=`, list(lhs[[1]], rhs), envir=frame)
    return(invisible(NULL)) 
  }
  if (is.function(rhs) || is(rhs, 'formula'))
    rhs <- list(rhs)
  if (length(lhs) > length(rhs))
    rhs <- c(rhs, rep(list(NULL), length(lhs) - length(rhs)))
  for (i in 1:length(lhs))
    do.call(`=`, list(lhs[[i]], rhs[[i]]), envir=frame)
  return(invisible(NULL)) 
}

raster_to_df=function(dir,n){

  breaks_df=function(vector,n){
    seq(min(vector),max(vector),length.out=n)
  }
  
  reclass_raster=function(breaks){
    n=length(breaks)
    class_matrix=matrix(data=NA,nrow = n-1,ncol = 3)
    count=0
    legenda=c()
    for (i in 1:(n-1)){
      count=count+1
      class_matrix[i,]=c(breaks[i],breaks[i+1],count)
      
      legenda=c(legenda,paste(as.character(round(breaks[i],2)),as.character(round(breaks[i+1],2)),sep = '-'))
    }
    list(matt=class_matrix,legends=legenda)
  }
    
  map = raster(dir)
  c(matt,legenda):=reclass_raster(breaks = breaks_df(na.omit(values(map)),n))
  
  map = reclassify(map,matt,include.lowest=TRUE)
  
  map.p=rasterToPoints(map)
  df <- data.frame(map.p)
  colnames(df) = c("Longitude", "Latitude", "MAP")
  list(df=df,legenda=legenda)
}

plot_map=function(df.map,legenda,titulo){
  
  Mypal=gray.colors(length(legenda))

  #number of intervals
  ggplot(data=df.map, aes(y=Latitude, x=Longitude)) +
    geom_raster(aes(fill=factor(MAP))) +
    theme(legend.position="left")+
    scale_fill_manual(values =  Mypal,labels=legenda,name=titulo)+
    coord_equal()
  
}


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


