library(raster)
library(ggplot2)
library(gridExtra)
library(gtable)
library(grid)
#library(tmap)
library(dplyr)



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

g_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)}

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

plot_map=function(df.map,legenda,titulo,shapefile_df){
  
  #Mypal=gray.colors(length(legenda))
  Mypal=topo.colors(length(legenda))
  #number of intervals
  ggplot(data=df.map, aes(y=Latitude, x=Longitude)) +
    geom_raster(aes(fill=factor(MAP))) +scale_x_continuous(limits = c(-82.0,-67.5),breaks= scales::pretty_breaks(n=4))+
    theme(axis.text.x = element_text(size=10),legend.key.height = unit(0.5, 'cm'),legend.text = element_text(size=10),legend.position="left",plot.title = element_text(hjust = 0.5))+ggtitle(titulo)+
    scale_fill_manual(values =  Mypal,labels=legenda,name=NULL)+
    coord_equal()+geom_path(data = shapefile_df,aes(x = long, y = lat, group = group),color = 'black', size = .5)
  
}

graficoVS=function(y,obs,sim){
  df=data.frame(y,obs,sim)
  names(df)=c('y','obs','sim')
  df=mutate(df,Location=factor(case_when(y>-19   & y<=-12.7 ~'Costa sur',
                                         y>-12.7 & y<=-6.4  ~ 'Costa central',
                                         y>-6.4  & y<=0     ~'Costa norte')))
  #ggplot(df, aes(x = obs, y = sim,colour=Location))
  img=ggplot(df, aes(x = obs, y = sim))+geom_point(size=2, shape=20)+geom_abline(intercept = 0)
  #leyenda=g_legend(img)
  img=img+theme(legend.position="none")+theme(axis.title.x=element_blank(),
                                              
                                              axis.ticks.x=element_blank(),
                                              axis.title.y=element_blank(),
                                              
                                              axis.ticks.y=element_blank())
  leyenda=0
  list(img,leyenda)
}



plot_comparison=function(gauge_stat,simulated_stats,file_save_name,language='Spanish'){
  if (language == 'Spanish'){
    c(img11,legenda):=graficoVS(gauge_stats$y,gauge_stats$mean24,simulated_stats$mean24)
    img11=img11+labs(title='Promedio (mm)')+theme(plot.title = element_text(hjust = 0.5,size=10))
    
    img12=graficoVS(gauge_stats$y,gauge_stats$var24,simulated_stats$var24)[[1]]
    img12=img12+labs(title='Varianza')+theme(plot.title = element_text(hjust = 0.5,size=10))
    
    img13=graficoVS(gauge_stats$y,gauge_stats$autocov24,simulated_stats$autocov24)[[1]]
    img13=img13+labs(title='Lag-1 Autocovarianza')+theme(plot.title = element_text(hjust = 0.5,size=10))
    
    img14=graficoVS(gauge_stats$y,gauge_stats$dryperiod24,simulated_stats$dryperiod24)[[1]]
    img14=img14+labs(title='Probabilidad de 0 lluvia')+theme(plot.title = element_text(hjust = 0.5,size=10))
    
    c(img21,legenda):=graficoVS(gauge_stats$y,gauge_stats$mean3,simulated_stats$mean3)
    img22=graficoVS(gauge_stats$y,gauge_stats$var3,simulated_stats$var3)[[1]]
    img23=graficoVS(gauge_stats$y,gauge_stats$autocov3,simulated_stats$autocov3)[[1]]
    img24=graficoVS(gauge_stats$y,gauge_stats$dryperiod3,simulated_stats$dryperiod3)[[1]]
    
    c(img31,legenda):=graficoVS(gauge_stats$y,gauge_stats$mean6,simulated_stats$mean6)
    img32=graficoVS(gauge_stats$y,gauge_stats$var6,simulated_stats$var6)[[1]]
    img33=graficoVS(gauge_stats$y,gauge_stats$autocov6,simulated_stats$autocov6)[[1]]
    img34=graficoVS(gauge_stats$y,gauge_stats$dryperiod6,simulated_stats$dryperiod6)[[1]]
    
    c(img41,legenda):=graficoVS(gauge_stats$y,gauge_stats$mean12,simulated_stats$mean12)
    img42=graficoVS(gauge_stats$y,gauge_stats$var12,simulated_stats$var12)[[1]]
    img43=graficoVS(gauge_stats$y,gauge_stats$autocov12,simulated_stats$autocov12)[[1]]
    img44=graficoVS(gauge_stats$y,gauge_stats$dryperiod12,simulated_stats$dryperiod12)[[1]]
    
    bottom <- textGrob("Precipitación diaria", gp = gpar(fontsize = 9))
    left <- textGrob('CV Simulado 24-Horas Nivel acum ', gp = gpar(fontsize = 9),rot = 90)
    p1=arrangeGrob(img11,img12,img13,img14,nrow = 1,ncol = 4,bottom = bottom,left = left)
    
    left <- textGrob('CV Simulado 3-Horas Nivel acum ', gp = gpar(fontsize = 9),rot = 90)
    bottom <- textGrob("TRMM sesgo corregido", gp = gpar(fontsize = 10))
    p2=arrangeGrob(img21,img22,img23,img24,nrow = 1,ncol = 4,bottom = bottom,left = left)
    
    left <- textGrob('CV Simulado 6-Horas Nivel acum ', gp = gpar(fontsize = 9),rot = 90)
    bottom <- textGrob("TRMM sesgo corregido", gp = gpar(fontsize = 10))
    p3=arrangeGrob(img31,img32,img33,img34,nrow = 1,ncol = 4,bottom = bottom,left = left)
    
    left <- textGrob('CV Simulado 12-Horas Nivel acum ', gp = gpar(fontsize = 9),rot = 90)
    bottom <- textGrob("TRMM sesgo corregido", gp = gpar(fontsize = 10))
    p4=arrangeGrob(img41,img42,img43,img44,nrow = 1,ncol = 4,bottom = bottom,left = left)
    
  }else{
    c(img11,legenda):=graficoVS(gauge_stats$y,gauge_stats$mean24,simulated_stats$mean24)
    img11=img11+labs(title='Mean Rainfall (mm)')+theme(plot.title = element_text(hjust = 0.5,size=10))
    
    img12=graficoVS(gauge_stats$y,gauge_stats$var24,simulated_stats$var24)[[1]]
    img12=img12+labs(title='Variance')+theme(plot.title = element_text(hjust = 0.5,size=10))
    
    img13=graficoVS(gauge_stats$y,gauge_stats$autocov24,simulated_stats$autocov24)[[1]]
    img13=img13+labs(title='Lag-1 Autocovariance')+theme(plot.title = element_text(hjust = 0.5,size=10))
    
    img14=graficoVS(gauge_stats$y,gauge_stats$dryperiod24,simulated_stats$dryperiod24)[[1]]
    img14=img14+labs(title='Probability of 0 Rain')+theme(plot.title = element_text(hjust = 0.5,size=10))
    
    c(img21,legenda):=graficoVS(gauge_stats$y,gauge_stats$mean3,simulated_stats$mean3)
    img22=graficoVS(gauge_stats$y,gauge_stats$var3,simulated_stats$var3)[[1]]
    img23=graficoVS(gauge_stats$y,gauge_stats$autocov3,simulated_stats$autocov3)[[1]]
    img24=graficoVS(gauge_stats$y,gauge_stats$dryperiod3,simulated_stats$dryperiod3)[[1]]
    
    c(img31,legenda):=graficoVS(gauge_stats$y,gauge_stats$mean6,simulated_stats$mean6)
    img32=graficoVS(gauge_stats$y,gauge_stats$var6,simulated_stats$var6)[[1]]
    img33=graficoVS(gauge_stats$y,gauge_stats$autocov6,simulated_stats$autocov6)[[1]]
    img34=graficoVS(gauge_stats$y,gauge_stats$dryperiod6,simulated_stats$dryperiod6)[[1]]
    
    c(img41,legenda):=graficoVS(gauge_stats$y,gauge_stats$mean12,simulated_stats$mean12)
    img42=graficoVS(gauge_stats$y,gauge_stats$var12,simulated_stats$var12)[[1]]
    img43=graficoVS(gauge_stats$y,gauge_stats$autocov12,simulated_stats$autocov12)[[1]]
    img44=graficoVS(gauge_stats$y,gauge_stats$dryperiod12,simulated_stats$dryperiod12)[[1]]
    
    bottom <- textGrob("Daily rain gauge", gp = gpar(fontsize = 9))
    left <- textGrob('CV Simulated 24-Hourly Acc Level', gp = gpar(fontsize = 9),rot = 90)
    p1=arrangeGrob(img11,img12,img13,img14,nrow = 1,ncol = 4,bottom = bottom,left = left)
    
    left <- textGrob('CV Simulated 3-Hourly Acc Level', gp = gpar(fontsize = 9),rot = 90)
    bottom <- textGrob("TRMM bias corrected", gp = gpar(fontsize = 10))
    p2=arrangeGrob(img21,img22,img23,img24,nrow = 1,ncol = 4,bottom = bottom,left = left)
    
    left <- textGrob('CV Simulated 6-Hourly Acc Level', gp = gpar(fontsize = 9),rot = 90)
    bottom <- textGrob("TRMM bias corrected", gp = gpar(fontsize = 10))
    p3=arrangeGrob(img31,img32,img33,img34,nrow = 1,ncol = 4,bottom = bottom,left = left)
    
    left <- textGrob('CV Simulated 12-Hourly Acc Level', gp = gpar(fontsize = 9),rot = 90)
    bottom <- textGrob("TRMM bias corrected", gp = gpar(fontsize = 10))
    p4=arrangeGrob(img41,img42,img43,img44,nrow = 1,ncol = 4,bottom = bottom,left = left)
  }
  
  figure=grid.arrange(p1,p2,p3,p4,nrow=4)
  ggsave(file_save_name,figure,dpi=1200,units = 'cm',width =20 ,height =25 )
  
  figure
}


maps_plot=function(file1,file2,file3,file4,file5,file6,intervals,file_save_name){
  a=expression(alpha)
  l=expression(paste(lambda,'(1/hr)'))
  v=expression(paste(upsilon,'(hr)'))
  k=expression(kappa)
  phi=expression(phi)
  u=expression(paste(mu,'(mm/hr)'))
  
  
  
  c(df.map,legenda):=raster_to_df(file1,intervals)
  mapa1=plot_map(df.map,legenda,titulo = a)
  
  c(df.map,legenda):=raster_to_df(file2,intervals)
  mapa2=plot_map(df.map,legenda,titulo = l)
  
  c(df.map,legenda):=raster_to_df(file3,intervals)
  mapa3=plot_map(df.map,legenda,titulo = v)
  
  c(df.map,legenda):=raster_to_df(file4,intervals)
  mapa4=plot_map(df.map,legenda,titulo = k)
  
  c(df.map,legenda):=raster_to_df(file5,intervals)
  mapa5=plot_map(df.map,legenda,titulo = phi)
  
  c(df.map,legenda):=raster_to_df(file6,intervals)
  mapa6=plot_map(df.map,legenda,titulo = u)
  
  
  figure1=grid.arrange(arrangeGrob(mapa1,mapa2,mapa3,mapa4,mapa5,mapa6,nrow = 3))
  ggsave(file_save_name,figure1,dpi=1200,units = 'cm',width =20 ,height =25 )
  
}

plot_cdf=function(file,par,titule,language='English'){
  obs=data.frame(obs=nonzero(read.csv(file,sep = ',')$Rainfall.mm))
  sim=data.frame(sim=nonzero(precp_sim(as.numeric(par),dim(obs)[1],tscale = 3)))
  x=c(obs$obs,sim$sim)
  g=c(rep(1,length(obs$obs)),rep(2,length(sim$sim)))
  df=data.frame(x,g=factor(g))
  
  
  
  if (language=='Spanish'){
    ggplot(df,aes(x,colour=g))+stat_ecdf(geom = "point")+stat_ecdf(geom = "point")+
      labs(color="Legend",title = titule,x='Acumulado 3 horas (mm)',y='CDF')+theme(legend.margin=margin(t = 0, unit='cm'),legend.title=element_blank(),plot.title = element_text(hjust = 0.5),legend.position = c(0.8, 0.25))+
      scale_color_manual(labels = c("Observado", "Simulado"), values = c("red", "blue"))
  }else{
    ggplot(df,aes(x,colour=g))+stat_ecdf(geom = "point")+stat_ecdf(geom = "point")+
      labs(color="Legend",title = titule,x='Three hourly (mm)',y='CDF')+theme(legend.margin=margin(t = 0, unit='cm'),legend.title=element_blank(),plot.title = element_text(hjust = 0.5),legend.position = c(0.8, 0.25))+
      scale_color_manual(labels = c("Observed", "Synthetic"), values = c("red", "blue"))
  }
  
}





