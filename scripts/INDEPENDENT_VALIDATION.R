
#Importing the regionalization functionality

source('D:/Proyectos_GitHub/Bartlet-Lewis_Regionalization/src/RMBLRP.R')
source('D:/Proyectos_GitHub/Bartlet-Lewis_Regionalization/src/plotHelpers.R')


inde_gauge=read.csv("D:/Proyectos_GitHub/Bartlet-Lewis_Regionalization/data/ValidacionIndependiente/Validation_points/independent_gauges.csv",sep = ';')
names(inde_gauge)=c('x','y','gauge')
gauge_loc=points_wgs84(inde_gauge)
cv_maps=stack(list.files("D:/Proyectos_GitHub/Bartlet-Lewis_Regionalization/output/CV_maps/feb/",full.names = T,pattern = '.tif'))
gauge_par <-data.frame(raster::extract(cv_maps, gauge_loc))
names(gauge_par)=c('a','l','v','k','f','mx')


setwd("D:/Proyectos_GitHub/Bartlet-Lewis_Regionalization/data/ValidacionIndependiente/Validation_points/")

library(ggplot2)

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


#plot_cdf('FEB_PIU_03_1hr.csv',gauge_par[1,])
plot11=plot_cdf('FEB_PIU_02_3hr.csv',gauge_par[2,],'PIU_02')
plot12=plot_cdf('FEB_PIU_06_3hr.csv',gauge_par[3,],'PIU_06')
plot21=plot_cdf('FEB_PIU_07_3hr.csv',gauge_par[4,],'PIU_07')
plot22=plot_cdf('FEB_CHA_01_3hr.csv',gauge_par[5,],'CHA_01')
#plot_cdf('FEB_CHA_02_3hr.csv',gauge_par[6,])
plot31=plot_cdf('FEB_HUA_01_3hr.csv',gauge_par[7,],'HUA_01')
plot32=plot_cdf('FEB_HMT_01_3hr.csv',gauge_par[8,],'HMT_01')

figure=grid.arrange(arrangeGrob(plot11,plot12,plot21,plot22,plot31,plot32,nrow = 3))
ggsave("D:/Proyectos_GitHub/Bartlet-Lewis_Regionalization/output/img/independent_cdf.png",figure,dpi=1200,units = 'cm',width =20 ,height =25 )



plot11=plot_cdf('FEB_PIU_02_3hr.csv',gauge_par[2,],'PIU_02',language = 'Spanish')
plot12=plot_cdf('FEB_PIU_06_3hr.csv',gauge_par[3,],'PIU_06',language = 'Spanish')
plot21=plot_cdf('FEB_PIU_07_3hr.csv',gauge_par[4,],'PIU_07',language = 'Spanish')
plot22=plot_cdf('FEB_CHA_01_3hr.csv',gauge_par[5,],'CHA_01',language = 'Spanish')
#plot_cdf('FEB_CHA_02_3hr.csv',gauge_par[6,])
plot31=plot_cdf('FEB_HUA_01_3hr.csv',gauge_par[7,],'HUA_01',language = 'Spanish')
plot32=plot_cdf('FEB_HMT_01_3hr.csv',gauge_par[8,],'HMT_01',language = 'Spanish')

figure=grid.arrange(arrangeGrob(plot11,plot12,plot21,plot22,plot31,plot32,nrow = 3))
ggsave("D:/Proyectos_GitHub/Bartlet-Lewis_Regionalization/output/img/cdf_independiente.png",figure,dpi=1200,units = 'cm',width =20 ,height =25 )

