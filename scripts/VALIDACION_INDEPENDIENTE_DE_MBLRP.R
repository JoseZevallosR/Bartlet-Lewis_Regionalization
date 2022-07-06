
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


plot11=plot_cdf('FEB_PIU_02_3hr.csv',gauge_par[2,],'PIU_02')
plot12=plot_cdf('FEB_PIU_06_3hr.csv',gauge_par[3,],'PIU_06')
plot21=plot_cdf('FEB_PIU_07_3hr.csv',gauge_par[4,],'PIU_07')
plot22=plot_cdf('FEB_CHA_01_3hr.csv',gauge_par[5,],'CHA_01')
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