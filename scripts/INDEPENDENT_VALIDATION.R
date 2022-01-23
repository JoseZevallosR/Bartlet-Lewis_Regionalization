
#Importing the regionalization functionality

source('D:/Proyectos_GitHub/Bartlet-Lewis_Regionalization/src/RMBLRP.R')


inde_gauge=read.csv("D:/Proyectos_GitHub/Bartlet-Lewis_Regionalization/data/ValidacionIndependiente/Validation_points/independent_gauges.csv",sep = ';')
names(inde_gauge)=c('x','y','gauge')
gauge_loc=points_wgs84(inde_gauge)
cv_maps=stack(list.files("D:/Proyectos_GitHub/Bartlet-Lewis_Regionalization/output/CV_maps/",full.names = T))
gauge_par <-data.frame(raster::extract(cv_maps, gauge_loc))
names(gauge_par)=c('a','l','v','k','f','mx')


setwd("D:/Proyectos_GitHub/Bartlet-Lewis_Regionalization/data/ValidacionIndependiente/Validation_points/")


#par=c(6.230970662,	0.031663573,	2.099945979,	0.079368555,	0.085400002,	12.743713211)
sim=precp_sim(as.numeric(gauge_par[6,]),1000,tscale = 3)
#sim=precp_sim(par,10000,tscale = 1)

obs=read.csv('FEB_CHA_02_3hr.csv',sep = ',')

ecdf1 <- ecdf(nonzero(obs$Rainfall.mm))
ecdf2 <- ecdf(nonzero(sim))

plot(ecdf2, verticals=TRUE, do.points=T, col='orange')
plot(ecdf1, verticals=TRUE, do.points=T, add=TRUE)


par=as.numeric(gauge_par[6,])
meanMBLRPM(a = par[1],l = par[2],v = par[3],k = par[4],f = par[5],mx = par[6],h = 3)
mean(obs$Rainfall.mm,na.rm=T)
sim=precp_sim(as.numeric(gauge_par[7,]),1000,tscale = 3)
mean(sim)

pdrRPBLRPM(a = par[1],l = par[2],v = par[3],k = par[4],f = par[5],h = 3)
pdrMBLRPM(a = par[1],l = par[2],v = par[3],k = par[4],f = par[5],h = 11)
