source('D:/Proyectos_GitHub/Bartlet-Lewis_Regionalization/src/RMBLRP.R')
source('D:/Proyectos_GitHub/Bartlet-Lewis_Regionalization/src/plotHelpers.R')
library(ggplot2)


par=c(2.38,0.036,1.551,0.078,0.085,1.870)

estacion1=data.frame(x=1:1000,pp=precp_sim(as.numeric(par),1000,tscale = 1))
estacion3=data.frame(x=1:1000,pp=precp_sim(as.numeric(par),1000,tscale = 6))
estacion5=data.frame(x=1:1000,pp=precp_sim(as.numeric(par),1000,tscale = 12))
estacion8=data.frame(x=1:1000,pp=precp_sim(as.numeric(par),1000,tscale = 24))


agg1=ggplot(data=estacion1,aes(x=x,y=pp))+geom_line()+ylab('Precipitación 1h (mm)') +xlab(element_blank())

agg3=ggplot(data=estacion3,aes(x=x,y=pp))+geom_line()+ylab('Precipitación 6h (mm)') +xlab(element_blank())
  
agg5=ggplot(data=estacion5,aes(x=x,y=pp))+geom_line()+ylab('Precipitación 12h (mm)') +xlab(element_blank())

agg8=ggplot(data=estacion8,aes(x=x,y=pp))+geom_line()+ylab('Precipitación 24h (mm)') +xlab(element_blank())


dev.new()
grid.arrange(arrangeGrob(agg1,agg3,agg5,agg8,nrow = 4))
