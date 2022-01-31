#Importing the regionalization functionality
source('D:/Proyectos_GitHub/Bartlet-Lewis_Regionalization/src/RMBLRP.R')
source('D:/Proyectos_GitHub/Bartlet-Lewis_Regionalization/src/plotHelpers.R')

#Gauges statistics
gauge_stats=read.csv('D:/Proyectos_GitHub/Bartlet-Lewis_Regionalization/data/gauge_stats.csv')
gauge_stats=kickOutliers(gauge_stats)
gauge_stats=filter_Neigbors(gauge_stats)
gauge_stats=gauge_stats[-c(78,137),]

#Model CV parameters
validation_parameters=read.csv('D:/Proyectos_GitHub/Bartlet-Lewis_Regionalization/output/CV_validation/CrossValidationParameters.csv')[-c(78,137),]
#validation_parameters=read.csv('D:/Proyectos_GitHub/Bartlet-Lewis_Regionalization/output/CV_parameters/parameters01.csv')
#Simulated Stats
simulated_stats=data.frame(gauge_stats[,c(1,2)],SimStats(validation_parameters))

dev.new()
par(mfrow=c(4,4)) 

labelsy=c(rep('CV Simulated 24-Hourly Acc Level',4),
          rep('CV Simulated 3-Hourly Acc Level',4),
          rep('CV Simulated 6-Hourly Acc Level',4),
          rep('CV Simulated 12-Hourly Acc Level',4))

titulos=c('Mean Rainfall (mm)','STDE Rainfall (mm)','Lag-1 Autocorrelation','Probability of 0 Rain')



for (i in 1:16){
  if (i %in% 1:4){
    if( i==1){
      plot(gauge_stats[[i+2]],simulated_stats[[i+2]],xlab="Observed",ylab='Simulated 24-Hourly Acc Level',main=titulos[i])
    }else{
      y=simulated_stats[[i+2]]
      plot(x=gauge_stats[[i+2]],y,xlab="Observed",ylab=NULL,main=titulos[i])
    }
  }else{
    if (i %in% c(5,9,13)){
      plot(x=gauge_stats[[i+2]],y=simulated_stats[[i+2]],xlab="TRMM bias corrected",ylab=labelsy[i])
    }else{
      y=simulated_stats[[i+2]]
      plot(x=gauge_stats[[i+2]],y,xlab="TRMM bias corrected",ylab=NULL)
    }
    
  }
  abline(coef = c(0,1))
}


################################################################
####GGPLOT PART##################################################
#################################################################
graficoVS=function(y,obs,sim){
  df=data.frame(y,obs,sim)
  names(df)=c('y','obs','sim')
  df=mutate(df,Location=factor(case_when(y>-19   & y<=-12.7 ~'Costa sur',
                                    y>-12.7 & y<=-6.4  ~ 'Costa central',
                                    y>-6.4  & y<=0     ~'Costa norte')))
  
  img=ggplot(df, aes(x = obs, y = sim,colour=Location))+geom_point(size=2, shape=20)+geom_abline(intercept = 0)
  leyenda=g_legend(img)
  img=img+theme(legend.position="none")+theme(axis.title.x=element_blank(),
                              
                                              axis.ticks.x=element_blank(),
                                              axis.title.y=element_blank(),
                                            
                                              axis.ticks.y=element_blank())
  list(img,leyenda)
}
names(simulated_stats)=names(gauge_stats)[1:18]


c(img11,legenda):=graficoVS(gauge_stats$y,gauge_stats$mean24,simulated_stats$mean24)
img11=img11+labs(title='Mean Rainfall (mm)')+theme(plot.title = element_text(hjust = 0.5))

img12=graficoVS(gauge_stats$y,gauge_stats$var24,simulated_stats$var24)[[1]]
img12=img12+labs(title='STDE Rainfall (mm)')+theme(plot.title = element_text(hjust = 0.5))

img13=graficoVS(gauge_stats$y,gauge_stats$autocov24,simulated_stats$autocov24)[[1]]
img13=img13+labs(title='Lag-1 Autocorrelation')+theme(plot.title = element_text(hjust = 0.5))

img14=graficoVS(gauge_stats$y,gauge_stats$dryperiod24,simulated_stats$dryperiod24)[[1]]
img14=img14+labs(title='Probability of 0 Rain')+theme(plot.title = element_text(hjust = 0.5))

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

figure=grid.arrange(p1,p2,p3,p4,nrow=4)
ggsave("D:/Proyectos_GitHub/Bartlet-Lewis_Regionalization/output/img/cv_stats.png",figure,dpi=1200,units = 'cm',width =20 ,height =25 )


############################################################
####################CFD#####################################
############################################################

































