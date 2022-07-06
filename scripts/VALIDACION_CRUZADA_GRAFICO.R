#Importing the regionalization functionality
source('D:/Proyectos_GitHub/Bartlet-Lewis_Regionalization/src/RMBLRP.R')
source('D:/Proyectos_GitHub/Bartlet-Lewis_Regionalization/src/plotHelpers.R')

#Gauges statistics
gauge_stats=read.csv('D:/Proyectos_GitHub/Bartlet-Lewis_Regionalization/data/gauge_stats_feb_filtered.csv')


#Model CV parameters
validation_parameters=read.csv('D:/Proyectos_GitHub/Bartlet-Lewis_Regionalization/output/CV_validation/CrossValidationParameters2.csv',sep =',')

#Simulated Stats
simulated_stats=data.frame(gauge_stats[,c(1,2)],SimStats(validation_parameters[,-c(1,2)]))

################################################################
####GGPLOT PART##################################################
#################################################################

names(simulated_stats)=names(gauge_stats)[1:18]
file_save_name="D:/Proyectos_GitHub/Bartlet-Lewis_Regionalization/output/img/cv_stats_february.png"
plot_comparison(gauge_stat = gauge_stat,simulated_stats = simulated_stats,file_save_name = file_save_name,language = 'English')

file_save_name="D:/Proyectos_GitHub/Bartlet-Lewis_Regionalization/output/img/cv_estadisticos_febrero.png"
plot_comparison(gauge_stat = gauge_stat,simulated_stats = simulated_stats,file_save_name = file_save_name,language = 'Spanish')