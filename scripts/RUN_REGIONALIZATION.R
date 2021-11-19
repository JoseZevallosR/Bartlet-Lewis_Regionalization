
#Importing the regionalization functionality

source('D:/Proyectos_GitHub/Bartlet-Lewis_Regionalization/src/RMBLRP.R')

#Mixed stats from gauge stations and corrected TRMM
gauge_stats=read.csv('D:/Proyectos_GitHub/Bartlet-Lewis_Regionalization/data/gauge_stats.csv')
gauge_stats=kickOutliers(gauge_stats)
gauge_stats=filter_Neigbors(gauge_stats)
dim(gauge_stats)

maps=run(rain_stats=gauge_stats,path="D:/Proyectos_GitHub/Bartlet-Lewis_Regionalization/output/CV_parameters/",iterations=10)

################
#check clusters#
################
data=maps
data_help=data
coordinates(data_help) <- ~x+y
proj4string(data_help)='+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0'
mdist <- distm(data_help,fun = distHaversine)
vecinos=nearpoints(mdist)
idx=clusterIDX(data)

for (clouster in idx){
  print(data[c(clouster,vecinos[[clouster]]),])
}

