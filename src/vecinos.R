####################################
#######Repetitive Cross Validation##
####################################

repetitiveCV=function(times=1,data,Stats,Lmin,Lmax,fun=MBLRPM){
  #data contains the intial parameter estimation
  #stats is the rainfall statistics
  
  #Estaciones cambiantes de intervalos
  
  parameters=data[,3:8]
  data_help=data
  coordinates(data_help) <- ~x+y
  proj4string(data_help)='+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0'

  mdist <- distm(data_help,fun = distHaversine)
  vecinos=nearpoints(mdist)

  range=clusterIDX(data)
  old_error=rep(100,length(range))
  current_error=numeric(length(range))
  for (iter in 1:times){
    print(paste("Number of cross validation iteration",as.character(iter)))
    k_cluster=1
    for (station in range){
      print(paste('cluster :',as.character(k_cluster),'/',as.character(length(range))))
      mistakes=c()
      for (k in 1:6){#parameters, 6 in total
        x <- idwCV(data[c(station,vecinos[[station]]),],parameter= c('a','l','v','k','f','mx')[k],power=2)
        #Checking region error
        sub=x[c('x','y','var1.pred','observed','residual')]
        sub$porcentaje=abs(sub$residual)*100/sub$observed
        sub$residual=NULL
        sub$location=c(station,vecinos[[station]])
        
        #median(sub$porcentaje)
        good_neighbors=subset(sub,sub$porcentaje<30)
        wrong_neigbors=subset(sub,sub$porcentaje>=30)
        
        if (dim(wrong_neigbors)[1]!=0 & dim(good_neighbors)[1]!=0 ){
          for (fix_id in wrong_neigbors$location){
            Lmin[k,fix_id]=min(good_neighbors$var1.pred)
            Lmax[k,fix_id]=max(good_neighbors$var1.pred)
            
            mistakes=c(mistakes,fix_id) #add the wrong stations
          }
        }
      }
      n_parameters=length(mistakes)
      mistakes=unique(mistakes)
      
      if (length(mistakes)==0){
              current_error[k_cluster]=0
              print('No parameters to correct')
      }else{
        current_error[k_cluster]=round(n_parameters*100/(6*length(sub$location)),2)
        if(old_error[k_cluster]<current_error[k_cluster]){
              print('Using old parameter with lower error')
              print(paste("Incorrect parameters in the cluster:",as.character(n_parameters),'/',as.character(6*length(sub$location)),' (',as.character(old_error[k_cluster]),'%)'))
              parameters[mistakes,]=old_parameters[mistakes,]
              old_error[k_cluster]=old_error[k_cluster]
          }else{
              
              print(paste("Incorrect parameters in the cluster:",as.character(n_parameters),'/',as.character(6*length(sub$location)),' (',as.character(current_error[k_cluster]),'%)'))
              n.cores <- parallel::detectCores() - 1
              #create the cluster
              my.cluster <- parallel::makeCluster(
                n.cores, 
                type = "PSOCK"
              )
              #register it to be used by %dopar%
              doParallel::registerDoParallel(cl = my.cluster)

              parameters[mistakes,]=t(matrix(foreach(
                  i=mistakes,
                  .combine = 'c', 
                  .packages = "HyetosMinute"
                ) %dopar% {
                momentos=Stats[i,]
                
                mean24 = momentos$mean24
                var24 = momentos$var24
                cov24lag1 =momentos$autocov24
                pdr24=momentos$dryperiod24
                var3=momentos$var3
                var6=momentos$var6
                var12=momentos$var12
                var18=momentos$var18
                
                par=fun(mean24,var24,cov24lag1,pdr24,var3,var6,var12,var18,Lmin[,i],Lmax[,i])
                
                return(par)
                },nrow = 6,ncol = length(mistakes)))
              parallel::stopCluster(cl = my.cluster) #closing the cluster 
          

              old_error[k_cluster]=current_error[k_cluster]
          }
      }

      
      k_cluster=k_cluster+1 #counting clusters
      
    }  
      #parameters=cbind(Stats[,1:2],parameters)
      names(parameters)=c('a','l','v','k','f','mx')#c('x','y','a','l','v','k','f','mx')
      old_parameters=data[,3:8]
      
      data[,3:8]=parameters#check
  }
  
  data
}