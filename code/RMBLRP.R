library(extRemes)
library(raster)
library("HyetosMinute")
library(geosphere)

####################################
###Model Statistics#################
####################################
#Mean
meanMBLRPM<-function(a,l,v,k,f,mx,h=1) {
  x<-(h*l*mx*v*(1+k/f))/(a-1) 
  return(x)
}
#Variance
varMBLRPM<-function(a,l,v,k,f,mx,h=1) {
  A<-(2*l*(1+k/f)*(mx^2)*(v^a))/((f^2)*((f^2)-1)*(a-1)*(a-2)*(a-3))
  B<-(2*(f^2)-2+k*f)*(f^2)*((a-3)*h*(v^(2-a))-(v^(3-a))+((v+h)^(3-a)))
  C<-k*(f*(a-3)*h*(v^(2-a))-(v^(3-a))+((v+f*h)^(3-a)))
  D<-A*(B-C)
  return(D)
}
#Covariance
covarMBLRPM<-function(a,l,v,k,f,mx,h=1,lag=1) {
  A<-(l*(1+k/f)*(mx^2)*(v^a))/((f^2)*((f^2)-1)*(a-1)*(a-2)*(a-3))
  B<-(2*(f^2)-2+k*f)*(f^2)*(((v+(lag+1)*h)^(3-a))-2*((v+lag*h)^(3-a))+((v+(lag-1)*h)^(3-a)))
  C<-k*(((v+(lag+1)*h*f)^(3-a))-(2*((v+h*lag*f)^(3-a)))+((v+(lag-1)*h*f)^(3-a))) 
  D<-A*(B-C)
  return(D)
}
#Dry probabilities
pdrMBLRPM<-function(a,l,v,k,f,h=1) {
  mt<-((1+(f*(k+f))-(0.25*f*(k+f)*(k+4*f))+((f/72)*(k+f)*(4*(k^2)+27*k*f+72*(f^2))))*v)/(f*(a-1))
  G00<-((1-k-f+1.5*k*f+(f^2)+0.5*(k^2))*v)/(f*(a-1))
  A<-(f+(k*(v/(v+(k+f)*h))^(a-1)))/(f+k)
  D<-exp(l*(-h-mt+G00*A)) 
  return(D)
}

####################################
#######Optimization Function########
####################################

MBLRPM=function(mean24,var24,cov24lag1,pdr24,var3,var6,var12,var18,Lmin,Lmax){
  
  #Objective function
  fopt <- function(x) {
    a<-x[1];l<-x[2];v<-x[3];k<-x[4];f<-x[5];mx<-x[6]
    w1=1;w2=1;w3=1;w4=1;w5=1;w6=1;
    
    
    S3<-w2*((varMBLRPM(a,l,v,k,f,mx,h=3)/var3)-1)^(2)

    S6<-w2*((varMBLRPM(a,l,v,k,f,mx,h=6)/var6)-1)^(2)
    
    S12<-w2*((varMBLRPM(a,l,v,k,f,mx,h=12)/var12)-1)^(2)
    
    S18<-w2*((varMBLRPM(a,l,v,k,f,mx,h=18)/var18)-1)^(2)
    
    S24 <- w1*((meanMBLRPM(a,l,v,k,f,mx,h=24)/mean24)-1)^(2)+ w2*((varMBLRPM(a,l,v,k,f,mx,h=24)/var24)-1)^(2)+ w3*((covarMBLRPM(a,l,v,k,f,mx,h=24,lag=1)/cov24lag1)-1)^(2)+w4*((pdrMBLRPM(a,l,v,k,f,h=24)/pdr24)-1)^(2)
    
    
    S<-S24+S3+S6+S12+S18
    
    if(is.infinite(S)) {S<-10^8}
    if(is.na(S)) {S<-10^8} 
    return(S) 
  }

  # set the interior and exterior parameters bounds
  xmin <- Lmin
  xmax <- Lmax
  xlow <- Lmin
  xup <- c(50,20,runif(1,min = 1.8, max = 2.2),runif(1,min = 0.05, max = 0.09),runif(1,min = 1.8, max = 2.2),runif(1,min = 8, max = 12))

  modecal <- eas(n=6,m=30,xmin,xmax,xlow,xup,fn=fopt,maxeval=5000,ftol=1.e-10,ratio=0.99,pmut=0.95, beta=2,maxclimbs=5)
  modecal
  a<-modecal$bestpar[[1]];
  l<-modecal$bestpar[[2]];
  v<-modecal$bestpar[[3]];
  k<-modecal$bestpar[[4]];
  f<-modecal$bestpar[[5]];
  mx<-modecal$bestpar[[6]]
  # In order to use the derived parameters in the functions of HyetosR 
  # as well as in the classic version of Hyetos,please be sure that 
  # for parameters mx and sx the length units are millimeters (mm) 
  # and for parameters l, v, mx and sx the time units are days (d). 
  # For this reason, make the following unit conversions:
  
  #checar
  #l<-l*24 
  #v<-v/24 
  #mx<-mx*24
  
  # parameter set for implementation in HyetosR functions
  par <- c(a=a,l=l,v=v,k=k,f=f,mx=mx) 
  
  par
}


nearpoints=function(mdist,radio=60000){
  #Return the index of the closest gauge station to each station
  #mdist = matrix of distance in meters
  #radio is the maximum distance allowed among station
  distance=list()
  for (i in 1:dim(mdist)[1]){
    values=sort(mdist[i,])
    values=tail(values, -1)
  
    condition=values[values<radio]
    near=numeric(length(condition))
    
    for (j in 1:length(condition)){
      near[j]=which(mdist[i,]==condition[j])[1]
    }
    
    distance[[i]]=near
  }
  distance
}

##################################
####Functions for filtering data #
####before regionalization########
##################################

kickOutliers=function(data){
  #delete neighbors based on variance outliers
  data_help=data
  coordinates(data_help) <- ~x+y
  mdist <- distm(data_help,fun = distHaversine)
  neighbors=nearpoints(mdist)
  outlier=c()
  for (station in 1:dim(data)[1]){
    sub=data[c(station,neighbors[[station]]),]
    outvals=which(sub$var24 %in% boxplot(sub$var24)$out)
    outlier=c(outlier,outvals)
  }
  
  idx=unique(outlier)
  data[-idx,]
}

filter_Neigbors=function(data,min_n=3,radio=60000){
  #Return the statins with at least n neighbors
  #data= location of the stations
  #min_n= number of minumun neighbors required
  data_help=data
  coordinates(data_help) <- ~x+y
  mdist <- distm(data_help,fun = distHaversine)
  neighbors=nearpoints(mdist,radio=radio)
  stations=data[lengths(neighbors)>=min_n,]

  stations
}


################################################
#Cross Validations using Inverse Distance Weigth
################################################
idwCV=function(data,parameter='a',power=2){
  x=data
  coordinates(x) <- ~x+y
  proj4string(x)='+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0'
  mdist <- distm(x) #the answer is in meters
  
  crossValidated=numeric()
  for (i in 1:dim(data)[1]){
    info=data[[parameter]][-i]
    denominador=sum((1/mdist[i,-i])^power)
    crossValidated[i]=sum(info/mdist[i,-i]^power/denominador)
  }
  
  rr=cbind(data[c('x','y')],'var1.pred'=crossValidated,'observed'=data[[parameter]])
  rr$residual=rr$observed-rr[['var1.pred']]
  rr
}


####################################
#######Repetitive Cross Validation##
####################################

repetitiveCV=function(times=1,data,Stats,Lmin,Lmax){
  #data contains the intial parameter estimation
  #stats is the rainfall statistics
  
  #Estaciones cambiantes de intervalos
  iter=1
  range=1:dim(data)[1]
  
  while (length(range)>0.05*length(range)){
    print(paste("Number of cross validation iteration",as.character(iter)))
    iter=iter+1
    
    
    data_help=data
    coordinates(data_help) <- ~x+y
    mdist <- distm(data_help,fun = distHaversine)
    vecinos=nearpoints(mdist)
    
    formulas=c(formula('a~1'),formula('l~1'),formula('v~1'),formula('k~1'),formula('f~1'),formula('mx~1'))
    
    mistakes=c()
    for (station in range){
      if (station%%50==0){
        print(paste0('Search region modification ...',as.character(station)))
      }
      
      for (k in 1:6){#parameters, 6 in total
        if (station%%50==0){
          print(paste('Checking parameter',k))
        }
        
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
    }
    
    mistakes=unique(mistakes)
    range=mistakes #"range is changing "
    
    n=dim(Stats)[1]
    parameters=data[,3:8]#matrix(data=NA,nrow =n,ncol = 6)
    print("number of station to correct: ")
    print(length(mistakes))
    for (i in mistakes){
      momentos=Stats[i,]
      
      mean24 = momentos$mean24
      var24 = momentos$var24
      cov24lag1 =momentos$autocov24
      pdr24=momentos$dryperiod24
      var3=momentos$var3
      var6=momentos$var6
      var12=momentos$var12
      var18=momentos$var18
      
      par=MBLRPM(mean24,var24,cov24lag1,pdr24,var3,var6,var12,var18,Lmin[,i],Lmax[,i])
      
      parameters[i,]=par
    }
    
    
    parameters=cbind(Stats[,1:2],parameters)
    names(parameters)=c('x','y','a','l','v','k','f','mx')
    data=parameters#check
    
    if (iter==80){
      break
    }
  }
  
  parameters
}

##########################################
###### Run Function#######################
##########################################
run=function(rain_stats,path,iterations=5){
  #rain_stats: contains the rainfall statistics
  #path: where to save the results
  #Maskshape: Shape form of the final results 
  
  
  n=dim(rain_stats)[1]

  #Maximum and minimum search parameters space
  Lmin=matrix(c(1,0.001,0.001,0.001,0.0854,1),nrow = 6,ncol = n)
  Lmax=matrix(c(4,0.1,0.1,0.1,0.1,20),nrow=6,ncol=n)
  
  
  print('Calculating the initial parameters ...')
  #Initial parameters 
  parameters0=matrix(data=NA,nrow =n,ncol = 6)
  
  
  for (i in 1:n){
    momentos=rain_stats[i,]

    if (i%%20==0){
      print(paste('Parameters of','gauge',as.character(i)))
    }
    
    mean24 = momentos$mean24
    var24 = momentos$var24
    cov24lag1 =momentos$autocov24
    pdr24=momentos$dryperiod24
    var3=momentos$var3
    var6=momentos$var6
    var12=momentos$var12
    var18=momentos$var18
    par=MBLRPM(mean24,var24,cov24lag1,pdr24,var3,var6,var12,var18,Lmin[,i],Lmax[,i])

    parameters0[i,]=par

    
  }


  parameters=cbind(rain_stats[,1:2],parameters0)
  names(parameters)=c('x','y','a','l','v','k','f','mx')
  
  print('Reptitive Cross Validations ...')
  CV_parameters=repetitiveCV(times = iterations,parameters,rain_stats,Lmin = Lmin ,Lmax = Lmax)
  
  #saving the initial parameters
  
  write.table(CV_parameters,paste0(path,'parameters01.csv'),sep = ',',row.names = F)
}