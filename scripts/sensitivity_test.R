
library("HyetosMinute")
gauge_stats=read.csv('D:/Proyectos_GitHub/Bartlet-Lewis_Regionalization/data/feb_gauge_stat.csv')

par = c(-6.91,2.522550686,0.015,0.05,0.1,0.01,20,1/24)

cost_function = function(mean_obs,par){
  mean_sim = HyetosMinute::meanRPBLRPM(a = par[1],l = par[2]
                                       ,v = par[3],k = par[4],
                                       f = par[5],mx = par[6], h = par[7])
  return(mean_sim-mean_obs)**2
}

epsilon = 10**-6

sensitivity = matrix(data = NA,nrow = 5, ncol = 6)
estacion = gauge_stats[1,]

mean_obs_values = as.numeric(estacion[c(3, 7, 11, 15, 19)])

hours = c(24,3,6,12,18)

for (k in 1:length(mean_obs_values)) {  # Iteramos sobre los valores de observación
  mean_obs = mean_obs_values[k]
  
  for (i in 1:6) {
    par[7] = hours[k]
    par_mod = par
    par_mod[i] = par_mod[i] + epsilon
    sensitivity[k, i] = (cost_function(mean_obs = mean_obs, par = par_mod) - 
                           cost_function(mean_obs = mean_obs, par = par)) / epsilon
  }
}

# Calculate the mean sensitivity for each parameter
mean_sensitivity <- colMeans(sensitivity)

# Names for the parameters
param_names <- c("Param 1", "Param 2", "Param 3", "Param 4", "Param 5", "Param 6")

# Create a horizontal bar plot
barplot(abs(mean_sensitivity), 
        names.arg = param_names, 
        horiz = TRUE, 
        las = 1,  # Makes the parameter names horizontal
        col = "lightblue", 
        xlab = "Sensitivity", 
        main = "Parameter Sensitivity")


# Create a horizontal bar plot
barplot(abs(sensitivity[,6]),
        names.arg = c('24','3','6','12','18'), 
        horiz = TRUE, 
        las = 1,  # Makes the parameter names horizontal
        col = "lightblue", 
        xlab = "Sensitivity", 
        main = "Parameter Sensitivity mu")
