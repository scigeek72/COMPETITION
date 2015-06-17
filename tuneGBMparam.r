##improved code for hyper-parameter tuning 
aucVals <- list()
#n.minobs = list()
#depth <- list()
#shrinkage <- list()
bestIter <- list()
N <- 3 #cv repeated N times


## better way to tune is to use expand.grid function from the base utils package
param_grid <- list()
param_grid$n.trees = c(1000,2000)
param_grid$n.minobsinnode = seq(20,50, by=10)
param_grid$interaction.depth = 1:7
param_grid$shrinkage = c(0.0001,0.001)

datagrid <- expand.grid( param_grid )

## use the apply function to apply runGBM at each row of datagrid
## run it multiple times

results_df <- datagrid

for(jj in 1:N){
  A <- apply( datagrid,1, function(z){ 
    runGBM(n.trees = z["n.trees"], n.minobsinnode=z["n.minobsinnode"],
           interaction.depth=z["interaction.depth"], 
           shrinkage=z["shrinkage"] ) } )
  
  temp_aucVals <- c()
  temp_bestIter <- c()
  for(i in 1:nrow(datagrid)){
    temp_aucVals <- append(temp_aucVals, A[[i]]$mean_aucVals)
    temp_bestIter <- append(temp_bestIter, A[[i]]$best.iter)
  }
  
  results_df = cbind(results_df,aucVals = temp_aucVals,best.iter = temp_bestIter)
  
}

