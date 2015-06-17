#RUN GBM 

#library(gbm)

runGBM <- function(data=filtered_control_data, 
                   numFold=10, 
                   n.trees=100, #hyper-parameter
                   interaction.depth=1, #hyper-parameter
                   n.minobsinnode=10, #hyper-parameter
                   shrinkage = 0.001, #hyper-parameter
                   verbose=FALSE){
  
  #set.seed(1975)
  folds.1 <- cut(sample(1:length(which(targetVar==1))), breaks=numFold, labels=FALSE)
  folds.0 <- cut(sample(1:length(which(targetVar==0))), breaks=numFold, labels=FALSE)
  
#   trainIndex.1 <- sample(which(targetVar==1), length(which(targetVar==1))*0.8)
#   trainIndex.0 <- sample(which(targetVar==0), length(which(targetVar==0))*0.8)
#   testIndex.1 <- setdiff(which(targetVar==1), trainIndex.1)
#   testIndex.0 <- setdiff(which(targetVar==0), trainIndex.0)
#   trainIndex <- c(trainIndex.1, trainIndex.0)
#   testIndex <- c(testIndex.1, testIndex.0)
# 

  aucVals <- c()
  bestIter <- c()
  for(ii in 1:numFold){
    testIndices <- c(which(folds.1==ii, arr.ind=TRUE),which(folds.0==ii, arr.ind=TRUE))
    
    trainLabels <- targetVar[-testIndices]
    valLabels <- targetVar[testIndices]
    
    train_X <- control_data[-testIndices,]
    
    val_X <- control_data[testIndices,]
    #don't include the id column, hence -1
    set.seed(1972)
    gbmFit <- gbm.fit(x=train_X[-1], #don't include the id column, hence -1
                      y=trainLabels,
                      distribution = "bernoulli",
                      n.trees = n.trees , #hyper-parameter
                      interaction.depth = interaction.depth, # hyper-parameter
                      n.minobsinnode = n.minobsinnode, # hyper-parameter
                      shrinkage = shrinkage, # hyper-parameter
                      bag.fraction = 0.5, #use random-seed above:reproducability
                      nTrain = floor(nrow(train_X)*0.8),
                      verbose=verbose)
    
    best.iter <- gbm.perf(gbmFit, method="test", plot.it=FALSE)
    
    #don't include the id column, hence -1
    gbm_pred <- predict(gbmFit,
                        newdata=val_X[-1],
                        n.trees = best.iter,
                        type="response")
    
      
    aucVals[ii] <- c(aucVals, auc(valLabels, gbm_pred))
    bestIter[ii] <- c(bestIter, best.iter)
  }
  retVals = list('mean_aucVals'=mean(aucVals), 'best.iter'= mean(bestIter))
  return(retVals)
}


