iterations <- 1
meanValues <- list()
Scores <- list()
for(i in 1:iterations){
  numFold <- 2
  #create folds
  #set.seed(1972)
  FOLDS <- createFolds(size = nrow(raw.train),shuffle = TRUE,K = numFold)
  
  folds <- FOLDS$Folds
  shuffledIndex <- FOLDS$Index
  
  #prepare data
  
  newTrain <- raw.train[shuffledIndex,-c(1,3580:3585)]
  labels <- raw.train[shuffledIndex,3581:3585]
  
  
  
  #note number of folds is 5
  
  for(ii in 1:numFold){
    cat("Begining fold:",ii,"=========\n")
    testIndices <- which(folds == ii, arr.ind = TRUE)
    trainLabels <- labels[-testIndices,]
    valLabels <- labels[testIndices,]
    
    new.xVal <- newTrain[testIndices, ]
    new.xTrain <- newTrain[-testIndices,]
    
    cat("\n")
    cat("Building SVM model==========\n")
    #right now, we will not run it as we have the models
    svm.model <- runTunedModel(C = 100
                               , Gamma = 10^(-4)
                               , data = new.xTrain
                               , response = trainLabels)
    
    
    svm.test.pred <- myPrediction(modelName = svm.model
                                  , testData = new.xVal)
    svm.train.pred <- myPrediction(modelName = svm.model
                                   , testData = new.xTrain)
    
    cat("SVM done=============\n")
    
    cat("Building GBM model========\n")
#   # right now, we will not run it as we have the models
    GBM <- list()
    count = 1
    for(sp in soil_prop){
      GBM[[count]] <- gbm.fit(x=new.xTrain
                              , y=trainLabels[,sp]
                              , distribution="gaussian"
                              , n.trees = best.iter[count]
                              , nTrain = floor(nrow(xTrain)*0.8))
      
      count = count + 1
  
   }
    
    
    for(count in 1:5){
      if(count == 1){
        gbm.test.pred <- data.frame(X = rep(1,nrow(valLabels)))
        gbm.train.pred <- data.frame(X = rep(1, nrow(trainLabels)))
      }
      
      gbm.test.pred <- cbind(gbm.test.pred, zz = predict(GBM[[count]]
                                             , newdata = new.xVal
                                             , type = "response"
                                             , n.trees = best.iter[count]))
      gbm.train.pred <- cbind(gbm.train.pred,
                              zz = predict(GBM[[count]]
                                           , newdata = new.xTrain
                                           , type = "response"
                                           , n.trees = best.iter[count]))
      
      
    }
    gbm.test.pred[,1] <- NULL
    names(gbm.test.pred) <- soil_prop
    gbm.train.pred[,1] <- NULL
    names(gbm.train.pred) <- soil_prop
   
    cat("GBM done=============\n")
  
    cat("\n")
    cat("Building Random Forest (RF) Model=============\n")
    rf <- list()
    count = 1
    for(sp in soil_prop){
      rf[[count]] <- randomForest(x=new.xTrain
                                  ,y=trainLabels[,sp]
                                  ,ntree = 100)
      count = count + 1
                                                    
    }
  
    for(count in 1:5){
        if(count == 1){
          rf.test.pred <- data.frame(X=rep(1,nrow(new.xVal)))
          rf.train.pred <- data.frame(X=rep(1,nrow(new.xTrain)))
        }
        rf.test.pred <- cbind(rf.test.pred, zz = predict(rf[[count]], new.xVal))
        rf.train.pred <- cbind(rf.train.pred, zz = predict(rf[[count]], new.xTrain))
        
    }
    rf.test.pred[,1] <- NULL
    names(rf.test.pred) <- soil_prop
    rf.train.pred[,1] <- NULL
    names(rf.train.pred) <- soil_prop
  
    cat("RF done===============\n")
    cat("==================================================\n")
  
    cat("Begining nearest neighboer........................\n")
    knn.model <- list()
    knn.t.model <- list()
    count = 1
    for(sp in soil_prop){
      knn.model[[count]] <- knn.reg(train = new.xTrain
                                    , test = new.xVal
                                    , y = trainLabels[,sp]
                                    , k = 4)
      knn.t.model[[count]] <- knn.reg(train = new.xTrain
                                      , y = trainLabels[,sp]
                                      , k = 4)
      
      if(count == 1){
        knn.test.pred <- data.frame(X = rep(1,nrow(valLabels)))
        knn.train.pred <- data.frame(X = rep(1, nrow(trainLabels)))
      }
      knn.test.pred <- cbind(knn.test.pred, zz = knn.model[[count]]$pred)
      knn.train.pred <- cbind(knn.train.pred, zz = knn.t.model[[count]]$pred)
      count = count + 1
    }
    
    knn.test.pred[,1] <- NULL
    names(knn.test.pred) <- soil_prop
    knn.train.pred[,1] <- NULL
    names(knn.train.pred) <- soil_prop
    
  
    cat("NN done..........................................\n")
  
    cat("Combining the predictions for each response variables\n")
    Z = list()
    for(i in 1:5){
      Z[[i]] = cbind(svm=svm.test.pred[,i]
                     , rf = rf.test.pred[,i]
                     , knn = knn.test.pred[,i])
    }
    tZ = list()
    for(i in 1:5){
      tZ[[i]] = cbind(svm=svm.train.pred[,i] 
                     , rf = rf.train.pred[,i]
                     , knn = knn.train.pred[,i])
    }
    
  
    cat("Ridge on Z to find weights\n")
    count = 1
    glmnet.model <- list()
    for(i in 1:5){
      glmnet.model[[i]] <- cv.glmnet(x = as.matrix(tZ[[i]])
                                , y = trainLabels[,i]
                                , alpha = 0)
    }
    
    for(i in 1:5){
      if(i == 1){glmnet.test.pred <- data.frame(X = rep(1,nrow(Z[[1]])))}
      glmnet.test.pred <- cbind(glmnet.test.pred
                                , zz = predict(glmnet.model[[i]]
                                                  , newx = as.matrix(Z[[i]])
                                                  , type = "response"
                                                  , s = "lambda.min"))
    }
    glmnet.test.pred[,1] <- NULL
    names(glmnet.test.pred) <- soil_prop
    cat("End GLMNET WEIGHT calculations==========\n")
    cat("\n")
#     cat("Computing ridge-coeff\n")
    ridgeCoeff <- list()
    train_ridgeCoeff <- list()
    for(i in 1:5){
      ridgeCoeff[[i]] <- computeMyBetaRidge(Z=Z[[i]],lambda = 0.001,y = valLabels[,i])
      train_ridgeCoeff[[i]] <- computeMyBetaRidge(Z=tZ[[i]]
                                                  ,lambda = 0.001
                                                  , y = trainLabels[,i])
    }
    cat("Computing Z%*%ridgeCoeff: The combined predictions\n")
    for(i in 1:5){
      if(i == 1){
        ridge.test.pred <- data.frame(X = rep(1, nrow(Z[[i]])))
        ridge.train.pred <- data.frame(X = rep(1, nrow(tZ[[i]])))
      }
      ridge.test.pred <- cbind(ridge.test.pred, zz = Z[[i]]%*%train_ridgeCoeff[[i]])
      ridge.train.pred <- cbind(ridge.train.pred
                                , zz = tZ[[i]]%*%train_ridgeCoeff[[i]])
    }
    ridge.test.pred[,1] <- NULL
    names(ridge.test.pred) <- soil_prop
    ridge.train.pred[,1] <- NULL
    names(ridge.train.pred) <- soil_prop
  
  
    cat("\n")
    cat("Computing the scores\n")
    if(ii == 1){newScores <- matrix(0, nrow = length(unique(folds)), ncol = 2)}
    
    newScores[ii,1] <- MCRMSE(pred = svm.test.pred, obs = valLabels)
#     newScores[ii,2] <- MCRMSE(pred = gbm.test.pred, obs = valLabels)
#     newScores[ii,3] <- MCRMSE(pred = (svm.test.pred*3 + gbm.test.pred)/4
#                               , obs =valLabels)
#     newScores[ii,4] <- MCRMSE(pred = (svm.test.pred*2 + gbm.test.pred)/3
#                               , obs = valLabels)
#     newScores[ii,5] <- MCRMSE(pred = (svm.test.pred*4 + gbm.test.pred)/5
#                               , obs = valLabels)
#     newScores[ii,6] <- MCRMSE(pred = rf.test.pred, obs = valLabels)
#     newScores[ii,7] <- MCRMSE(pred = (svm.test.pred*3+rf.test.pred)/4
#                               , obs = valLabels)
#     newScores[ii,8] <- MCRMSE(pred = (svm.test.pred*2 + rf.test.pred)/3
#                               , obs = valLabels)
#    newScores[ii,2] <- MCRMSE(pred = (svm.test.pred*7 + rf.test.pred*2 + gbm.test.pred)/10
#                              , obs = valLabels)
    
    newScores[ii,2] <- MCRMSE(pred = glmnet.test.pred, obs = valLabels)
    
    Scores[[ii]] <- newScores
    cat("\n")
    cat("\n")
    cat("End of fold:", ii, "=======================\n")  
    cat("\n")
  }
  
  
  
  colnames(newScores) <-c("svm","gbm","3svm1gbm", "2svm1gbm", "4svm1gbm"
                          , "rf","3svm1rf","2svm1rf","7svm2rf1gbm"
                          ,"ridgeCombo","glmnet")
  
 
  
}
#compute the beta_ridge(coeffs in ridge regression) using numerically stable SVD

computeMyBetaRidge = function(Z, lambda,y){
  #Z, data matrix, y is the response vector
  #step 1 (SVD)
  ##need package MASS
  if(!library(MASS, logical.return = TRUE)){library(MASS)}
  ## Z must be a numeric matrix. 
  if(!is.matrix(Z)){ Z = as.matrix(Z)}
  #n = dim(Z)[1]
  
  #perform SVD 
  svd.z <- svd(Z)
  
  #step 2
  D = svd.z$d
  U = svd.z$u
  V = svd.z$v
  matD <- matrix(0,nrow=length(D), ncol = length(D))
  diag(matD) <- D
  
    
  #step 3
  #beta_ridge <- (V %*% diag(D/(lambda + D^2)) %*% t(V))%*%(t(U)%*%y)
  beta_ridge <- (V %*% solve(lambda*diag(length(D)) + matD^2)%*%matD) %*% (t(U)%*%y)
  return(beta_ridge)
}