train <- raw.train[,-c(1,3580:3585)]
labels <- raw.train[,3581:3585]


cat("Building the final SVM model for test set=======\n")

count = 1
svm.model <- list()
for(sp in soil_prop){
  svm.model[[count]] <- svm(x=as.matrix(train)
                            ,y = labels[,sp]
                            , cost= 100, gamma = 10^(-4))
  count = count + 1
}

cat("Predicting...........\n")
for(count in 1:5){
  
  if(count == 1){
    svm.pred.test = data.frame(PIDN = raw.test$PIDN)
    svm.pred.train = data.frame(PIDN = raw.train$PIDN)
  }
  svm.pred.test <- cbind(svm.pred.test
                          , zz = predict(svm.model[[count]]
                                         , newdata = as.matrix(raw.test[,-c(1,3580)])))
  svm.pred.train <- cbind(svm.pred.train
                          , zz.train = predict(svm.model[[count]]
                                               , newdata = as.matrix(train)))
}

names(svm.pred.test)[2:6] <- soil_prop
names(svm.pred.train)[2:6] <- soil_prop
cat("SVM done............................\n")

cat("First residual......................\n")

residual = labels - svm.pred.train[,-1]

cat("Begining RandomForest on the residual...............\n")

count = 1
rf.model <- list()
for(sp in soil_prop){
  rf.model[[count]] <- randomForest(x=train
                                    ,y=residual[,sp]
                                    ,ntree = 100)
  count = count + 1
  
}

cat("Predicting..............\n")
for(count in 1:5){
  if(count == 1){
    rf.pred.test <- data.frame(PIDN = raw.test$PIDN)
    rf.pred.train <- data.frame(PIDN = raw.train$PIDN)
  }
  
  rf.pred.test <- cbind(rf.pred.test
                        , zz = predict(rf.model[[count]]
                                       , newdata =raw.test[,-c(1,3580)]) )
  
  rf.pred.train <- cbind(rf.pred.train
                         , zz.train = predict(rf.model[[count]]
                                              , newdata = train))
}

names(rf.pred.test)[2:6] <- soil_prop
names(rf.pred.train)[2:6] <- soil_prop
cat("Random Forest is done ................................\n")
cat("\n")

res.pred.test <- svm.pred.test[,-1] + rf.pred.test[,-1]
res.pred.test <- cbind(PIDN = raw.test$PIDN, res.pred.test)

write.csv(res.pred.test, "resEnsemble.csv"
          , row.names = FALSE,quote = FALSE)


# # cat("\n")
# # cat("Building GBM model on the entire train set==========\n")
# # count = 1
# # gbm.model <- list()
# # for(sp in soil_prop){
# #   gbm.model[[count]] <- gbm.fit(x=train
# #                               , y=labels[,sp]
# #                               , distribution="gaussian"
# #                               , n.trees = best.iter[count]
# #                               , nTrain = floor(nrow(train)*0.8))
# #   
# #   count = count + 1
# # }
# # cat("Predicting..............\n")
# # for(count in 1:5){
# #   if(count == 1){
# #     gbm.pred.test <- data.frame(PIDN = raw.test$PIDN)
# #     gbm.pred.train <- data.frame(PIDN = raw.train$PIDN)
# #   }
# #   
# #   gbm.pred.test <- cbind(gbm.pred.test
# #                           , zz = predict(gbm.model[[count]]
# #                                          , newdata = raw.test[,-c(1,3580)]
# #                                          , type = "response"
# #                                          , n.trees = best.iter[count]))
# #   gbm.pred.train <- cbind(gbm.pred.train
# #                           , zz.train = predict(gbm.model[[count]]
# #                                                , newdata = train
# #                                                , type = "response"
# #                                                , n.trees = best.iter[count]))
# #   
# # }
# # 
# # names(gbm.pred.test)[2:6] <- soil_prop
# # names(gbm.pred.train)[2:6] <- soil_prop
# # cat("GBM done================\n")
# # cat("\n")
# 
# cat("Begining RandomForest on the entire training set=============\n")
# 
# count = 1
# rf.model <- list()
# for(sp in soil_prop){
#     rf.model[[count]] <- randomForest(x=train
#                                       ,y=labels[,sp]
#                                       ,ntree = 100)
#     count = count + 1
# 
# }
# 
# cat("Predicting..............\n")
# for(count in 1:5){
#   if(count == 1){
#     rf.pred.test <- data.frame(PIDN = raw.test$PIDN)
#     rf.pred.train <- data.frame(PIDN = raw.train$PIDN)
#   }
#   
#   rf.pred.test <- cbind(rf.pred.test
#                          , zz = predict(rf.model[[count]]
#                                         , newdata =raw.test[,-c(1,3580)]) )
#   
#   rf.pred.train <- cbind(rf.pred.train
#                          , zz.train = predict(rf.model[[count]]
#                                               , newdata = train))
# }
# 
# names(rf.pred.test)[2:6] <- soil_prop
# names(rf.pred.train)[2:6] <- soil_prop
# cat("Random Forest is done ==============\n")
# cat("\n")
# 
# cat("Begining KNN...........................\n")
# 
# knn.model <- list()
# knn.t.model <- list()
# count = 1
# for(sp in soil_prop){
#   knn.model[[count]] <- knn.reg(train = train
#                                 , test = raw.test[,-c(1,3580)]
#                                 , y = labels[,sp]
#                                 , k = 4)
#   knn.t.model[[count]] <- knn.reg(train = train
#                                   , y = labels[,sp]
#                                   , k = 4)
#   
#   if(count == 1){
#     knn.pred.test <- data.frame(X = rep(1,knn.model[[1]]$n))
#     knn.pred.train <- data.frame(X = rep(1, knn.t.model[[1]]$n))
#   }
#   knn.pred.test <- cbind(knn.pred.test, zz = knn.model[[count]]$pred)
#   knn.pred.train <- cbind(knn.pred.train, zz = knn.t.model[[count]]$pred)
#   count = count + 1
# }
# 
# #knn.pred.test[,1] <- raw.test$PIDN
# names(knn.pred.test) <- soil_prop
# #knn.pred.train[,1] <- raw
# names(knn.pred.train) <- soil_prop
# 
# cat("KNN done................................\n")
# 
# 
# cat("Combining the predictions for each response variables via ridge\n")
# Z.test = list()  ## test set for the glmnet
# Z.train = list() ## this is the training set for glmnet
# for(i in 1:5){
#   Z.train[[i]] = cbind(svm=svm.pred.train[,(i+1)], gbm = gbm.pred.train[,(i+1)]
#                        , rf = rf.pred.train[,(i+1)])
#   Z.test[[i]] = cbind(svm.t=svm.pred.test[,(i+1)], gbm = gbm.pred.test[,(i+1)]
#                  , rf.t = rf.pred.test[,(i+1)] )
# }
# 
# # cat("Ridge on Z.train to find weights\n")
# # ## We run glmnet on Z.train with labels as the response
# # numFolds <- 10
# # FOLDS <- createFolds(size = nrow(Z.train[[1]]),K = numFolds)
# # folds <- FOLDS$Folds
# # glmnet.model <- list()
# # glmnet.scores <- c()
# # 
# # for(ii in 1:numFolds){
# #   count = 1
# #   testIndices <- which(folds == ii, arr.ind = TRUE)
# #   trainLabels <- labels[-testIndices,]
# #   valLabels <- labels[testIndices,]
# #   
# #   for(count in 1:5){
# #     train.Z.train <- Z.train[[count]][-testIndices,]
# #     val.Z.train <- Z.train[[count]][testIndices,]
# #     
# #     glmnet.model[[count]] <- cv.glmnet(x=as.matrix(train.Z.train)
# #                                        ,y = trainLabels[,count]
# #                                        , alpha = 0)
# #     
# #   }
# #   for(i in 1:5){
# #    if(i == 1){glmnet.pred.val <- data.frame(X = rep(1,nrow(val.Z.train)))}
# #     glmnet.pred.val <- cbind(glmnet.pred.val
# #                               , zz = predict(glmnet.model[[i]]
# #                                              , newx = as.matrix(val.Z.train)
# #                                              , type = "response"
# #                                              , s = "lambda.min"))
# #   }
# #   glmnet.pred.val[,1] <- NULL
# #   names(glmnet.pred.val) <- soil_prop
# #   ## calculate MCRMSE 
# #   glmnet.scores[ii] <- MCRMSE(pred = glmnet.pred.val, obs = valLabels)
# # 
# # }
# # cat("End GLMNET WEIGHT calculations==========\n")
# 
# ##final day prediction 1:weighted average
# 
# combo.test.pred <- (0.8*svm.pred.test[,-1]+rf.pred.test[,-1]+0.05*gbm.pred.test[,-1]+0.05*knn.pred.test[,-1])
# combo.test.pred <- cbind(PIDN = raw.test$PIDN, combo.test.pred)

