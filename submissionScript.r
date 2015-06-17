#Submission Script

##run the model with specific 's' and alpha 
#get these from sumary_of_results_df dataframe

#s = sumary_of_results_df[3,4]
#alpha = sumary_of_results_df[3,1]

# alpha = 0.4
# #s = "lambda.min"
# s = 0.014670928
# 
# #fit the model on the entire control set
# fit <- cv.glmnet(as.matrix(control_data[-1]), 
#               targetVar, 
#               alpha=alpha,
#               family ="binomial")
# #predicted set with s 
# pred <- predict(fit, 
#                 as.matrix(test_data[-1]), 
#                 s=s,
#                 type="response")


##==========GBM=========

submitGBMmodel <- function(submissionFileName = 'submission_gbm1000_3_50.csv', 
                           n.trees = 1000, 
                           interaction.depth = 3,
                           n.minobsinnode = 50,
                           shrinkage = 0.001){

    
  fit <- gbm.fit(x=control_data[-1], #don't include the id column, hence -1
                    y=targetVar,
                    distribution = "bernoulli",
                    n.trees = n.trees , #hyper-parameter
                    interaction.depth = interaction.depth, # hyper-parameter
                    n.minobsinnode = n.minobsinnode, # hyper-parameter
                    shrinkage = shrinkage, # hyper-parameter
                    bag.fraction = 0.5, #use random-seed above:reproducability
                    nTrain = floor(nrow(train_X)*0.8),
                    verbose=FALSE)
  
  best.iter <- gbm.perf(fit, method="test", plot.it=FALSE)
  
  cat("best.iter: ", best.iter,"\n")
  #don't include the id column, hence -1
  pred <- predict(fit,
                  newdata=test_data[-1],
                  n.trees = best.iter,
                  type="response")
  
  cat("prediction done. Preparing for submission\n")
  #prepare the data for submission
  submission_df <- data.frame(test_CAX_ID, pred)
  names(submission_df) <- c('CAX_ID','Exacer')
  
  write.csv(submission_df,submissionFileName, 
            row.names=FALSE ,
            quote=FALSE)
  
}