#read all the submission files back into a data frame

eReg <- read.csv('submission_elasticReg.csv')
lasso <- read.csv('submission_lasso.csv')
gbm1000 <- read.csv('submission_gbm1000.csv')
gbm5000 <- read.csv('submission_gbm5000.csv')
gbm1000_3_50 <- read.csv('submission_gbm1000_3_50.csv')
gbm10000_3_50 <- read.csv('sub_gbm10000_3_50.csv')

#collect all the previous submission values into a data frame
df <- cbind(eReg$Exacer, 
            lasso$Exacer, 
            gbm1000$Exacer, 
            gbm5000$Exacer,
            gbm1000_3_50$Exacer, 
            gbm10000_3_50$Exacer)

df_1 <- df[,c(3,6)]

##find the mean value of each row of df
meanVals <- apply(df_1,1, mean)

##max vals
maxVals <- apply(df,1,max)

## this is our final submission
submission_df <- data.frame(test_CAX_ID, meanVals)
#submission_df <- data.frame(test_CAX_ID, maxVals)
names(submission_df) <- c('CAX_ID','Exacer')

write.csv(submission_df,'submission_final_mean_2.csv', 
          row.names=FALSE ,
          quote=FALSE)