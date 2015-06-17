#Initial Processing

control_data <- read.csv('CAX_COPD_TRAIN_data.csv')
targetVar <- control_data$Exacer 
control_data$Exacer <- NULL
CAX_ID <- control_data$CAX_ID

test_data <- read.csv('CAX_COPD_TEST_data.csv')
test_CAX_ID <- test_data$CAX_ID
test_data$CAX_ID <- NULL


#library(caret)
#used this package to remove highly correlated columms
## the reduced feature data frame is called filtered_control_data
##this is what I did:

#remove the near-zero values 
nzv <- nearZeroVar(control_data[-1])
#nothing in it. Otherwise woud do control_data[-nzv]

#remove highly correlated columns/features
featureCorrs <- cor(control_data[-1])
highlyCorFeatures <- findCorrelation(featureCorrs, cutoff=0.8)
filtered_control_data <- control_data[,-highlyCorFeatures]