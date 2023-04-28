##### Data Processing ###### #Website link: https://towardsdatascience.com/predict-customer-churn-the-right-way-using-pycaret-8ba6541608ac
# Youtube link to ggplot tutorial = https://www.youtube.com/watch?v=49fADBfcDD4
file <-
  "https://raw.githubusercontent.com/srees1988/predict-churn-py/main/customer_churn_data.csv"
data <- read.csv(file)
View(data) # view the data in a different tab
#library("dplyr")
column_names <- data name_of_col <- colnames(data)
#structure of the data str(data)
#	7043 obs. of  21 variables:
class(data)
### loop over the column headings and change characters to factors for (i in 1:length(column_names)) {
if (lapply(data, class)[[i]] == "character") {
  data[,i] <- as.factor(data[,i])
  # data$column<- as.factor(data$column)
}
}
data$customerID<- as.character(data$customerID) # CustomerID must be Character because it is Unique
## check that the data has changed to factor
#structure of the data str(data) data_with_NA <- data
# is.na.data.frame(data)
# to check null values in each column cbind( lapply( lapply(data, is.na),sum) )
# replace NA columns with 0 values data <- replace(data, is.na(data), 0)
##########################################################################
########## Decision Trees ##########
# install libraries library(rpart) library(rpart.plot) library(caTools) library(caret) library(dplyr)
# the question to answer is, will this customer churn?
# dataset1 won't include CustomerID dataset1 <- subset(data, select = -customerID) str(dataset1)
set.seed(6563) # split the data subset(data, ) training <- createDataPartition(data[ ,"Churn"], p=0.9, list = FALSE)
train_data <- dataset1[training,] # split train and valid test_data <- dataset1[-training,]
# training_2 <- createDataPartition(train_data[ ,"Churn"], p=0.8, list = FALSE)
# # run CV with training indices 2
# # cp optimised
# # take that optimised # cv_data <set.seed(6563)
# 10-Fold CV
#cross_val <- trainControl(method = "repeatedcv", number = 10, repeats = 10) cross_val <- trainControl(method = "cv", number = 10)
set.seed(6563)
# Training the DT Classifier # by default, the splitting index is gini tree_train <- train(Churn ~. , data = train_data, method = "rpart", trControl = cross_val, tuneLength = 10)
# plot model accuracy vs cp (complexity parameter) plot(tree_train)
plot(tree_train, xlim = c(0, 0.01)) # plot the accuracy curve
# print the best tune parameter cp # that maximises the model accuracy tree_train$bestTune # 0.003267974
#max accuracy?
max(tree_train$results) # [1] 0.7966954 # which depth tree_train$finalModel summary(tree_train$finalModel)
par(xpd = NA) plot(tree_train$finalModel) text(tree_train$finalModel, pretty = 0) title(main = "10-CV Classification Tree with Gini")
# or can plot out this way suppressMessages(library(rattle)) fancyRpartPlot(tree_train$finalModel)
# Make predictions on the test data predicted_classes_gini <- tree_train %>% predict(test_data)
# Compute model accuracy rate on test data mean(predicted_classes_gini == test_data$Churn)
#### 0.8122333
### CHECK WITH CROSS ENTROPY AND SEE RESULTS
set.seed(6563) tree_cross_entropy <- train(Churn ~. , data = train_data, method = "rpart",
                                           trControl = cross_val, tuneLength = 10, parms = list(split = "information"))
# plot model accuracy vs cp (complexity parameter) plot(tree_cross_entropy) plot(tree_cross_entropy, xlim = c(0, 0.02)) #readjust
# plot the accuracy curve ggplot(tree_cross_entropy) ggplot(tree_cross_entropy) + coord_cartesian(xlim = c(0, 0.02))
# print the best tune parameter cp # that maximises the model accuracy tree_cross_entropy$bestTune # 0.002376708
# max accuracy? max(tree_cross_entropy$results) # 0.7906974
# which depth tree_cross_entropy$finalModel summary(tree_cross_entropy$finalModel)
par(xpd = NA) plot(tree_cross_entropy$finalModel) text(tree_cross_entropy$finalModel, pretty = 0) title(main = "10-Cross validation Classification Tree with Entropy")
# or can plot out this way suppressMessages(library(rattle)) fancyRpartPlot(tree_cross_entropy$finalModel)
# Make predictions on the test data predicted_classes_ce <- tree_cross_entropy %>% predict(test_data)
# Compute model accuracy rate on test data mean(predicted_classes_ce == test_data$Churn) ## [1] 0.8065434
##### CREATE A CONFUSION MATRIX
# gini confusionMatrix(test_data$Churn,predicted_classes_gini )
# Reference
# Prediction  No Yes
# No  479  38
# Yes  94  92
# cross entropy confusionMatrix(test_data$Churn,predicted_classes_ce )
# Reference
# Prediction  No Yes
# No  473  44
# Yes  92  94
##########################################################################
####################### RANDOM FOREST #######################
## with out of bag for the missing values
library(ggplot2) library(cowplot) library(randomForest) library(dplyr)
data_with_NA dataset_NA <- subset(data_with_NA, select = -customerID) ?rfImpute #missing value imputations by randomForest set.seed(6563)
# Impute missing values in predictor data using proximity # from randomForest.
imputed_data <- rfImpute(Churn ~. , data = dataset_NA) # it will produce 5 rows
# ntree      OOB      1      2
# 300:  20.25% 10.55% 47.08%
# ntree      OOB      1      2
# 300:  20.50% 10.55% 48.05%
# ntree      OOB      1      2
# 300:  20.49% 10.46% 48.26%
# ntree      OOB      1      2
# 300:  20.15% 10.32% 47.35%
# ntree      OOB      1      2
# 300:  20.47% 10.63% 47.73%
model_rf <- randomForest(Churn~. , data = imputed_data, proximity = TRUE) model_rf
# Type of random forest: classification
# Number of trees: 500
# No. of variables tried at each split: 4
#
# OOB estimate of  error rate: 20.26%
## what this means: that 79.74% of OOB samples were correctly classified in the random forest # Confusion matrix:
#   No Yes class.error
# No  4651 523   0.1010823
# Yes  904 965   0.4836811
### plot variable importance ## mean decrease gini table1<-data.frame(model_rf$importance) rowz_z <-table1[,1] rowz_n <-row.names.data.frame(table1) barplot(table1)
weee <- data.frame(variable = rowz_n, variable_importance = rowz_z)
ggplot(data = weee, aes(x = variable, y = variable_importance)) + geom_bar(stat = 'identity')+
  coord_flip() #####
oob_error_rate <- data.frame(
  Trees = rep(1:nrow(model_rf$err.rate), times = 3),
  Type = rep(c("OOB", "No", "Yes"), each = nrow(model_rf$err.rate)), Error = c(model_rf$err.rate[, "OOB"], model_rf$err.rate[, "No"], model_rf$err.rate[, "Yes"]))
ggplot(data = oob_error_rate, aes(x = Trees, y = Error)) + geom_line(aes (color = Type)) +
  labs( title = 'Error rate of 500 trees' ) + theme(plot.title = element_text(hjust = 0.5))
## Blue line = The error rate specifically for Non Churners that ## are OOB.
##
## Green line = The overall OOB error rate.
##
## Red line = The error rate specifically for Yes Churners that ##are OOB.
## REPEAT BUT WITH 1000 TREES
model_rf <- randomForest(Churn~. , data = imputed_data, proximity = TRUE, ntree = 1000) model_rf
# Call:
#   randomForest(formula = Churn ~ ., data = imputed_data, proximity = TRUE,      ntree = 1000)
# Type of random forest: classification
# Number of trees: 1000
# No. of variables tried at each split: 4
#
# OOB estimate of  error rate: 20.29% # Confusion matrix:
#   No Yes class.error
# No  4626 548   0.1059142 # Yes  881 988   0.4713751
oob_error_rate <- data.frame(
  Trees = rep(1:nrow(model_rf$err.rate), times = 3),
  Type = rep(c("OOB", "No", "Yes"), each = nrow(model_rf$err.rate)),
  Error = c(model_rf$err.rate[, "OOB"], model_rf$err.rate[, "No"], model_rf$err.rate[, "Yes"]))
ggplot(data = oob_error_rate, aes(x = Trees, y = Error)) +
  geom_line(aes (color = Type)) + labs( title = 'Error rate of 1,000 trees' ) + theme(plot.title = element_text(hjust = 0.5))
# Would work just as best with 500 trees,
# as there is a convergence even after 500 trees
######### Calculate the test data accuracy (training accuracy) #########
# Make predictions on the test data predicted_classes_RF <- model_rf %>% predict(test_data)
# Compute model accuracy rate on test data mean(predicted_classes_RF == test_data$Churn)
####0.9765458
confusionMatrix(test_data$Churn, predicted_classes_RF)
# Confusion Matrix and Statistics
#
# Reference
# Prediction  No Yes
# No  510   7
# Yes  10 176
#
# Accuracy : 0.9758
# 95% CI : (0.9616, 0.9859)
# No Information Rate : 0.7397
# P-Value [Acc > NIR] : <2e-16
#
# Kappa : 0.9375
#
# Mcnemar's Test P-Value : 0.6276
#
#             Sensitivity : 0.9808
#             Specificity : 0.9617
#          Pos Pred Value : 0.9865
#          Neg Pred Value : 0.9462
#              Prevalence : 0.7397
#          Detection Rate : 0.7255
#    Detection Prevalence : 0.7354
#       Balanced Accuracy : 0.9713
#
#        'Positive' Class : No
##########################################################################
####################### NEURAL NETWORK #######################
# Deep Learning
# Neural Network
# The word 'deep' stems from  the many layers of the neural network
# A neural network that only has two or three layers is just a basic neural network.
str(dataset1)
str(train_data)
library(neuralnet) library(caret) library(NeuralNetTools) library(dplyr)
set.seed(6563)
#### PROCESS THE DATA: perform one-hot encoding ####### #exclude churn so that we can one hot enconding on others dataset2 <- subset(dataset1, select = -Churn) str(dataset2)
#define one-hot encoding function dummy <- dummyVars(" ~ .", data=dataset2)
#perform one-hot encoding on data frame final_df <- data.frame(predict(dummy, newdata=dataset2))
#view final data frame final_df
# combine back the Churn Column final_df<- cbind(final_df, dataset1$Churn)
#rename to Churn colnames(final_df)[colnames(final_df) == "dataset1$Churn"] <- "Churn"
######## ######## ######## ######## ######## ######## fit the neural network ######## nn_telco_all <- neuralnet( Churn ~ . ,
data = final_df, linear.output = FALSE, hidden = 2
# specify the nodes per layer)
)
## plot the neural network graphing plot(nn_telco_all)
#### have the final_df go through cross validation as well cross_val <- trainControl(method = "cv", number = 10) set.seed(6563)
# Training the neural net classifier #
# took about 7mins to load out
Gneural_net_train <- train(Churn ~. , data = final_df, method = "nnet", trControl = cross_val
                           #,size = 3
                           ,maxit = 200
                           ,preProcess = c("center","scale")
                           ,tuneGrid = expand.grid(size = seq(1,5,1), decay = c(0, 0.01, 0.1, 1))
                           # or decay = 10^seq(-5,0,by=1))
                           # size =Hidden Layers
                           # decay = _ #Weight Decay
                           ,metric= "Accuracy")
ggplot(Gneural_net_train)
# locate the best tuned, the accuracy rate
Gneural_net_train$bestTune
#   size decay # 7    4   0.1
Gneural_net_train$results
which.max(Gneural_net_train$results$Accuracy)
# 15    4  0.10 0.8059076 0.4716851 0.009576771 0.02545719
max(Gneural_net_train$results$Accuracy) # max accuracy attained at 0.8059076
## Now, let's test it with the test data ##
######### Calculate the test data accuracy (training accuracy) #########
## based on the highest is attained when model is 2 with decay 0.1 best_neural_net_train <- train(Churn ~. , data = final_df, method = "nnet",
trControl = cross_val
#,size = 3
,maxit = 200
,preProcess = c("center","scale")
,tuneGrid = expand.grid(size = 4, decay = 0.1)
# or decay = 10^seq(-5,0,by=1))
# size =Hidden Layers
# decay = _ #Weight Decay
,metric= "Accuracy")
## make test data become one hot encoding as well nn_test_data <- test_data
#### PROCESS THE DATA: perform one-hot encoding ####### #exclude churn so that we can one hot enconding on others nn_test_dataset <- subset(nn_test_data, select = -Churn) str(nn_test_dataset)
#define one-hot encoding function dummy <- dummyVars(" ~ .", data=nn_test_dataset)
#perform one-hot encoding on data frame nn_final_test_data<- data.frame(predict(dummy, newdata=nn_test_dataset))
#view final test data for nn nn_final_test_data
# combine back the Churn Column nn_final_test_data<- cbind(nn_final_test_data, nn_test_data$Churn)
#rename to Churn colnames(nn_final_test_data)[colnames(nn_final_test_data) == "nn_test_data$Churn"] <- "Churn"
# Make predictions on the test data predicted_classes_NN <- best_neural_net_train %>% predict(nn_final_test_data)
# Compute model accuracy rate on test data mean(predicted_classes_NN == nn_final_test_data$Churn)
####  0.8165007 confusionMatrix(nn_final_test_data$Churn,predicted_classes_NN)
# Confusion Matrix and Statistics
#
# Reference
# Prediction  No Yes
# No  469  48
# Yes  81 105
#
# Accuracy : 0.8165
# 95% CI : (0.7859, 0.8444)
# No Information Rate : 0.7824
# P-Value [Acc > NIR] : 0.014561
#
# Kappa : 0.5001
#
# Mcnemar's Test P-Value : 0.004841
