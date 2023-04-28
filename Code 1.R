## 1
#Website link:
https://towardsdatascience.com/predict-customer-churn-the-right-way-using-pycaret-8ba6541 608ac
# Youtube link to ggplot tutorial = https://www.youtube.com/watch?v=49fADBfcDD4
file <-
  "https://raw.githubusercontent.com/srees1988/predict-churn-py/main/customer_churn_data.cs v"
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
#structure of the data
str(data)
# is.na.data.frame(data)
# to check null values in each column cbind( lapply( lapply(data, is.na),sum) )
# replace NA columns with 0 values data <- replace(data, is.na(data), 0)
## use ggplot or dply for visualisation library(ggplot2)
library(scales) # for percentage scales
## 2
pdf('/Users/yenshen/Desktop/FYP/Data log/Output/plot_graphs.pdf') #create a pdf so that any graphs that load can be saved inside
# please change the pathway to find a place to be downloaded in your own computer myplots <- list() for (i in 1:length(column_names)) { if (class(data[,i]) == "factor") {
plotz <- ggplot(data, aes(x= Churn, fill = data[,i])) + theme_bw() +
  geom_bar(position = 'fill') + labs(y = 'Customer Percentage', title = paste('Churn by', column_names[i])) +
  labs(fill= paste(column_names[i]) ) +  # rename the Legend title scale_y_continuous(labels = scales::percent)
  print(plotz) myplots[[i]] <- plotz
}
if (class(data[,i]) == "numeric") {
  plotw <- ggplot(data, aes(x= Churn, y= .data[[column_names[i]]])) +
    theme_bw() +
    labs( y= paste(column_names[i]) , x = 'Churn',
          title = paste('Churn by', column_names[i])) +
    geom_boxplot() print(plotw) myplots[[i]] <- plotw
} else { myplots[[i]] <- 0
}
}
## For Senior Citizen (Exception) vv <-table(data$SeniorCitizen, data$Churn) frame <- as.data.frame(vv) names(frame)[2] <- 'Churn' names(frame)[1] <- 'SeniorCitizen'
ggplot(frame, aes(x=Churn, y = Freq, fill= SeniorCitizen)) + theme_bw() +
  geom_bar(stat="identity", position = 'fill') + scale_y_continuous(labels = scales::percent)+
  labs(title = 'Churn by Senior Citizen', y = 'Customer Percentage')
## For tenure (Exception) ggplot(data, aes(x= Churn, y= tenure)) +
theme_bw() + geom_boxplot() + labs( y= 'tenure', x = 'Churn',
                                    title = 'Churn rate by Tenure') dev.off() # end the doc
## 3
####### Testing for Normality  #########
############ numerical  ####################
######## We shall use Kolmogorov Smirnov test ########
## tenure
ks_ten_no <- ks.test(data$tenure[data$Churn == 'No'], "pnorm") ks_ten_yes <- ks.test(data$tenure[data$Churn == 'Yes'], "pnorm")
## Monthly Charges
ks_mc_no <- ks.test(data$MonthlyCharges[data$Churn == 'No'], "pnorm") ks_mc_yes <- ks.test(data$MonthlyCharges[data$Churn == 'Yes'], "pnorm")
## Total Charges
ks_tc_no <- ks.test(data$TotalCharges[data$Churn == 'No'], "pnorm") ks_tc_yes <- ks.test(data$TotalCharges[data$Churn == 'Yes'], "pnorm")
#### All of them do not follow a normal distribution ####
###### p value less than 2.2 x 10 ^ -16 ######
######  EXAMPLE KS TEST ###### ###### TENURE WITH NO CHURN ######
ten_no <- data$tenure[data$Churn == 'No'] freq <- as.data.frame(table(ten_no)) x <- seq(0, 72 , 1)
seqq1 <- seq(0, 1 , 1/73) seqq1 <- seqq1[-74]
seqq2 <- seq(0, 1 , 1/73) seqq2 <- seqq2[-1] f_x <- pnorm(x)
D_1 <- f_x - seqq1
D_2 <- seqq2 - f_x
##### mean 0 and variance 1 cbind(freq, seqq1, f_x, seqq2, D_1, D_2)
max(D_1,D_2) #  0.9575542
# D at 5% level
1.36 / sqrt(length(x)) # 0.159176
ks_ten_no
# 0.93009
#--------------------------------------------------------------------------------------------
################ Man Whitney Test ######################## ################## numerical ############################
# can't be computed exact because it has ties
# For tenure
tenure_tab <- wilcox.test(x= data$tenure[data$Churn =='Yes'],
                          y=data$tenure[data$Churn =='No'],
                          exact = FALSE)
tenure_tab$p.value
# For MonthlyCharges
MC_tab <- wilcox.test(x= data$MonthlyCharges[data$Churn =='Yes'], y=data$MonthlyCharges[data$Churn =='No'], exact = FALSE)
MC_tab$p.value
#For TotalCharges
TC_tab <- wilcox.test(x= data$TotalCharges[data$Churn =='Yes'], y=data$TotalCharges[data$Churn =='No'],
                      exact = FALSE)
TC_tab$p.value
#--------------------------------------------------------------------------------------------
####### Chisquared of Independence ######### ############ categorical variables ################# df <- data
df <- subset (df, select = -Churn) # delete churn
df <- subset (df, select = -customerID) # delete off CustomerID
df_chiqsq <- df
## delete off the numerical variables ###
df_chiqsq <- subset (df_chiqsq, select = -tenure) # delete off tenure
df_chiqsq <- subset (df_chiqsq, select = -MonthlyCharges) # delete off MonthlyCharges df_chiqsq <- subset (df_chiqsq, select = -TotalCharges) # delete off TotalCharges row.names(df_chiqsq) <- NULL # reset the index colname_chi <- colnames(df_chiqsq)
# For 2 categorial data, we shall perform chi square of independence test store_here <- list() ## store the p-value here for (i in 1:length(df_chiqsq)) { if (class(df_chiqsq[,i]) == "factor") { tab1 <- xtabs(~df_chiqsq[[colname_chi[i]]] + data$Churn ) chisq_ <- chisq.test(tab1) store_here[i] <- paste(chisq_$p.value)
}
else{
  store_here[i] <- '-'
}
}
# For SeniorCitizen
SC_tab <- xtabs(~data$SeniorCitizen + data$Churn) SC_chisq_ <- chisq.test(SC_tab) store_here[[2]]<- SC_chisq_$p.value ## store the test statistic here test_stat_here <- list() ## store the test statistic here for (i in 1:length(df_chiqsq)) { if (class(df_chiqsq[,i]) == "factor") { tab1 <- xtabs(~df_chiqsq[[colname_chi[i]]] + data$Churn ) chisq_ <- chisq.test(tab1)
test_stat_here[i] <- paste(chisq_$statistic)
} else{
  test_stat_here[i] <- '-'
}
}
# Senior Citizen Test Stat test_stat_here[[2]]<- SC_chisq_$statistic
######## degree of freedom ############ df_here <- list() ## store the df here for (i in 1:length(df_chiqsq)) { if (class(df_chiqsq[,i]) == "factor") { tab1 <- xtabs(~df_chiqsq[[colname_chi[i]]] + data$Churn ) chisq_ <- chisq.test(tab1)
df_here[i] <- paste(chisq_$parameter)
} else{
  df_here[i] <- '-'
}
}
# Senior Citizen DF
df_here[[2]]<- SC_chisq_$parameter
#### combine all the values into a table
chi_table <- data.frame(cbind(unlist(colname_chi), unlist(test_stat_here), unlist(df_here), unlist(store_here) )) colnames(chi_table) <- c('Variable', 'Test Statistic', 'DF', 'P_value')
P_value <- chi_table$P_value P_value<- as.numeric(P_value)
isit = list()
for (i in 1: nrow(chi_table)) {
  if (P_value[[i]] < 0.05) { # If our significant value is 5%
    isit[i] <- 'Yes'
  } else{
    isit[i] <- 'No'
  }
}
p <- data.frame(matrix(unlist(isit), nrow=length(isit), byrow=TRUE)) chi_table<- cbind(chi_table, p) names(chi_table)[5] <- 'Significant at 5% ?' # Is it significant to be rejected at this level?
row.names(chi_table) <- NULL # reset the index chi_table write.csv(chi_table,'/Users/yenshen/Desktop/FYP/Data log/Output/chi_table.csv')
############ EXAMPLE GENDER ############
############ chi_squared --- gender ##################
total_row <- apply(table_chi, 1, sum) # sum by row of female/male total_col <- apply(table_chi, 2, sum) # sum by column of yes/no total_n <- sum(table_chi) ## total of all men and women
O_male_no = table_chi[2,1]
O_male_yes = table_chi[2,2]
O_fem_yes = table_chi[1,2]
O_fem_no = table_chi[1,1]
# calculate the expected values
# E = (row total x column total) / n
# expected of men, yes to churn
E_male_no = (3555* 5174)/total_n
E_male_yes = (3555* 1869)/total_n
E_fem_yes = (3488 * 1869)/ total_n E_fem_no = (3488 * 5174)/ total_n
# chi squared = SUM [ ( (obs - exp)^2 / exp )]
gen_chi_sq_test =  ( (O_male_no - E_male_no)^2 / E_male_no ) +
  ( (O_male_yes - E_male_yes)^2 / E_male_yes ) +
  ( (O_fem_yes - E_fem_yes)^2 / E_fem_yes ) + ( (O_fem_no - E_fem_no)^2 / E_fem_no )
gen_chi_sq_test
# Yate's corrected version
yate_gen_chi_sq_test =  ( ( abs(O_male_no - E_male_no) - 0.5)^2 / E_male_no ) +
  ( (abs(O_male_yes - E_male_yes) - 0.5 )^2 / E_male_yes ) +
  ( (abs(O_fem_yes - E_fem_yes) - 0.5 )^2 / E_fem_yes ) + ( (abs(O_fem_no - E_fem_no) - 0.5 )^2 / E_fem_no ) yate_gen_chi_sq_test
p_value_gen <- pchisq(0.4840829, 1, lower.tail = FALSE) p_value_gen
table_chi <- xtabs(~data$gender + data$Churn ) chisq_gen <- chisq.test(table_chi) chisq_gen
#-------------------------------------------------------------------------------------------
####### For numerical values #########
####  Find the mean and variance ####
## Tenure ## Monthly Charges ## Total Charges
######tenure
summary(data$tenure[data$Churn == 'No']) summary(data$tenure[data$Churn == 'Yes'])
#std dev
sd(data$tenure[data$Churn == 'No']) sd(data$tenure[data$Churn == 'Yes'])
#total charges
summary(data$TotalCharges[data$Churn == 'No']) summary(data$TotalCharges[data$Churn == 'Yes'])
#std dev
sd(data$TotalCharges[data$Churn == 'No']) sd(data$TotalCharges[data$Churn == 'Yes'])
##### monthly charges
summary(data$MonthlyCharges[data$Churn == 'No']) summary(data$MonthlyCharges[data$Churn == 'Yes'])
#std dev
sd(data$MonthlyCharges[data$Churn == 'No']) sd(data$MonthlyCharges[data$Churn == 'Yes'])
#--------------------------------------------------------------------------------------------
################  Proportion tables ##################### ##########  to be accompanied with the figures ########## tab_here <- list()
for (i in 1:length(column_names)) {
  if (class(data[,i]) == "factor") {
    tab <- prop.table(xtabs(~ data[[column_names[i]]] + data$Churn ), 2) tab_here[[i]] <- tab
    write.csv(tab_here[[i]], file= paste(i, column_names[i], 'csv', sep = '.'))
    # for every proportion table, output it into a separate csv file
  } else{
    tab_here[[i]] <- 0
  }
}
##################################################################
### additional tables ######
## Paperless Billing & Senior Citizen
table_pb_sc <- xtabs(~data$PaperlessBilling+ data$Churn +data$SeniorCitizen)
table_dep_multiple <- xtabs(~data$Dependents+ data$Churn +data$MultipleLines) ##4
library(tidyverse) library(caret) theme_set(theme_bw())
################# PREPARING THE DATA ######################
## remove the CustomerID column data_new <- subset(data, select = -customerID)
# one hot encoding is not needed as we have changed our categorical variables to factors
########### COMPUTE LOGISTIC REGRESSION ################
### FIT 1 ###
# fit the model with all the variables
model1 <- glm(formula = Churn~. , data = data_new, family = binomial)
# ~. means every possible combination of var
summary(model1) summary(model1)$coef
### FIT 2 ### (with variables selection) # fit the model with all the significant variables data_new2 <- subset(data, select = -customerID)
data_new2 <- subset(data_new2, select = -gender) # not significant data_new2 <- subset(data_new2, select = - PhoneService) # not significant model2 <- glm(formula = Churn~., data = data_new2, family = binomial)
summary(model2) summary(model2)$coef
### FIT 3 ###
# fit the model with all variables from Fit2 (variable selection) except for Paperless Billing and Monthly Charges # data_new3 <- data_new2
data_new3<- subset(data_new3, select = -PaperlessBilling) data_new3 <- subset(data_new3, select = -TotalCharges) model3 <- glm(formula = Churn~., data = data_new3, family = binomial)
summary(model3) summary(model3)$coef ##### plot the predicted probability chart ############ predicted_data <- data.frame(prob_of_churn = model$fitted.values, Churn = data_new$Churn)
predicted_data <- predicted_data[
  order(predicted_data$prob_of_churn, decreasing = FALSE), ] # sort from low to high prob
predicted_data$rank <- 1:nrow(predicted_data) # add a new column that ranks it from low to high prob
ggplot(data = predicted_data, aes(x = rank, y = prob_of_churn)) +
  geom_point(aes(color = Churn), alpha = 1, shape = 4, stroke = 2) + xlab("Index") +
  ylab("Predicted probability of churning")
############## How do I determine which model is better? ####################
######################### PREDICTION PROBABILITY
############################
## with a threshold of 0.5
# if it is more than 0.5, output 1 - No Churn
# if it is less than 0.5, ouput 0 - Yes Churn
# sum up the entries
# compare each model with each classification
Churn <-  data_new$Churn
Churn <- as.factor(Churn)
Churn <- as.numeric(Churn) # 1 as No, 2 as Yes Churn <- as.factor(Churn)
############### for model 1 prediction ############################## value_1 <- predict(model1, type = 'response')
# baseline is No, so the probabilities are prob of not churning list_1 <- list() # store into this list of model 1 outputs whether churn or not for ( x in 1: length(value_1) ) {
# baseline is No, so the probabilities are prob of not churning
if (value_1[x] >= 0.05) {  #if more than 0.05
  list_1[x] = 1 # No to churn
}
else {
  list_1[x] = 2 # Yes to churn
}
}
### compare to the true value of churn
### Misclassification = 1 ### No misclassification = 0
mis_rate_1 <- ifelse(list_1 == Churn, 0, 1)
# if same, output 0. If not, output 1
# 0 - correct
# 1 - misclassification
mean(mis_rate_1) # 0.5222206
###########################################################################
###############
############### for model 2 ##############################
value_2 <- predict(model2, type = 'response')
# baseline is No, so the probabilities are prob of not churning
############## for model  prediction ##############################
list_2 <- list() # store into this list of model 2 outputs whether churn or not for ( x in 1: length(value_2) ) {
if (value_2[x] >= 0.05) { #if more than 0.05
  list_2[x] = 1 # No to churn
}
else {
  list_2[x] = 2 #Yes to churn
}
}
mis_rate_2 <- ifelse(list_2 == Churn, 0, 1) # if same, output 0. If not, output 1
mean(mis_rate_2) # 0.5232145
###########################################################################
###############
############### for model 3 ##############################
value_3 <- predict(model3, type = 'response')
# baseline is No, so the probabilities are prob of not churning
############## for model 3 prediction ##############################
list_3 <- list() # store into this list of model 3 outputs whether churn or not for ( x in 1: length(value_3) ) {
if (value_3[x] >= 0.05) { #if more than 0.05
  list_3[x] = 1 # No to churn
}
else {
  list_3[x] = 2 #Yes to churn
}
}
mis_rate_3 <- ifelse(list_3 == Churn, 0, 1) # if same, output 0. If not, output 1
mean(mis_rate_3) # 0.5156893
# deviance models
# This is a generic function which
# can be used to extract deviances for fitted models
deviance(model1) deviance(model2) deviance(model3)
# Conduct the likelihood ratio test on each model anova(model1, model2, test = 'LRT') anova(model2, model3, test = 'LRT') # significant
