#exploratory data analysis
setwd("C:/Users/William/OneDrive/Desktop/jo/nci/Data Mining & Machine Learning 1/projects/Loan Prediction")
getwd()
loan_data<-read.csv("Loan_default_prediction.csv",header=TRUE)
head(loan_data)
str(loan_data)
loan<-loan_data
summary(loan_data)

summary(loan_data)

library(dplyr)
library(class)
library(skimr)
library(DataExplorer)
library(ggplot2)
library(caret)
library(ROSE)
library(randomForest)
library(MLmetrics)

library(ROCR)


#checking for null values
sum(is.na(loan))
library(Amelia)

#checking duplicate values
duplicated(loan)
dup<-loan[duplicated(loan)]
print(dup) # no duplicated records

#checking for any name change or datatype change
head(loan)
str(loan)
#Removing ID column and changing the response variable into factors with codes "yes" "no"
loan <- loan %>% select(-loan$Property_ID,-Id)
loan$Risk_Flag <- ifelse(loan$Risk_Flag == 1, "Yes", "No")
head(loan)
loan$Risk_Flag <- as.factor(loan$Risk_Flag)
str(loan)

#Exploring numerical data using histogram

summary(loan)
glimpse(loan)
skim(loan)
plot_intro(loan)

#Visual representation of missing data
missmap(loan, main = "Missing values vs observed")

plot_density(loan)
plot_histogram(loan)
#plotting response variable
plot(loan$Risk_Flag,main="Plot 1: Distribution of Dependent Variable")

#categorical features
plot_bar(loan)

#scatterplot
#pairs(loan)
#cor(loan)

plot_correlation(loan)
#from the correlation graph there is a significant relation b/w current_job_years and experience

#Random_Forest
loan_random<-loan
attach(loan_random)
loan_random$Married.Single <- (as.factor(loan_random$Married.Single))
loan_random$House_Ownership <- (as.factor(loan_random$House_Ownership))
loan_random$Car_Ownership  <- (as.factor(loan_random$Car_Ownership ))
loan_random$Profession  <-(as.factor(loan_random$Profession ))
loan_random$CITY   <- (as.factor(loan_random$CITY  ))
loan_random$STATE   <- (as.factor(loan_random$STATE  ))
str(loan_random)

skim(loan_random)

#Remove city as its having 317 factors and consider state instead
loan_random <- loan_random %>% select(-CITY)
str(loan_random)

# Data Partition

set.seed(122)
# 75% of the sample size
size <- floor(0.75 * nrow(loan_random))
train_index <- sample(seq_len(nrow(loan_random)), size = size)
# creating test and training sets that contain all of the predictors
loan_random_train <- loan_random[train_index, ]
loan_random_test <- loan_random[-train_index, ]
table(loan_random_train$Risk_Flag)
loan_random_test$Risk_Flag

data_random_under <- ovun.sample(Risk_Flag ~ ., data = loan_random_train, method = "under", N = 50000 , seed = 1)$data
table(data_random_under$Risk_Flag)
par(mfrow=c(2,2))
plot(data_random_under$Risk_Flag,main="Plot 1: Distribution of Dependent Variable")
data_random_rose <- ROSE(Risk_Flag ~ ., data = loan_random_train, seed = 1)$data
table(data_random_rose$Risk_Flag)
plot(data_random_rose$Risk_Flag,main="Plot 1: Distribution of Dependent Variable")


#Random Forest using under ROSE method

set.seed(222)
rf1<-randomForest(data_random_rose$Risk_Flag~.,data=data_random_rose,importance=TRUE)
pred_rose_method<- predict(rf, newdata = loan_random_test)
print(rf1)
head(pred_rose_method)
table(pred_rose_method,loan_random_test$Risk_Flag)
pred$Risk_Flag
confusionMatrix(pred_rose_method,loan_random_test$Risk_Flag)



#Random Forest using under sample

set.seed(111)
rf<-randomForest(data_random_under$Risk_Flag~.,data=data_random_under,importance=TRUE)
pred<- predict(rf, newdata = loan_random_test)
print(rf)
head(pred)
table(pred,loan_random_test$Risk_Flag)
pred$Risk_Flag
confusionMatrix(pred,loan_random_test$Risk_Flag)
varImpPlot(rf)
#k<-multiclass.roc(loan_random_test$Risk_Flag,predict(rf, loan_random_test, type = 'prob'))
#k<-multiclass.roc(iris$Species, predict(rf, iris, type = 'prob'))

#roc = performance(pred,x.measure = "fpr",measure="tpr")
#plot(roc,colorize=TRUE)

roc.curve(loan_random_test$Risk_Flag,pred)

#----------KNN Method--------------------#
loan_knn<-loan
skim(loan_knn)
# unbalanced classes are not a problem at all for the k-nearest neighbor algorithm. 
#Because the algorithm 
#is not influenced in any way by the size of the class, it will not favor any on the basis of size.
#for KNN we will be considering all the continuous 
#variables and the category variables with less than 5 categories as 
#we need to convert all the category variables to factors

#seperate the response and predictor variables
loan_knn_response <- loan_knn %>% select(Risk_Flag)
loan_knn_response
str(loan_knn)

#not considered category variables with more than 5 levels
loan_knn_predictors <- loan_knn[,c(1,2,3,4,5,6,10,11)]
head(loan_knn_predictors)
str(loan_knn_predictors)

#create dummy variables for factors with two levels

table(loan_knn_predictors$Married.Single) # 2 levels
table(loan_knn_predictors$House_Ownership) # 3 levels
table(loan_knn_predictors$Car_Ownership) # 2 levels
library(fastDummies)

print(loan_knn_predictors)
# Create dummy variable
loan_knn_predictors$Married.Single <- ifelse(loan_knn_predictors$Married.Single == "married", 1, 0)
#change the column name
colnames(loan_knn_predictors)[4] <-'Is_Married'


loan_knn_predictors$Car_Ownership <- ifelse(loan_knn_predictors$Car_Ownership == "yes", 1, 0)
#create dummy variables for factors with three levels

loan_knn_predictors <- dummy_cols(loan_knn_predictors, 
                                     select_columns = "House_Ownership")

#remove the columns for which dummy columns are created

loan_knn_predictors <- loan_knn_predictors %>% select(-House_Ownership)
head(loan_knn_predictors)

#Normalize 
normalize <- function(x) {
  return ((x-min(x))/(max(x)-min(x)))
} 

loan_norm_subset<-as.data.frame(lapply(loan_knn_predictors,normalize))
head(loan_norm_subset)

#https://quantdev.ssri.psu.edu/sites/qdev/files/kNN_tutorial.html
#to reproduce the same random sample
set.seed(123)
# 70% of the sample size
size <- floor(0.70 * nrow(loan_norm_subset))
train_index <- sample(seq_len(nrow(loan_norm_subset)), size = size)
# creating test and training sets that contain all of the predictors
loan_pred_train <- loan_norm_subset[train_index, ]
loan_pred_test <- loan_norm_subset[-train_index, ]

nrow(loan_pred_train)
nrow(loan_pred_test)

#Split outcome variable into training and test sets using the same partition as above.
loan_knn_response
#mjob_outcome_test <- data.frame(mjob_outcome_test)
response_train <- loan_knn_response[train_index,]
response_test <- loan_knn_response[-train_index, ]
response_train
knn_10 <- knn(train = loan_pred_train, test = loan_pred_test, cl = response_train, k=10)
knn_9 <- knn(train = loan_pred_train, test = loan_pred_test, cl = response_train, k=9)
knn_15 <- knn(train = loan_pred_train, test = loan_pred_test, cl = response_train, k=15)
knn_20 <- knn(train = loan_pred_train, test = loan_pred_test, cl = response_train, k=20)
knn_434 <- knn(train = loan_pred_train, test = loan_pred_test, cl = response_train, k=434)

table(knn_10)
table(knn_9)

library(MLmetrics)

response_test<-as.data.frame(response_test)
F1_Score(y_pred = knn_10, y_true = response_test, positive = "No")
F1_Score(y_pred = knn_10, y_true = response_test, positive = "Yes")

F1_Score(y_pred = knn_9, y_true = response_test, positive = "No")
F1_Score(y_pred = knn_9, y_true = response_test, positive = "Yes")

confusionMatrix(table(knn_10,response_test))
confusionMatrix(table(knn_9,response_test))


