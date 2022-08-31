#Loading libraries
library(ggplot2)
library(car)
library(dplyr)
library(skimr)
library(DataExplorer)
library(lm.beta)
library(lmtest)

#importing data from the path
getwd()
setwd("C:/Users/William/OneDrive/Desktop/jo/nci/Data Mining & Machine Learning 1/projects/Loan Prediction")
loan_sanction<-read.csv("Loan_sanction_amt.csv",header=TRUE,na.strings=c(""),stringsAsFactors = T)
head(loan_sanction)
str(loan_sanction)
#drop customer ID
loan_sanction <- loan_sanction %>% select(-Customer.ID)
str(loan_sanction)

#change the  column_names
colnames(loan_sanction)[4]<-"Income"
colnames(loan_sanction)[5]<-"Income_Stability"
colnames(loan_sanction)[7]<-"Type_of_Employment"
colnames(loan_sanction)[9]<-"Loan_Amount_Request"
colnames(loan_sanction)[10]<-"Current_Loan_Expenses"
colnames(loan_sanction)[11]<-"Expense_Type_1"
colnames(loan_sanction)[12]<-"Expense_Type_2"
colnames(loan_sanction)[14]<-"Credit_Score"
colnames(loan_sanction)[15]<-"No_of_Defaults"
colnames(loan_sanction)[16]<-"Has_Active_Credit_Card"
colnames(loan_sanction)[17]<-"Property_ID"
colnames(loan_sanction)[18]<-"Property_Age"
colnames(loan_sanction)[19]<-"Property_Type"
colnames(loan_sanction)[20]<-"Property_Location"
colnames(loan_sanction)[21]<-"Co_Applicant"
colnames(loan_sanction)[22]<-"Property_Price"
colnames(loan_sanction)[23]<-"Loan_sanction_Amount"


attach(loan_sanction)
#setting up the digits display ,this helps to view p value in decimal
options(digits=3,scipen = 999)





skim(loan_sanction)
str(loan_sanction)
# Remove columns with lots of unique values and convert other category variable to factors

loan_sanction <- loan_sanction %>% select(-Type_of_Employment,-Name)

#convert No_of_Defaults and Property_Type and   Co_Applicant  and  Dependents to char and then to factors
loan_sanction$No_of_Defaults<-as.factor(as.character(loan_sanction$No_of_Defaults))
loan_sanction$Property_Type<-as.factor(as.character(loan_sanction$Property_Type))
loan_sanction$Co_Applicant<-as.factor(as.character(loan_sanction$Co_Applicant ))
loan_sanction$Dependents<-as.factor(as.character(loan_sanction$Dependents ))


table(loan_sanction$Has_Active_Credit_Card)
table(loan_sanction$Property_Location)

sapply(loan_sanction,function(x) sum(is.na(x)))


library(Amelia)
#Visual representation of missing data
missmap(loan_sanction, main = "Missing values vs observed")
# First we will analyse the response variable Loan_sanction_Amount
plot_histogram(Loan_sanction_Amount)
summary(loan_sanction$Loan_sanction_Amount)

loan_sanction<-loan_sanction[loan_sanction$Loan_sanction_Amount >0, ]
loan_sanction<-loan_sanction[!is.na(loan_sanction$Loan_sanction_Amount),]

#Handling Missing values
#Remove rows with empty values for Has_Active_Credit_Card and Property_Location
loan_sanction<-loan_sanction[!is.na(loan_sanction$Has_Active_Credit_Card),]
loan_sanction<-loan_sanction[!is.na(loan_sanction$Property_Location),]

#Replace missing continuous variables by mean
table(loan_sanction$Dependents)

loan_sanction[-1] <- lapply(loan_sanction[-1], function(x) {
  if(is.numeric(x)) replace(x, is.na(x), mean(x, na.rm=TRUE))
  else x
})


sapply(loan_sanction,function(x) sum(is.na(x)))

#Three category variables Gender,Income stability and Dependents are having missing values

table(loan_sanction$Gender)
# we will replace the 52 missing with mode ie,'M"
ggplot(loan_sanction, aes(x = loan_sanction$Gender)) +
  geom_bar()

#convert all NA's to 88
loan_sanction$Gender[is.na(loan_sanction$Gender)] = "M"

#Income stability 
table(loan_sanction$Income_Stability)
ggplot(loan_sanction, aes(x = Income_Stability)) +
  geom_bar()
loan_sanction$Income_Stability[is.na(loan_sanction$Income_Stability)] = "Low"

table(loan_sanction$Dependents)
#Assign 0 to na
levels(loan_sanction$Dependents)
loan_sanction$Dependents<-as.character(loan_sanction$Dependents)
loan_sanction$Dependents[is.na(loan_sanction$Dependents)] = "0"
loan_sanction$Dependents<-as.factor(loan_sanction$Dependents)
----------------------------------------------------------------------
# ggplot(loan_sanction, aes(x = Income_Stability,fill=Profession
# )) + 
#   geom_bar(position = "stack") 
# 
# # from this graph we can assign all the pensioners in NA to High and remaining to Low
# 
# if (is.na(loan_sanction$Income_Stability)){
#   
#   if(loan_sanction$Profession=="Pensioner"){
#   loan_sanction$Income_Stability="High"}
#   else{
#     loan_sanction$Income_Stability="Low"
#   
#     
#   }
# }
sum(is.na(loan_sanction$Income_Stability))

sapply(loan_sanction,function(x) sum(is.na(x)))

#-------------------EDA------------------------------------------------
  
  plot_intro(loan_sanction)
par(mfrow=c(2,2))
plot_density(loan_sanction)
plot_histogram(loan_sanction)
str(loan_sanction)

#df<-loan_sanction[is.numeric(loan_sanction),]
plot_bar(loan_sanction)
#pairs(loan_sanction)

df <- loan_sanction[ , sapply(loan_sanction, is.numeric)]
pairs(df)
cor(df)

# From the corelation plot loan sanction amount is highly dependent on Loan_Amount_Request,
#Current_Loan_Expenses and Property_Price,credit score..loamn amount request highly corelated withproperty price

plot_correlation(loan_sanction)

#The categorical variables profession,Expense_type,location and dependents

boxplot(Loan_sanction_Amount, horizontal = TRUE) #Outliers >165000

boxplot(loan_sanction$Credit_Score, horizontal = TRUE,main="Credit_Score") #no outliers
boxplot(loan_sanction$Loan_Amount_Request, horizontal = TRUE,main="Loan_Amount_Request")
boxplot(loan_sanction$Loan_sanction_Amount, horizontal = TRUE,main="Loan_sanction_Amount") 
?boxplot
loan_sanction <- filter(loan_sanction,Loan_sanction_Amount<=165000,Loan_Amount_Request<=230000)


#-----Multiple Regression------
loan_sanction_m<-loan_sanction
attach(loan_sanction_m)
lmfit1<-lm(loan_sanction_m$Loan_sanction_Amount~., data=loan_sanction_m)
summary(lmfit1)

#Remove outliers for the significant variables







hist((Credit_Score))
hist(sqrt(Credit_Score))
hist(log(Loan_Amount_Request))
hist(Loan_sanction_Amount)
hist(log(Loan_sanction_Amount))


lmfit2<-lm(loan_sanction_m$Loan_sanction_Amount~Loan_Amount_Request +
             Credit_Score+Current_Loan_Expenses  , data=loan_sanction_m)

summary(lmfit2)


lmfit3<-lm(loan_sanction_m$Loan_sanction_Amount~Loan_Amount_Request +
             Credit_Score + Dependents , data=loan_sanction_m)

summary(lmfit3)


lmfit4<-lm(loan_sanction_m$Loan_sanction_Amount~Loan_Amount_Request +
             Credit_Score , data=loan_sanction_m)
summary(lmfit5)




lmfit5<-lm(log10(loan_sanction_m$Loan_sanction_Amount)~log10(loan_sanction_m$Loan_Amount_Request) +
             sqrt(loan_sanction_m$Credit_Score) , data=loan_sanction_m)

par(mfrow=c(2,2))
plot(lmfit5)




plot(density(lmfit5$residuals))

summary(lmfit5)

durbinWatsonTest(lmfit5)

vif(lmfit5)

#identify outliers
cooks.distance(lmfit5)
influencePlot(model=lmfit5,scale=3,main="influence plot")
predict(lmfit5)
10^log10(loan_sanction_m$Loan_sanction_Amount)

(loan_sanction_m$Loan_sanction_Amount)-(10^predict(lmfit5))




# lmfit6<-lm(sqrt(loan_sanction_m$Loan_sanction_Amount)~sqrt(loan_sanction_m$Loan_Amount_Request) +
#              Current_Loan_Expenses , data=loan_sanction_m)
# 
# boxplot(Loan_sanction_Amount, horizontal = TRUE) #Outliers >180000
# loan_sanction_m <- filter(loan_sanction_m,Loan_sanction_Amount<=165000,Loan_Amount_Request<=235000)
# boxplot(loan_sanction_m$Loan_sanction_Amount, horizontal = TRUE) #Outliers >180000
# boxplot(loan_sanction_m$Credit_Score, horizontal = TRUE) #Outliers >180000
# boxplot(loan_sanction_m$Loan_Amount_Request, horizontal = TRUE)
# 
# 
# hist(lmfit5$residuals)
# hist(sqrt(Credit_Score))
# hist((Loan_Amount_Request))
# 
# 
# library(car)
# 
# summary(lmfit6)
# 
# 
# #Remove the rows with null values for  Has_Active_Credit_Card  and Property_Location
# 
# 
# durbinWatsonTest(lmfit5)
# 
# #check for multicollinearity informal check corelation formal check VIF test
# vif(lmfit5)
#convert the categorical variables to factors


# library(MASS)
# 
# lmfit20<-lm(((Loan_sanction_Amount^lambda-1)/lambda)~log(Loan_Amount_Request) +
#               (Credit_Score), data=loan_sanction_m)
# #find optimal lambda for Box-Cox transformation 
# bc <- boxcox(Loan_sanction_Amount~log(Loan_Amount_Request) +
#                (Credit_Score))
# bc
# (lambda <- bc$x[which.max(bc$y)])
# 
# par(mfrow=c(2,2))
# plot(lmfit20)
# summary(lmfit20)
# ncvTest(lmfit20)

#--------------Decision Trees------------------------------------------#

library(rpart)
library(rpart.plot)
loan_sanction_dt<-loan_sanction
attach(loan_sanction_dt)


set.seed(122)
# 70% of the sample size
size <- floor(0.70 * nrow(loan_sanction_dt))
train_index <- sample(seq_len(nrow(loan_sanction_dt)), size = size)
# creating test and training sets that contain all of the predictors
loan_sanction_train <- loan_sanction_dt[train_index, ]
loan_sanction_test <- loan_sanction_dt[-train_index, ]



tree<-rpart(loan_sanction_train$Loan_sanction_Amount~loan_sanction_train$Credit_Score+
              (loan_sanction_train$Loan_Amount_Request),
            data=loan_sanction_train,method="anova")

pred_sanction<- predict(tree, loan_sanction_test)
print(tree)
head(pred_sanction)
rpart.plot(tree)

library(caret)

MAE<-function(actual,predicted){mean(abs(actual-predicted))}
MAE(loan_sanction_test$Loan_sanction_Amount,pred_sanction)

mse<-mean((loan_sanction_test$Loan_sanction_Amount-pred_sanction)^2)
mse

R_square <- cor(loan_sanction_test$Loan_sanction_Amount,pred_sanction) ^ 2


#----------Multiple Regression with split------------------------------------------

loan_sanction_mr<-loan_sanction
par(mfrow=c(2,2))

set.seed(122)
# 70% of the sample size
size_lm <- floor(0.70 * nrow(loan_sanction_mr))
train_index_lm <- sample(seq_len(nrow(loan_sanction_mr)), size = size_lm)
# creating test and training sets that contain all of the predictors
loan_mr_train <- loan_sanction_mr[train_index_lm, ]
loan_mr_test <- loan_sanction_mr[-train_index_lm, ]

lm5<-lm(log(Loan_sanction_Amount)~log(Loan_Amount_Request) +
             (Credit_Score) , data=loan_mr_train)

summary(lm5)
plot(lm5)
head(loan_mr_test_1)#7,12

loan_mr_test_1<-loan_mr_test %>% select(Loan_Amount_Request,Credit_Score)

predict_lm<-predict(lm5,newdata=loan_mr_test_1)
?predict
predict_lm
predict_lm_transformed<-exp(predict_lm)
predict_lm_transformed
loan_mr_test$Loan_sanction_Amount
nrow(loan_mr_test)
nrow(loan_mr_train)

R_square <- cor(loan_mr_test$Loan_sanction_Amount,predict_lm_transformed) ^ 2
R_square
MAE(loan_mr_test$Loan_sanction_Amount,predict_lm_transformed)

nrow(loan_sanction_test)


#-----XGBOOST Regression__________
library(xgboost)
library(caret)

loan_sanction_xg<-loan_sanction[c(2,3,7,8,11,12,13,16,19,20,21)]
loan_sanction_xg$Dependents<-as.numeric(as.character(loan_sanction_xg$Dependents))
loan_sanction_xg$No_of_Defaults<-as.numeric(as.character(loan_sanction_xg$No_of_Defaults))
loan_sanction_xg$Co_Applicant<-as.numeric(as.character(loan_sanction_xg$Co_Applicant))
str(loan_sanction_xg)
par(mfrow=c(2,2))

set.seed(1)
# 70% of the sample size
size_xg <- floor(0.70 * nrow(loan_sanction_xg))
train_index_xg <- sample(seq_len(nrow(loan_sanction_xg)), size = size_xg)
# creating test and training sets that contain all of the predictors
loan_xg_train <- loan_sanction_xg[train_index_xg, ]
loan_xg_test <- loan_sanction_xg[-train_index_xg, ]

grid_tune<-expand.grid(
  
  nrounds=c(500,1000,1500),#numberof trees
  max_depth=c(2,4,6),
  eta=0.3,#learning rate
  gamma=0,#pruning
  colsample_bytree=1,#subsample ratio of columns for trees
  min_child_weight=1,#larger then more conservative model c(1,2,3)
  subsample=1
)

train_control<-trainControl(method="cv",
                            number=3,
                            verboseIter=TRUE,
                            allowParallel=TRUE)
xgb_tune<-train(x=loan_xg_train[,-11],y=loan_xg_train[,11],trControl=train_control
                ,tuneGrid=grid_tune,method="xgbTree",verbose=TRUE)

xgb_tune
xgb_tune$bestTune
pred_xgb<-predict(xgb_tune,loan_xg_test)


mae = caret::MAE(loan_xg_test$Loan_sanction_Amount,pred_xgb)
#rmse=caret::RMSE(loan_xg_test$Loan_sanction_Amount,pred_xgb)

R_square <- cor(loan_xg_test$Loan_sanction_Amount,pred_xgb) ^ 2
R_square

