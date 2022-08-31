#Loading libraries
library(ggplot2)
library(Amelia)
library(dplyr)
library(skimr)
library(DataExplorer)
library(factoextra)
library(cluster)


#importing data from the path
getwd()
setwd("C:/Users/William/OneDrive/Desktop/jo/nci/Data Mining & Machine Learning 1/projects/Loan Prediction")
credit_data<-read.csv("credit_card_details.csv",header=TRUE,na.strings=c(""))
head(credit_data)
str(credit_data)
attach(credit_data)
#drop customer ID
credit_data <- credit_data %>% dplyr::select(-CUST_ID)
str(credit_data)

options(digits=3,scipen = 999)
# First we will analyse the response variable Loan_sanction_Amount
plot_histogram(credit_data)

#Most of the data is skewed we have to apply log function to get a normal distribution
summary(credit_data)

#Handling Missing values
sapply(credit_data,function(x) sum(is.na(x)))

#Visual representation of missing data
missmap(credit_data, main = "Missing values vs observed")

#credit limit has only one Na, we are dropping that row
credit_data<-credit_data[!is.na(credit_data$CREDIT_LIMIT ),]
# we can simply use median to replace the Nan values as the distribution 
# for minimum payments is skewed and hence median gives a better estimation of the
# central tendency of this feature.

credit_data$MINIMUM_PAYMENTS[is.na(credit_data$MINIMUM_PAYMENTS)]<-median(credit_data$MINIMUM_PAYMENTS,
                                                                          na.rm=TRUE)


plot_intro(credit_data)

par(mfrow=c(3,5))
plot_density(credit_data)
plot_histogram(credit_data)
#There is a lot of skewwd 
credit_transformed<-log(1+credit_data)



head(credit_transformed)

plot_histogram(credit_transformed)
plot_density(credit_transformed)

plot_correlation(credit_transformed)
#There are many co-related variables in the plot. we will go ahead with PCA dimensionality reduction




#Calculate how many clusters you need
#within sum squares

#PCA for dimension reduction


pca_credit = prcomp(credit_transformed, center = TRUE, scale = TRUE)
summary(pca_credit)
fviz_eig(pca_credit)
 
credit_pca = as.data.frame(-pca_credit$x[,1:2])
fviz_nbclust(credit_pca, kmeans, method = 'wss')
fviz_nbclust(credit_pca, kmeans, method = 'silhouette')
fviz_nbclust(credit_transform, kmeans, method = 'gap_stat')
nrow(credit_pca)
k = 3
kmeans_credit = kmeans(credit_pca, centers = k, nstart = 100,iter.max=300)
?kmeans
kmeans_credit
fviz_cluster(kmeans_credit, data = credit_pca)


kmeans_credit$cluster

head(credit_data)
#credit_data = subset(credit_data, select = -c(cluster_id) )

credit_data$cluster <- kmeans_credit$cluster
credit_data
credit_data_scale<-scale(credit_data)

credit_data$cluster<-as.factor(credit_data$cluster)
ggplot(credit_data, aes(x=CREDIT_LIMIT, y=PURCHASES, shape=cluster, color=cluster)) +
  geom_point()

ggplot(credit_data, aes(x=BALANCE, y=PURCHASES, color=cluster)) +
  geom_point()

ggplot(credit_data, aes(x=ONEOFF_PURCHASES, y=PURCHASES, color=cluster)) +
  geom_point()

credit_data_1 = subset(credit_data, select = -c(cluster) )
library(cluster)

head(credit_data)

plot_density(PURCHASES,col=credit_data$cluster)

ggplot(credit_data, aes(x=PURCHASES_FREQUENCY,color=cluster)) + 
  geom_density()

ggplot(credit_data, aes(x=PURCHASES_INSTALLMENTS_FREQUENCY,color=cluster)) + 
  geom_density()


# ncol(credit_data)---normalizing for better visiblity-----------------
# 
# #define Min-Max normalization function
# min_max_norm <- function(x) {
#   (x - min(x)) / (max(x) - min(x))
# }
# 
# #apply Min-Max normalization to first four columns in iris dataset
# credit_norm <- as.data.frame(lapply(credit_data[1:17], min_max_norm))
# 
# #view first six rows of normalized iris dataset
# head(credit_norm)
# credit_norm$cluster <- kmeans_credit$cluster
# str(credit_norm)
# 
# ggplot(credit_norm, aes(x=BALANCE, y=PURCHASES, color=cluster)) +
#   geom_point()
# ggplot(credit_norm, aes(x=PURCHASES_INSTALLMENTS_FREQUENCY,color=cluster)) + 
#   geom_density()
# 
# credit_norm$cluster<-as.factor(credit_norm$cluster)
# 
# ggplot(credit_norm, aes(x=CREDIT_LIMIT, y=PURCHASES, shape=cluster, color=cluster)) +
#   geom_point()
