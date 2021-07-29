
library(tidyverse)
library(tidyr)
library(ggplot2)
library(dplyr)
library(caret)
library(ranger)
library(glmnet)
library(plyr)
library(readr)
library(repr)




#####GLMNET 


data_avg<-readRDS("./Data/dataset_version_2/Average_SD_Data.rds", refhook = NULL)
data_avg %>% glimpse()
copy_data<-data_avg
t<-copy_data[!(copy_data$type_cross=="DG" | copy_data$type_cross=="SC"),]
copy_data<-t
copy_data$type_cross<-factor(copy_data$type_cross)
copy_data$id<-seq.int(nrow(copy_data))




cassini.train_unscaled<-copy_data%>% filter(type_cross=='NE')%>%sample_n(150) %>%rbind(copy_data%>%filter(type_cross=='BS')%>%sample_n(100))%>%rbind(copy_data%>%filter(type_cross=='MP')%>%sample_n(100))
cassini.train_unscaled$type_cross %>% table()
cassini.train_unscaled$type_cross<-as.factor(cassini.train_unscaled$type_cross)
cassini.test_unscaled<-copy_data[-cassini.train_unscaled$id,]
cassini.test_unscaled<-dplyr::select(cassini.test_unscaled,-id)
cassini.train_unscaled<-dplyr::select(cassini.train_unscaled,-id)





## Scaling the data

cassini.train_scaled<-cassini.train_unscaled%>%mutate_if(is.numeric,scale)
means <- lapply(dplyr::select(cassini.train_unscaled,-c(type_cross,dirn_cross)), mean)
sds <- lapply(dplyr::select(cassini.train_unscaled,-c(type_cross,dirn_cross)), sd)
means <- matrix(unlist(means), ncol = 23, byrow = FALSE)
sds <- matrix(unlist(sds), ncol = 23, byrow = FALSE)


## Normalizing the TEST DATASET


cassini.test_scaled<-dplyr::select(cassini.test_unscaled,-c(type_cross,dirn_cross))

for(col in c(1:dim(cassini.test_scaled)[2])){
  temp<-(cassini.test_scaled[,col]-means[1,col])/sds[1,col]
  cassini.test_scaled[,col]<-as.matrix(temp)
  temp<-NULL
}
cassini.test_scaled$type_cross<-cassini.test_unscaled$type_cross
cassini.test_scaled$dirn_cross<-cassini.test_unscaled$dirn_cross


cassini.train_scaled<-cassini.train_scaled%>%mutate(event_occured=ifelse(type_cross=='NE',0,ifelse(type_cross=='BS',1,ifelse(type_cross=='MP',2,0))))
cassini.train_scaled$event_occured<-as.factor(cassini.train_scaled$event_occured)
cassini.train_scaled <- dplyr::select(cassini.train_scaled,-c(type_cross,dirn_cross))

cassini.test_scaled<-cassini.test_scaled%>%mutate(event_occured=ifelse(type_cross=='NE',0,ifelse(type_cross=='BS',1,ifelse(type_cross=='MP',2,0))))
cassini.test_scaled$event_occured<-as.factor(cassini.test_scaled$event_occured)
cassini.test_scaled <- dplyr::select(cassini.test_scaled,-c(type_cross,dirn_cross))


# Setting alpha = 1 implements lasso regression
x_tr<-as.matrix(dplyr::select(cassini.train_scaled,-event_occured))
y_tr<-as.matrix(dplyr::select(cassini.train_scaled,event_occured))
x_te<-as.matrix(dplyr::select(cassini.test_scaled,-event_occured))
y_te<-as.matrix(dplyr::select(cassini.test_scaled,event_occured))

lambdas <- 10^seq(2, -3, by = -.1)

lasso_reg <- lasso_reg <- cv.glmnet(x_tr, y_tr, alpha = 1, lambda = lambdas, nfolds = 5, family="multinomial")

# Best 
lambda_best <- lasso_reg$lambda.min 
lambda_best

###Lasso

lasso_model <- glmnet(x_tr, y_tr, alpha = 1, lambda = lambda_best, family="multinomial")

predictions_train <- predict(lasso_model, s = lambda_best, newx = x_tr,type="class")
#eval_results(y_train, predictions_train, train)

predictions_test <- predict(lasso_model, s = lambda_best, newx = x_te,type="class")
#eval_results(y_test, predictions_test, test)
table(predictions_train)
table(predictions_test)

confusionMatrix(table(predictions_train,y_tr))

confusionMatrix(table(predictions_test,y_te))
