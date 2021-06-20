library(tidyr)
library(ggplot2)
library(dplyr)
library(caret)
library(ranger)
library(matrixStats)
library(MASS)


data_wider<-readRDS('input_dataset.rds', refhook = NULL)
copy_data<-data_wider
copy_data<-dplyr::select(data_wider, !ends_with("15"))
copy_data$id<-seq.int(nrow(copy_data))

##Making the data Balanced

cassini.train_unscaled<-copy_data%>%filter(type_cross=='NE')%>%sample_n(100)%>%rbind(copy_data%>%filter(type_cross=='BS')%>%sample_n(100))%>%rbind(copy_data%>%filter(type_cross=='MP')%>%sample_n(100))

cassini.test_unscaled<-copy_data[-cassini.train_unscaled$id,]
cassini.test_unscaled<-dplyr::select(cassini.test_unscaled,-id)
cassini.train_unscaled<-dplyr::select(cassini.train_unscaled,-id)


####Shuffling Dataset


set.seed(42)
rows <- sample(nrow(cassini.train_unscaled))
temp<- cassini.train_unscaled[rows, ]
cassini.train_unscaled<-temp
##### Scaling the data



cassini.train_scaled<-cassini.train_unscaled%>%mutate_if(is.numeric,scale)

means <- lapply(dplyr::select(cassini.train_unscaled,-c(type_cross,dirn_cross)), mean)
sds <- lapply(dplyr::select(cassini.train_unscaled,-c(type_cross,dirn_cross)), sd)
means <- matrix(unlist(means), ncol = 210, byrow = FALSE)
sds <- matrix(unlist(sds), ncol = 210, byrow = FALSE)

#### Normalizing the TEST DATASET 

cassini.test_scaled<-dplyr::select(cassini.test_unscaled,-c(type_cross,dirn_cross))
  
for(col in c(1:dim(cassini.test_scaled)[2])){
  temp<-(cassini.test_scaled[,col]-means[,col])/sds[,col]
  cassini.test_scaled[,col]<-temp
  temp<-NULL
  
}
cassini.test_scaled$type_cross<-cassini.test_unscaled$type_cross
cassini.test_scaled$dirn_cross<-cassini.test_unscaled$dirn_cross


###### Random Forrest Model


cassini.rfmodel <- ranger(type_cross ~ ., data = cassini.train_scaled)
cassini.test_scaled$pred<-predict(cassini.rfmodel,data = cassini.test_scaled)$predictions
confusionMatrix(cassini.test_scaled$pred, cassini.test_scaled$type_cross)

#####Logistic Regression

##### MP as 1 and all other events as 0

  ##Train
  new_cassini.train_scaled<-cassini.train_scaled%>%mutate(event_occured=ifelse(type_cross=='NE'|type_cross=='DG'|type_cross=='SC'|type_cross=='BS',0,1))
  new_cassini.train_scaled$event_occured<-as.factor(new_cassini.train_scaled$event_occured)
  new_cassini.train_scaled<-dplyr::select(new_cassini.train_scaled,-c(type_cross,dirn_cross))
  
  ##Test
  
  new_cassini.test_scaled<-cassini.test_scaled%>%mutate(event_occured=ifelse(type_cross=='NE'|type_cross=='DG'|type_cross=='SC'|type_cross=='BS',0,1))
  new_cassini.test_scaled$event_occured<-as.factor(new_cassini.test_scaled$event_occured)
  new_cassini.test_scaled<-dplyr::select(new_cassini.test_scaled,-c(type_cross,pred,dirn_cross))

  
  ####Model
  set.seed(120)
  log_model <- glm(event_occured ~ ., family = "binomial", data = new_cassini.train_scaled) 
  
  summary(log_model)
  
  new_cassini.test_scaled$prob<-predict(log_model,new_cassini.test_scaled,type="response")
  
  
  
  new_cassini.test_scaled$pred <- factor(ifelse(new_cassini.test_scaled$prob < 0.9, 0,1))
  confusionMatrix(new_cassini.test_scaled$pred, new_cassini.test_scaled$event_occured)

  
##### BS as 1 and all other events as 0
  
  ##Train
  new_cassini.train_scaled<-cassini.train_scaled%>%mutate(event_occured=ifelse(type_cross=='NE'|type_cross=='DG'|type_cross=='SC'|type_cross=='MP',0,1))
  new_cassini.train_scaled$event_occured<-as.factor(new_cassini.train_scaled$event_occured)
  new_cassini.train_scaled<-dplyr::select(new_cassini.train_scaled,-c(type_cross,dirn_cross))
  
  ##Test
  
  new_cassini.test_scaled<-cassini.test_scaled%>%mutate(event_occured=ifelse(type_cross=='NE'|type_cross=='DG'|type_cross=='SC'|type_cross=='MP',0,1))
  new_cassini.test_scaled$event_occured<-as.factor(new_cassini.test_scaled$event_occured)
  new_cassini.test_scaled<-dplyr::select(new_cassini.test_scaled,-c(type_cross,pred,dirn_cross))
  
  
  ####Model
  set.seed(220)
  log_model <- glm(event_occured ~ ., family = "binomial", data = new_cassini.train_scaled) 
  
  
  
  new_cassini.test_scaled$prob<-predict(log_model,new_cassini.test_scaled,type="response")
  
  
  
  new_cassini.test_scaled$pred <- factor(ifelse(new_cassini.test_scaled$prob < 0.9, 0,1))
  confusionMatrix(new_cassini.test_scaled$pred, new_cassini.test_scaled$event_occured)
