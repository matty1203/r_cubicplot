# Models Run Function Script

library(tidyverse)
library(tidyr)
library(ggplot2)
library(dplyr)
library(caret)
library(ranger)
library(matrixStats)
library(MASS)

######Data Splitting function

prepareData<-function(ip_data,ne,bs,mp,logModel){
  copy_data<-ip_data
  t<-copy_data[!(copy_data$type_cross=="DG" | copy_data$type_cross=="SC"),]
  copy_data<-t
  copy_data$type_cross<-factor(copy_data$type_cross)
  copy_data$id<-seq.int(nrow(copy_data))
  ##Making the data Balanced
  
  cassini.train_unscaled<-copy_data%>%filter(type_cross=='NE')%>%sample_n(ne)%>%rbind(copy_data%>%filter(type_cross=='BS')%>%sample_n(bs))%>%rbind(copy_data%>%filter(type_cross=='MP')%>%sample_n(mp))

  cassini.test_unscaled<-copy_data[-cassini.train_unscaled$id,]
  cassini.test_unscaled<-dplyr::select(cassini.test_unscaled,-id)
  cassini.train_unscaled<-dplyr::select(cassini.train_unscaled,-id)
  ##### Scaling the data
  
  cassini.train_scaled<-cassini.train_unscaled%>%mutate_if(is.numeric,scale)
  cassini.train_scaled$type_cross<-factor(cassini.train_scaled$type_cross)
  means <- lapply(dplyr::select(cassini.train_unscaled,-c(type_cross,dirn_cross)), mean)
  sds <- lapply(dplyr::select(cassini.train_unscaled,-c(type_cross,dirn_cross)), sd)
  means <- matrix(unlist(means), ncol = dim(ip_data)[2], byrow = FALSE)
  sds <- matrix(unlist(sds), ncol = dim(ip_data)[2], byrow = FALSE)
  
  #### Normalizing the TEST DATASET 
  
  cassini.test_scaled<-dplyr::select(cassini.test_unscaled,-c(type_cross,dirn_cross))
  
  for(col in c(1:dim(cassini.test_scaled)[2])){
    temp<-(cassini.test_scaled[,col]-means[,col])/sds[,col]
    if(logModel==TRUE){
      cassini.test_scaled[,col]<-as.matrix(temp)
    }
    
    temp<-NULL
    
  }
  
  cassini.test_scaled$type_cross<-cassini.test_unscaled$type_cross
  cassini.train_scaled<-dplyr::select(cassini.train_scaled,-dirn_cross)
  ret_Data<-list("testData"=cassini.test_scaled,"trainData"=cassini.train_scaled)
  
  return(ret_Data)
  }

##### Logistic Regression

lr<-function(test_data,train_data,limit){
  LModel<- glm(event_occured ~ ., family = "binomial", data = train_data)
  test_data$prob=predict(LModel,test_data,type="response")
  test_data$pred <- factor(ifelse(test_data$prob < limit, 0,1))
  confusionMatrix(test_data$pred, test_data$event_occured,positive='1')
}

##### Logistic Regression

rf<-function(test_data,train_data){
  rfmodel <- ranger(type_cross ~ ., data = train_data)
  test_data$pred<-predict(rfmodel,data = test_data)$predictions
  confusionMatrix(test_data$pred, test_data$type_cross)
}

###########################  Different Data  ###########################################

##Data: Only considering 15 minutes before

data<-readRDS('input_datasetFinal.rds', refhook = NULL)
glimpse(data)
dat_15<-data[,1:107]
scaled<-prepareData(dat_15,100,100,100,TRUE)
  ##Logistic Regression Model

  ###Train Data
  cassini.logTrain<-scaled$trainData%>%mutate(event_occured=ifelse(type_cross=='NE'|type_cross=='MP',0,1))
  cassini.logTrain$event_occured<-as.factor(cassini.logTrain$event_occured)
  cassini.logTrain <- dplyr::select(cassini.logTrain,-c(type_cross))

  ###Test Data
  cassini.logTest<-scaled$testData%>%mutate(event_occured=ifelse(type_cross=='NE'|type_cross=='MP',0,1))
  cassini.logTest$event_occured<-as.factor(cassini.logTest$event_occured)
  cassini.logTest <- dplyr::select(cassini.logTest,-c(type_cross))
  
  lr( cassini.logTest,cassini.logTrain,0.8)
  
##Data : Average mean and SD 






