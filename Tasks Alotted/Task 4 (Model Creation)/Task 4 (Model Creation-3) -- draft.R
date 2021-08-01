library(tidyr)
library(ggplot2)
library(dplyr)
library(caret)
library(ranger)
library(matrixStats)
library(MASS)


data_wider<-readRDS('input_datasetFinal.rds', refhook = NULL)
copy_data<-data_wider
copy_data<-dplyr::select(data_wider, !ends_with("15"))
t<-copy_data[!(copy_data$type_cross=="DG" | copy_data$type_cross=="SC"),]
copy_data<-t
copy_data$type_cross<-factor(copy_data$type_cross)
copy_data$id<-seq.int(nrow(copy_data))
##Making the data Balanced

cassini.train_unscaled<-copy_data%>%filter(type_cross=='NE')%>%sample_n(100)%>%rbind(copy_data%>%filter(type_cross=='BS')%>%sample_n(100))%>%rbind(copy_data%>%filter(type_cross=='MP')%>%sample_n(100))
table(cassini.train_scaled$type_cross)
cassini.train_scaled$type_cross<-factor(cassini.train_scaled$type_cross)
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

##### BS as 1 and all other events as 0
new_trainData<-dplyr::select(cassini.train_scaled,-dirn_cross)
new_testData<-dplyr::select(cassini.test_scaled,-c(pred,dirn_cross))


###Training Data

cassini.logTrain<-new_trainData%>%mutate(event_occured=ifelse(type_cross=='NE'|type_cross=='MP',0,1))
cassini.logTrain$event_occured<-as.factor(cassini.logTrain$event_occured)
cassini.logTrain <- dplyr::select(cassini.logTrain,-c(type_cross))

###Training Data

cassini.logTest<-new_testData%>%mutate(event_occured=ifelse(type_cross=='NE'|type_cross=='MP',0,1))
cassini.logTest$event_occured<-as.factor(cassini.logTest$event_occured)
cassini.logTest <- dplyr::select(cassini.logTest,-c(type_cross))


dim(cassini.logTrain)
dim(cassini.logTest)


##Model

log_model <- glm(event_occured ~ ., family = "binomial", data = cassini.logTrain)

