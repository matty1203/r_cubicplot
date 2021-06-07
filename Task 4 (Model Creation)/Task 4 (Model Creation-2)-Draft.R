library(tidyr)
library(ggplot2)
library(dplyr)
library(caret)
library(ranger)
library(matrixStats)
library(MASS)

data_Wider<-readRDS('input_dataset.rds', refhook = NULL)
copy_data<-data_Wider

scaled_data<-copy_data%>%mutate_if(is.numeric,scale)


##Randomly choose 500 NE Points and remaining Events

set.seed(234)
new_data<-scaled_data%>%filter(type_cross=='NE')%>%sample_n(500)%>%rbind(scaled_data%>%filter(type_cross!='NE'))
new_data<-new_data%>%mutate(event_occured=ifelse(type_cross=='NE'|type_cross=='DG'|type_cross=='SC',0,1))
new_data$event_occured<-as.factor(new_data$event_occured)
new_data<-dplyr::select(new_data,-c(type_cross))
  
train.idx <- sample(nrow(new_data), 2/3 * nrow(new_data))
cassini.train <- new_data[train.idx, ]
cassini.test <- new_data[-train.idx, ]

log_model <- glm(event_occured ~ ., family = "binomial", data = cassini.train)

cassini.test$prob=predict(log_model,cassini.test,type="response")
cassini.test$pred <- factor(ifelse(cassini.test$prob < .9, 0,1))

confusionMatrix(cassini.test$pred, cassini.test$event_occured)

########################

data1<-scaled_data%>%mutate(event_occured=ifelse(type_cross=='NE'|type_cross=='DG'|type_cross=='SC',0,1))
data1$event_occured<-as.factor(data1$event_occured)
data1<-dplyr::select(data1,-c(type_cross))

data2<-data1%>%filter(event_occured==0)%>%sample_n(500)%>%rbind(data1%>%filter(event_occured!=0))

set.seed(120)
train.idx <- sample(nrow(data2), 2/3 * nrow(data2))
cassini.train1 <- data2[train.idx, ]
cassini.test1 <- data2[-train.idx, ]

log_model1 <- glm(event_occured ~ ., family = "binomial", data = cassini.train1)