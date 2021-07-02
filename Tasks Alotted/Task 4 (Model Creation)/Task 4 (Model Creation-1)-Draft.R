library(tidyr)
library(ggplot2)
library(dplyr)
library(caret)
library(ranger)
library(matrixStats)
library(janitor)

data_Wider<-readRDS('input_dataset.rds', refhook = NULL)
x<-tabyl(data_Wider, type_cross)
View(x)
copy_data<-data_Wider

scaled_data<-copy_data%>%mutate_if(is.numeric,scale)


set.seed(3100)

###Full Dataset

train.idx <- sample(nrow(scaled_data), 2/3 * nrow(scaled_data))
cassini.train <- scaled_data[train.idx, ]
cassini.test <- scaled_data[-train.idx, ]
dim(cassini.test)
dim(cassini.train)


cassini.rfmodel <- ranger(type_cross ~ ., data = cassini.train, write.forest = TRUE)
getTerminalNodeIDs(rf, scaled_data)
summary(rf)



set.seed(2504)
cassini.test$pred<-predict(cassini.rfmodel,data = cassini.test)$predictions

confusionMatrix(cassini.test$pred, cassini.test$type_cross)

##Randomly choose 500 NE Points and remaining Events

set.seed(234)
new_data<-scaled_data%>%filter(type_cross=='NE')%>%sample_n(500)%>%rbind(scaled_data%>%filter(type_cross!='NE'))
train.idx <- sample(nrow(new_data), 2/3 * nrow(new_data))
cassini.train <- new_data[train.idx, ]
cassini.test <- new_data[-train.idx, ]

cassini.rfmodel2 <- ranger(type_cross ~ ., data = cassini.train, write.forest = TRUE)
cassini.test$pred<-predict(cassini.rfmodel2,data = cassini.test)$predictions
confusionMatrix(cassini.test$pred, cassini.test$type_cross)


#############Count of Data
head(data_Wider)
