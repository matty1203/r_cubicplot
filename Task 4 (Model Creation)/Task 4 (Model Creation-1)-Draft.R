library(tidyr)
library(ggplot2)
library(dplyr)
library(caret)
library(ranger)
library(matrixStats)

data_Wider<-readRDS('input_dataset.rds', refhook = NULL)
copy_data<-data_Wider

scaled_data<-copy_data%>%mutate_if(is.numeric,scale)


set.seed(3100)
scaled_data<-scale(data_Wider)



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
