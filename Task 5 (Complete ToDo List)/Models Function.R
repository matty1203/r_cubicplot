# Models Run Function Script

library(tidyverse)
library(tidyr)
library(ggplot2)
library(dplyr)
library(caret)
library(ranger)
library(matrixStats)
library(MASS)

##Data: Only considering 15 minutes before

data<-readRDS('input_datasetFinal.rds', refhook = NULL)
glimpse(data)
dat_15<-data[,1:107]

##Data : Average mean and SD 