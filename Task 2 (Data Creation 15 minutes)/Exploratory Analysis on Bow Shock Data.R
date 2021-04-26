#Task 3
library(dplyr)
library(ggplot2)

bowShock<-read.csv('Data/Bow_Shock_Data.csv')

ggplot(bowShock, aes(x=type_cross , y=BTotal.nT.)) + 
  geom_boxplot(notch=TRUE)
