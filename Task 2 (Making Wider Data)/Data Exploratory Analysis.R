library(dplyr)
library(ggplot2)
library(lubridate)

data1<-read.csv('Data/15minData_Wider.csv')
head(data1)

ggplot(data1, aes(x=TimeStamp , y=BTotal.nT.,group = 1))+
  geom_line()+
  geom_line(aes(y = BTot_KSM2), color = "red")+
  geom_line(aes(y = BTot_KSM31), color = "blue")
  

ggplot(data1, aes(y=BTotal.nT.))+
  geom_boxplot()


ggplot(data1, aes(y=BTot_KSM2))+
  geom_boxplot()

