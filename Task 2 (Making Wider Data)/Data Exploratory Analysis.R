library(dplyr)
library(ggplot2)
library(lubridate)

library(corrplot)
library("PerformanceAnalytics")

data1<-read.csv('Data/15minData_Wider.csv')
head(data1)

ggplot(data1, aes(x=TimeStamp , y=BTotal.nT.,group = 1))+
  geom_line()+
  geom_line(aes(y = BTot_KSM2), color = "red")+
  geom_line(aes(y = BTot_KSM31), color = "blue")
  

ggplot(data1, aes(x=TimeStamp , y=BX_KSM.nT.,group = 1))+
  geom_line(color="orange",size=2)+
  geom_line(aes(y = BX_KSM15), color = "red")+
  geom_line(aes(y = BX_KSM17), color = "blue")

ggplot(data1, aes(x=TimeStamp , y=BY_KSM.nT.,group = 1))+
  geom_line(color="green",size=2)+
  geom_line(aes(y = BY_KSM2), color = "red")+
  geom_line(aes(y = BY_KSM31), color = "blue")

  ggplot(data1, aes(x=TimeStamp , y=BX_KSM.nT.,group = 1))+
  geom_line(color="orange",size=2)+
  geom_line(aes(y = BX_KSM2), color = "red")+
  geom_line(aes(y = BX_KSM31), color = "blue")


ggplot(data1, aes(x=dirn_cross ,y=BTotal.nT.))+
  geom_boxplot()

hist(data1$BX_KSM.nT.)
hist(data1$BTotal.nT.)
hist(data1$BTot_KSM31)

chart.Correlation(data1, histogram=TRUE, pch=19)



ggplot(data1, aes(x=dirn_cross, y=BTot_KSM2))+
  geom_boxplot()

