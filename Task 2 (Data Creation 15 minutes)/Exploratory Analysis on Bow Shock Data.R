#Task 3
library(dplyr)
library(ggplot2)

bowShock<-read.csv('Data/bow_shock_only_Data.csv')

ggplot(bowShock, aes(x=type_cross , y=BTotal.nT.)) + 
  geom_boxplot(notch=TRUE)
head(sqrt(bowShock$BX_KSM.nT.^2 + bowShock$BY_KSM.nT.^2 + bowShock$BZ_KSM.nT.^2))
head(bowShock)

ggplot(bowShock, aes(x=X_KSM.km. , y=Y_KSM.km.,color=type_cross)) + 
  geom_point(pch=19)
