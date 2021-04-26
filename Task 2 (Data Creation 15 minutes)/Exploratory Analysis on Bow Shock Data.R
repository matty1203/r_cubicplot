#Task 3
library(dplyr)
library(ggplot2)

bowShock<-read.csv('Data/bow_shock_only_Data.csv')
finalData<-read.csv('Data/15minutes_interval_boeshockData.csv')

ggplot(finalData, aes(x=type_cross , y=BTotal.nT.)) + 
  geom_boxplot(notch=TRUE)

ggplot(finalData, aes(x=type_cross , y=BX_KSM.nT.)) + 
  geom_boxplot(notch=TRUE)
ggplot(finalData, aes(x=type_cross , y=BY_KSM.nT.)) + 
  geom_boxplot(notch=TRUE)
ggplot(finalData, aes(x=type_cross , y=BZ_KSM.nT.)) + 
  geom_boxplot(notch=TRUE)



head(sqrt(bowShock$BX_KSM.nT.^2 + bowShock$BY_KSM.nT.^2 + bowShock$BZ_KSM.nT.^2))
head(bowShock)

ggplot(finalData, aes(x=X_KSM.km. , y=Y_KSM.km.),size=8) + 
  geom_point(alpha=0.3) + 
  geom_point(bowShock, mapping =aes(x=X_KSM.km. , y=Y_KSM.km.,color='red'))
