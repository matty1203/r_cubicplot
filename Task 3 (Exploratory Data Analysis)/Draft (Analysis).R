library(ggplot2)
library(dplyr)
library(plotly)

data_Wider<-readRDS('Full_Data_Wider.rds', refhook = NULL)
bs_data<-read.csv('Data/bow_shock_only_Data.csv')
data_copy<-data_Wider
events_data<-na.omit(data_copy,cols=c("type_cross"))


head(data_Wider)

ggplot(data_Wider, aes(x=X_KSM.km. , y=Y_KSM.km.,color=type_cross))+
  geom_point()

hist(data_Wider$BTotal.nT.,breaks = 10,col = "blue",xlab = "BTotal (in KSM Cordinate System)",main = "Histogram of BTotal")
hist(data_Wider$BX_KSM.nT.,breaks = 10,col = "green" ,xlab = "Bx (in KSM Cordinate System)",main = "Histogram of Bx")
hist(data_Wider$BY_KSM.nT. ,breaks = 10,col = "red" ,xlab = "By (in KSM Cordinate System)",main = "Histogram of By")
hist(data_Wider$BZ_KSM.nT. ,breaks = 10,col = "orange" ,xlab = "Bz (in KSM Cordinate System)",main = "Histogram of Bz")


  
ggplot(data_Wider, aes(x=X_KSM.km. , y=Y_KSM.km.))+
  geom_point(alpha=0.3)+
  geom_point(events_data, mapping =aes(x=X_KSM.km. , y=Y_KSM.km.,color=type_cross,shape=dirn_cross))

#Inward Data
inward_data<-events_data%>%filter(dirn_cross=="I")

ggplot(data_Wider, aes(x=X_KSM.km. , y=Y_KSM.km.))+
  geom_point(alpha=0.3)+
  geom_point(inward_data, mapping =aes(x=X_KSM.km. , y=Y_KSM.km.,color=type_cross))

#Outward Data
outward_data<-events_data%>%filter(dirn_cross=="O")

ggplot(data_Wider, aes(x=X_KSM.km. , y=Y_KSM.km.))+
  geom_point(alpha=0.3)+
  geom_point(outward_data, mapping =aes(x=X_KSM.km. , y=Y_KSM.km.,color=type_cross))


###Row means 

lag_col_index<-c(15,22,29,36,43,50,57,64,71,78,85,92,99,106,113)
lead_col_index<-c(120,127,134,141,148,155,162,169,176,183,190,197,204,211,218)


short_Data<-data.frame(TimeStamp=data_Wider[,1], X_KSM=data_Wider[,2],Y_KSM=data_Wider[,3],
                       Z_KSM=data_Wider[,4],
                       B_Tot=data_Wider[,5],
                       BX=data_Wider[,6],
                       BY=data_Wider[,7],
                       BZ=data_Wider[,8], 
                       type_cross=data_Wider[,10],
                       dirn_cross=data_Wider[,11],
                       Avg_Lag_Bx=rowMeans(data_Wider[,lag_col_index]),
                       Avg_Lag_By=rowMeans(data_Wider[,lag_col_index+1]),
                       Avg_Lag_Bz=rowMeans(data_Wider[,lag_col_index+2]),
                       Avg_Lag_BTot=rowMeans(data_Wider[,lag_col_index+3]),
                       Avg_Lead_Bx=rowMeans(data_Wider[,lead_col_index]),
                       Avg_Lead_By=rowMeans(data_Wider[,lead_col_index+1]),
                       Avg_Lead_Bz=rowMeans(data_Wider[,lead_col_index+2]),
                       Avg_Lead_BTot=rowMeans(data_Wider[,lead_col_index+3])
                       )


head(bs_data)
as.factor(data_Wider$type_cross)
