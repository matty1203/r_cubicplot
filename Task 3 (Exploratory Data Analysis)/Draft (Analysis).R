library(ggplot2)
library(dplyr)
library(plotly)
library(rgl)

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

saveRDS(short_Data,"Average_Data.rds")



###################SHORT DATA Exploratory Data Analysis

###Data Imbalance

counts.with_na <- table(short_Data$type_cross,useNA = "always")
counts.without_na<-table(short_Data$type_cross)
barplot(counts.with_na,names.arg=c("BS", "DG", "MP","SC","NA"))
barplot(counts.without_na)


ggplot(data = short_Data) +
  geom_bar(mapping = aes(x = type_cross, fill=dirn_cross) , position = "fill")

###Direction

ggplot(events_data, aes(type_cross, fill=dirn_cross )) +
  geom_bar(stat="count")



ggplot(data = events_data) +
  geom_bar(mapping = aes(x = type_cross, fill=dirn_cross) , position = "fill")

###Checking Outliers


##only bowshock Data
ggplot(data=bs_data, aes(x=type_cross, y=B_Tot)) +
  geom_boxplot()

##Overall Bx Data
ggplot(data=short_Data, aes(B_Tot)) +
  geom_boxplot()

#### Checking Correlation


bs_data<-na.omit(short_Data,cols=c("type_cross"))


ggplot(short_Data)+
  geom_point(alpha=0.3)


ggplot(data=short_Data, aes(x=TimeStamp, y=BX, group=1)) +
  geom_line(color="blue", size=1.2)


ggplot(data=bs_data, aes(x=TimeStamp, group=1)) +
  geom_line(aes(y = BX), color = "red")+
  geom_line(aes(y = Avg_Lag_Bx), color = "green")



ggplot(data=bs_data, aes(x=BX, y=Avg_Lag_Bx,color=type_cross)) +
  geom_point()





head(bs_data)
as.factor(data_Wider$type_cross)
