library(dplyr)
library(ggplot2)
data1<-readRDS('./Data/Full_Cassini_Master_MP_BS_CMJ_revised2005.rds', refhook = NULL)
data2<-readRDS('./Data/Cass_data2005.rds', refhook = NULL)
copy_data1<-data1
copy_data2<-data2

head(data1)
head(data2)

###################### Formatting Date in Data1

copy_data1$date<-as.Date(copy_data1$doy_cross,origin="2005-01-01")
head(copy_data1)
copy_data1$time<-paste(copy_data1$hour_cross,copy_data1$minute_cross,"00",sep=":")
#copy_data1$TimeStamp<-as.POSIXct(paste(copy_data1$date, copy_data1$time), format="%Y-%m-%d %H:%M:%S")
#copy_data1$DateOnly=format(copy_data1$date, "%Y-%m-%d" )
#copy_data1<-copy_data1%>%select(-c(date,time))

###################### Formatting Date in Data2

head(copy_data2)
copy_data2$TimeStamp<-as.POSIXct(copy_data2$Timestamp.UTC., format="%d/%m/%Y %H:%M")
copy_data2$date<-as.Date(format(copy_data2$TimeStamp, "%Y-%m-%d" ))
copy_data2$hour_cross<-as.integer(format(copy_data2$TimeStamp, "%H" )) 
copy_data2$minute_cross<-as.integer(format(copy_data2$TimeStamp, "%M" )) 
copy_data2<-copy_data2%>%select(-c(TimeStamp))

######### Merging with time variables 

leftJoinDf <- left_join(copy_data2,copy_data1,by=c('date','hour_cross','minute_cross'))
head(leftJoinDf)


###Finding all points in MAG 2005 between day 72 and 74

d1<-as.Date(72,origin="2005-01-01")
d2<-as.Date(73,origin="2005-01-01")

temp<-filter(leftJoinDf,between(date,d1,d2))
bowShock<-filter(temp,type_cross=="BS")$TimeStamp
MagP<-filter(temp,type_cross=="MP")$TimeStamp


#####Plotting Data
ggplot(data=temp, aes(x=TimeStamp , y=abs(BTotal.nT.))) +
  geom_line()+
  ylab("|B|(nT)")+
  geom_vline(xintercept=bowShock, linetype="dotted",colour="blue",size=1.3)+
  geom_vline(xintercept=MagP, linetype="dotted",colour="red",size=1.3)+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

  
