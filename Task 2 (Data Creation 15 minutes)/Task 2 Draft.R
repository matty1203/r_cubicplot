library(dplyr)
library(ggplot2)
library(lubridate)

data1<-readRDS('Data/Full_Cassini_Master_MP_BS_CMJ_revised2005.rds', refhook = NULL)
data2<-readRDS('Data/Cass_data_ksm2005.rds', refhook = NULL)
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
#copy_data2<-copy_data2%>%select(-c(TimeStamp))

######### Merging with time variables 

joinedData <- left_join(copy_data2,copy_data1,by=c('date','hour_cross','minute_cross'))
copy_joined<-joinedData
bowShock<-filter(joinedData,type_cross=="BS")%>%select(c('TimeStamp','Timestamp.UTC.','hour_cross','minute_cross','year_cross','doy_cross'))
bowShock$minlag<-bowShock$TimeStamp-(15*60)
bowShock$minlead<-bowShock$TimeStamp+(15*60)
write.csv(bowShock,"bow_shock_only_Data.csv")
#######New Task of Finding 15 minutes before and after Bow Shock


d<-copy_joined%>%filter(as.Date(TimeStamp)>=bowShock$minlag[1]&as.Date(TimeStamp)<=bowShock$minlead[1])
formattedData<-NULL;
for(bs in c(1:dim(bowShock)[1])){
  temp=copy_joined%>%filter(TimeStamp>=bowShock$minlag[bs]&TimeStamp<=bowShock$minlead[bs])
  if(bs==1){
      formattedData<-temp;
  }
  else{
    formattedData <- rbind(formattedData, temp)
  }
  
} 

final_data<-formattedData%>%select(-c('time'))%>% distinct(Timestamp.UTC., .keep_all= TRUE)

write.csv(final_data,"15minutes_interval_boeshockData.csv")

head(bowShock)
head(copy_joined)
head(formattedData)
