library(dplyr)
library(ggplot2)
library(lubridate)

data1<-read.csv('Data/15minutes_interval_boeshockData.csv')
bs_data<-read.csv('Data/bow_shock_only_Data.csv')
join_data<-readRDS('joined_data.rds', refhook = NULL)
copy_data1<-data1
copy_bs<-bs_data
head(data1)


formattedData<-copy_bs%>%select(-c(X.1,X,DOY.UTC.,SCET.s.,deltaTime.s.,date,hour_cross,minute_cross,year_cross,doy_cross,time));

for(bs in c(1:dim(copy_bs)[1])){
  temp=join_data%>%filter(TimeStamp>=bs_data$minlag[bs]&TimeStamp<=bs_data$minlead[bs])
  for(minData in c(1:dim(temp)[1])){
    if(minData!=16){
       formattedData[bs,paste("X_KSM",minData,sep = "")]<-temp[minData,'X_KSM.km.']
       formattedData[bs,paste("Y_KSM",minData,sep = "")]<-temp[minData,'Y_KSM.km.']
       formattedData[bs,paste("Z_KSM",minData,sep = "")]<-temp[minData,'Z_KSM.km.']
       formattedData[bs,paste("BX_KSM",minData,sep = "")]<-temp[minData,'BX_KSM.nT.']
       formattedData[bs,paste("BY_KSM",minData,sep = "")]<-temp[minData,'BY_KSM.nT.']
       formattedData[bs,paste("BZ_KSM",minData,sep = "")]<-temp[minData,'BZ_KSM.nT.']
       formattedData[bs,paste("BTot_KSM",minData,sep = "")]<-temp[minData,'BTotal.nT.']
    }
    
  }
  
} 


write.csv(formattedData,"15minData_Wider.csv")


final_data<-formattedData%>%select(-c('time'))%>% distinct(Timestamp.UTC., .keep_all= TRUE)