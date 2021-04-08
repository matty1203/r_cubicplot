library(dplyr)
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
copy_data1$TimeStamp<-as.POSIXct(paste(copy_data1$date, copy_data1$time), format="%Y-%m-%d %H:%M:%S")
copy_data1$TimeStamp=format(copy_data1$TimeStamp, "%Y-%m-%d %H:%M:%S" )
copy_data1<-copy_data1%>%select(-c(date,time))

###################### Formatting Date in Data2

head(copy_data2)
copy_data2$TimeStamp<-as.POSIXct(copy_data2$Timestamp.UTC., format="%d/%m/%Y %H:%M:%S")
copy_data2$TimeStamp<-format(copy_data2$TimeStamp, "%Y-%m-%d %H:%M:%S" )


######### Merging with time variables 

