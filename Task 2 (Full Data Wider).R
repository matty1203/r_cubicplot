library(dplyr)
library(ggplot2)
library(lubridate)

join_data<-readRDS('./Data/joined_data.rds', refhook = NULL)
head(join_data)

copy_data<-join_data%>%select(-c(X,DOY.UTC.,SCET.s.,deltaTime.s.,date,hour_cross,minute_cross,year_cross,doy_cross,time,xcrosslist,ycrosslist,zcrosslist,doyfrac_cross));
head(copy_data)
copy_data<-copy_data%>%select(-c(xcrosslist,ycrosslist,zcrosslist));
head(copy_data)
test<-copy_data[1:200,]


formattedData<-rbind(formattedData,copy_data[16,])

formattedData<-data.frame();
i<-0;
for(data_ind in c(16:dim(copy_data)[1])){
  #count restarts for each column in new row
  diff_min<-1;
  
  formattedData<-rbind(formattedData,copy_data[data_ind,])
  
  for(time_ind in c((data_ind-15):(data_ind+15))){
    if(time_ind!=data_ind){
      
      formattedData[i,paste("X_KSM",diff_min,sep = "")]<-copy_data[time_ind,'X_KSM.km.']
      formattedData[i,paste("Y_KSM",diff_min,sep = "")]<-copy_data[time_ind,'Y_KSM.km.']
      formattedData[i,paste("Z_KSM",diff_min,sep = "")]<-copy_data[time_ind,'Z_KSM.km.']
      formattedData[i,paste("BX_KSM",diff_min,sep = "")]<-copy_data[time_ind,'BX_KSM.nT.']
      formattedData[i,paste("BY_KSM",diff_min,sep = "")]<-copy_data[time_ind,'BY_KSM.nT.']
      formattedData[i,paste("BZ_KSM",diff_min,sep = "")]<-copy_data[time_ind,'BZ_KSM.nT.']
      formattedData[i,paste("BTot_KSM",diff_min,sep = "")]<-copy_data[time_ind,'BTotal.nT.']
      
    }
    # else{
    #   
    #   formattedData[i,"X_KSM"]<-copy_data[time_ind,'X_KSM.km.']
    #   formattedData[i,"Y_KSM"]<-copy_data[time_ind,'Y_KSM.km.']
    #   formattedData[i,"Z_KSM"]<-copy_data[time_ind,'Z_KSM.km.']
    #   formattedData[i,"BX_KSM"]<-copy_data[time_ind,'BX_KSM.nT.']
    #   formattedData[i,"BY_KSM"]<-copy_data[time_ind,'BY_KSM.nT.']
    #   formattedData[i,"BZ_KSM"]<-copy_data[time_ind,'BZ_KSM.nT.']
    #   formattedData[i,"BTot_KSM"]<-copy_data[time_ind,'BTotal.nT.']
    #   formattedData[i,"Timestamp"]<-copy_data[time_ind,'Timestamp.UTC.']
    #   formattedData[i,"type_cross"]<-copy_data[time_ind,'type_cross']
    #   formattedData[i,"dirn_cross"]<-copy_data[time_ind,'dirn_cross']
    #   
    # }
  }
  i=i+1; 
  # New Data Index
}

newData_creator<-function(data_ip){
  data_created<-data.frame();
  i<-0;
  total<-(dim(data_ip)[1]-16)
  for(data_ind in c(16:total )){
    #count restarts for each column in new row
    diff_min<-1;
    out <- paste0("Completed ",data_ind, "/",total)
    print(out)
    data_created<-dplyr::bind_rows(data_created,data_ip[data_ind,])
    
    for(time_ind in c((data_ind-15):(data_ind+15))){
      if(time_ind!=data_ind){
        
        data_created[i,paste("X_KSM",diff_min,sep = "")]<-data_ip[time_ind,'X_KSM.km.']
        data_created[i,paste("Y_KSM",diff_min,sep = "")]<-data_ip[time_ind,'Y_KSM.km.']
        data_created[i,paste("Z_KSM",diff_min,sep = "")]<-data_ip[time_ind,'Z_KSM.km.']
        data_created[i,paste("BX_KSM",diff_min,sep = "")]<-data_ip[time_ind,'BX_KSM.nT.']
        data_created[i,paste("BY_KSM",diff_min,sep = "")]<-data_ip[time_ind,'BY_KSM.nT.']
        data_created[i,paste("BZ_KSM",diff_min,sep = "")]<-data_ip[time_ind,'BZ_KSM.nT.']
        data_created[i,paste("BTot_KSM",diff_min,sep = "")]<-data_ip[time_ind,'BTotal.nT.']
        
      }
      diff_min=diff_min+1;
    }
    i=i+1; 
    # New Data Index
  }
  
  print("Saving Data as a RDS File.....")
  saveRDS(data_created,"Full_Data_Wider.rds")
  print("Function Completed.....")
  return(data_created)
}


res<-newData_creator(copy_data)
