library(tidyverse)
library(tidyr)
library(ggplot2)
library(dplyr)
library(caret)
library(ranger)
library(matrixStats)
library(MASS)

data_wider<-readRDS('input_dataset.rds', refhook = NULL)
copy_data<-data_wider
copy_data<-dplyr::select(data_wider, !ends_with("15"))
head(newData_creator(copy_data))


#copy_data[17,'BTotal.nT.']
newData_creator<-function(data_ip){
  total<-(dim(data_ip)[1]-16)
  for(data_ind in c(1:total )){
 
    out <- paste0("Completed ",data_ind, "/",total)
    print(data_ind)
    datIndex<-data_ind+1
    data_ip[data_ind,"X_KSM16"]<-data_ip[datIndex,'X_KSM.km.']
    data_ip[data_ind,"Y_KSM16"]<-data_ip[datIndex,'Y_KSM.km.']
    data_ip[data_ind,"Z_KSM16"]<-data_ip[datIndex,'Z_KSM.km.']
    data_ip[data_ind,"BX_KSM16"]<-data_ip[datIndex,'BX_KSM.nT.']
    data_ip[data_ind,"BY_KSM16"]<-data_ip[datIndex,'BY_KSM.nT.']
    data_ip[data_ind,"BZ_KSM16"]<-data_ip[datIndex,'BZ_KSM.nT.']
    data_ip[data_ind,"BTot_KSM16"]<-data_ip[datIndex,'BTotal.nT.']
     
    # New Data Index
  }
  print("Saving Data as a RDS File.....")
  saveRDS(data_ip,"input_datasetFinal.rds")
  print("Function Completed.....")
  return(data_ip)
}


