
library(tidyverse)
library(tidyr)
library(ggplot2)
library(dplyr)
library(caret)
library(ranger)
library(matrixStats)
library(MASS)

dat<-readRDS('input_datasetFinal.rds', refhook = NULL)
dim(dat)
cpy_data<-dat
###Row means 

lag_col_index<-c(10,17,24,31,38,45,52,59,63,70,77,84,91,98,105)
lead_col_index<-c(112,119,126,133,140,147,154,161,168,175,183,190,197,204)


short_Data<-data.frame(X_KSM=cpy_data[,1],Y_KSM=cpy_data[,2],
                       Z_KSM=cpy_data[,3],
                       B_Tot=cpy_data[,4],
                       BX=cpy_data[,5],
                       BY=cpy_data[,6],
                       BZ=cpy_data[,7], 
                       type_cross=cpy_data[,8],
                       dirn_cross=cpy_data[,9],
                       Avg_Lag_Bx=rowMeans(cpy_data[,lag_col_index]),
                       Avg_Lag_By=rowMeans(cpy_data[,lag_col_index+1]),
                       Avg_Lag_Bz=rowMeans(cpy_data[,lag_col_index+2]),
                       Avg_Lag_BTot=rowMeans(cpy_data[,lag_col_index+3]),
                       
                       Avg_Lead_Bx=rowMeans(cpy_data[,lead_col_index]),
                       Avg_Lead_By=rowMeans(cpy_data[,lead_col_index+1]),
                       Avg_Lead_Bz=rowMeans(cpy_data[,lead_col_index+2]),
                       Avg_Lead_BTot=rowMeans(cpy_data[,lead_col_index+3])
                      
                                              )



shortSD_Data<-data.frame(X_KSM=cpy_data[,1],Y_KSM=cpy_data[,2],
                       Z_KSM=cpy_data[,3],
                       B_Tot=cpy_data[,4],
                       BX=cpy_data[,5],
                       BY=cpy_data[,6],
                       BZ=cpy_data[,7], 
                       type_cross=cpy_data[,8],
                       dirn_cross=cpy_data[,9],
                       Avg_Lag_Bx=rowMeans(cpy_data[,lag_col_index]),
                       Avg_Lag_By=rowMeans(cpy_data[,lag_col_index+1]),
                       Avg_Lag_Bz=rowMeans(cpy_data[,lag_col_index+2]),
                       Avg_Lag_BTot=rowMeans(cpy_data[,lag_col_index+3]),
                       
                       SD_Lag_Bx=rowSds(as.matrix(cpy_data[,lag_col_index])),
                       SD_Lag_By=rowSds(as.matrix(cpy_data[,lag_col_index+1])),
                       SD_Lag_Bz=rowSds(as.matrix(cpy_data[,lag_col_index+2])),
                       SD_Lag_BTot=rowSds(as.matrix(cpy_data[,lag_col_index+3])),
                       
                       Avg_Lead_Bx=rowMeans(cpy_data[,lead_col_index]),
                       Avg_Lead_By=rowMeans(cpy_data[,lead_col_index+1]),
                       Avg_Lead_Bz=rowMeans(cpy_data[,lead_col_index+2]),
                       Avg_Lead_BTot=rowMeans(cpy_data[,lead_col_index+3]),
                       
                       SD_Lead_Bx=rowSds(as.matrix(cpy_data[,lead_col_index])),
                       SD_Lead_By=rowSds(as.matrix(cpy_data[,lead_col_index+1])),
                       SD_Lead_Bz=rowSds(as.matrix(cpy_data[,lead_col_index+2])),
                       SD_Lead_BTot=rowSds(as.matrix(cpy_data[,lead_col_index+3]))
                       
)



head(shortSD_Data)
saveRDS(short_Data,"Average_Data.rds")

saveRDS(shortSD_Data,"Average_SD_Data.rds")




