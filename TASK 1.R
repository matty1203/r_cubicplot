library(dplyr)
data1<-readRDS('Full_Cassini_Master_MP_BS_CMJ_revised2005.rds', refhook = NULL)
data2<-readRDS('Cass_data2005.rds', refhook = NULL)

head(data1)
head(data2)
#as.POSIXct(paste(data1$))
max(data1$doy_cross)

##min and max of crossing year
min(data1$year_cross)
max(data1$year_cross)
###converting day into date 
as.Date(365,origin="2005-01-01")

