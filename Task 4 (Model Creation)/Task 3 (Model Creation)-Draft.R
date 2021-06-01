library(tree)
library(tidyr)
library(ggplot2)
library(dplyr)

data_Wider<-readRDS('Full_Data_Wider.rds', refhook = NULL)
avg_data<-readRDS('Average_Data.rds', refhook = NULL)


data_Wider$type_cross<-replace_na(data_Wider$type_cross,"NE")
data_Wider$dirn_cross<-replace_na(data_Wider$dirn_cross,"UD")


tree <- tree(type_cross ~ ., data=avg_data)


summary(tree)                                                                   s 


ggplot(data = data_Wider) +
  geom_bar(mapping = aes(x = type_cross, fill=dirn_cross))


ggplot(data = data_Wider) +
  geom_bar(mapping = aes(x = type_cross, fill=dirn_cross) , position = "fill")



head(data_Wider$type_cross)


sum(is.na(data_Wider))

View(data_Wider%>%filter(type_cross=="DG"))

View(data_Wider%>%filter(type_cross=="BS"))





#####Data Wider
cleaned_data<-data_Wider%>%select(c('X_KSM.km.','Y_KSM.km.','Z_KSM.km.','BTotal.nT.','type_cross','BTot_KSM1','BTot_KSM2','BTot_KSM3',
                                    'BTot_KSM4','BTot_KSM5','BTot_KSM6','BTot_KSM7','BTot_KSM8','BTot_KSM9','BTot_KSM10','BTot_KSM11',
                                    'BTot_KSM12','BTot_KSM13','BTot_KSM14','BTot_KSM15','BTot_KSM17','BTot_KSM18','BTot_KSM19','BTot_KSM20',
                                    'BTot_KSM21','BTot_KSM22','BTot_KSM23','BTot_KSM24','BTot_KSM25','BTot_KSM26','BTot_KSM27','BTot_KSM28',
                                    'BTot_KSM29','BTot_KSM30','BTot_KSM31'))


cleaned_data$type_cross<-as.factor(cleaned_data$type_cross)
tree <- tree(type_cross ~ ., data=cleaned_data)
summary(tree)


plot(tree)
text(tree, cex = .5, pretty = 0)
