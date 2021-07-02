library(tidyverse)
library(caret)
library(ranger)
library(matrixStats)
a <-readRDS('Full_Data_Wider.rds', refhook = NULL)
glimpse(a)
a %>% select("type_cross") %>% table()
a %>% select(starts_with("BTot_KSM")) %>% rowMeans() %>%head()
a <- a %>% mutate(BTotmean = rowMeans(select(., starts_with("BTot_KSM"))), BTotsd =rowSds(as.matrix(select(., starts_with("BTot_KSM")))))
a %>% filter(!is.na(type_cross))%>% 
  ggplot(aes(x=BTotmean, y=BTotsd))+
  geom_point()+facet_wrap(~type_cross)
a %>% filter(!is.na(type_cross))%>% 
  ggplot(aes(x=BTotmean, y=BTotsd, col =type_cross))+
  geom_point()

#a %>% select(starts_with("BTot_KSM")) %>% filter(a$type_cross=="MP")%>% t() %>%matplot(type = "l")
a %>% select(starts_with("BTot_KSM"), type_cross) %>% 
  filter(type_cross=="MP"|type_cross=="BS")%>% 
  mutate(id = 1:(203+245))  %>%
  pivot_longer(!c(id, type_cross)) %>% 
  ggplot(aes(x=name, y=value, group = id))+
  geom_line()+facet_wrap(~type_cross)

###Model Creation

sum(is.na(a))

a=na.omit(a)
dim(a)

head(a)

a$type_cross<-replace_na(a$type_cross,"NE")
a$dirn_cross<-replace_na(a$dirn_cross,"UD")

df = subset(a, select = -c(TimeStamp,Timestamp.UTC.) )

colnames(df)

df$type_cross=as.factor(df$type_cross)


saveRDS(df, "input_dataset.rds")

rf <- ranger(type_cross ~ ., data = df, write.forest = TRUE)
getTerminalNodeIDs(rf, iris)