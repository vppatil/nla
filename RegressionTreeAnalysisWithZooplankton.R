setwd("~/Dropbox/NLA analyses (1)/data")
library(rpart)
library(randomForest)
nla<-read.csv('NLAzooplankton.csv', header=TRUE)

pc.cov.names<-names(nla)[grep('PCT',names(nla))] #extracting landcover variable names
CHLA.sub<-subset(nla,select=c(pc.cov.names,'CHLA','LAT_DD','LON_DD','URBAN','LAKE_ORIGIN_00_1Manmade','AREAHA', 'avgDen', 'num_species')) 
PTL.sub<-subset(nla,select=c(pc.cov.names,'PTL','LAT_DD','LON_DD','URBAN','LAKE_ORIGIN_00_1Manmade','AREAHA', 'avgDen', 'num_species')) 

#generate random forest
CHLA.rf<-randomForest(log(CHLA)~.,data=CHLA.sub,na.action=na.omit)
PTL.rf<-randomForest(log(PTL)~.,data=PTL.sub,na.action=na.omit)

#generate linear models
CHLA.glm<-lm(log(CHLA)~.,data=CHLA.sub,na.action=na.omit)
PTL.glm<-lm(log(PTL)~.,data=PTL.sub,na.action=na.omit)

model.list<-list(list(summary(CHLA.glm),CHLA.rf),list(summary(PTL.glm),PTL.rf))
model.list

mean(nla$num_species[nla$LAKE_ORIGIN_00_1Manmade==0])
mean(nla$num_species[nla$LAKE_ORIGIN_00_1Manmade==1])





