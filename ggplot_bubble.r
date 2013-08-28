library(ggplot2)
nla<-read.csv('nla_all_connectivity.csv',header=T) #updated with road metrics

#simple bubble plot where point size is proportional to a variable's value
p<-ggplot(nla,aes(x=ALBERS_X,y=ALBERS_Y,size=(DEPTHMAX)))+geom_point(colour='white',fill='red',shape=21)
plot(p)

p2<-ggplot(nla,aes(x=ALBERS_X,y=ALBERS_Y,size=(log(CL)))+geom_point(colour='white',fill='red',shape=21)
plot(p,add=T)

p3<-ggplot(nla,aes(x=ALBERS_X,y=ALBERS_Y,color=WSA_ECO9.x))+geom_point(shape=19)

conn.cols<-vector()
conn.cols<-ifelse(nla$connectivity_class=='SE','red',ifelse(nla$connectivity_class == 'HW','blue',ifelse(nla$connectivity_class == 'STLA','green','purple')))

map('state')
points(LAT_DD.y~LON_DD.y,data=nla,col=conn.cols,pch=19,cex=(log(nla$PCT_FOREST_BSN)+2)/3)

ne<-subset(nla,nla$WSA_ECO9.x %in% c('NAP','SAP','UMW'))
