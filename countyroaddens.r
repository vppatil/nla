
  len<-function(startx,starty,endx,endy)
  {
    len<-sqrt((endy-starty)^2+(endx-startx)^2)
    return(len)
  }  
  
roadlen<-function(coords)
{
  c.dim<-dim(coords)
  if(c.dim[1]==2){starts = coords[1,];ends=coords[2,];
                  dist=sqrt((ends[1]-starts[1])^2+(ends[2]-starts[2])^2)} else{	
                    starts<-coords[1:c.dim[1]-1,]
                    ends<-coords[2:c.dim[1],]
                    dists<-len(starts[,1],starts[,2],ends[,1],ends[,2])
                    dist<-sum(dists)}
  return(dist)
}

road.lencalc<-function(roadsshp)
{
  
  coords<-coordinates(roadsshp)
  coords<-coords[[1]]
  
  roadsum<-sapply(coords,roadlen)
  roadsum<-sum(roadsum)
  return(roadsum)
}

library(sp)
library(shapefiles)
library(rgeos)
library(rgdal)
library(maptools)
library(fastshp)

roadfilepath=('C:/users/brad griffith/documents/roads')
#roadfilepath=('/media/vijay/OS/Users/Sam/Documents/roads' )

setwd('C:/Users/Brad Griffith/Dropbox/NLA analyses/data/shapefiles/nlaNHD')