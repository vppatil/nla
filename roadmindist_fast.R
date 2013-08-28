#simplified script to calculate distance to the nearest road for points spaced at 50m intervals around the perimeters of NLA lakes.
#script does not 
#set wd to NLA Analyses

#works at the watershed scale now.
library(sp)
library(shapefiles)
library(rgeos)
library(rgdal)
library(maptools)
library(raster)
library(fastshp)

roadproxpoints<-readOGR('../data/shapefiles/nlaNHD','roadproximitypoints')
bigbuffer.nad83<-readOGR('../data/shapefiles/nlaWatersheds','NLA_Sites_Sampled_DrainageBasins')
bigbuffer<-spTransform(bigbuffer.nad83,CRS('+proj=cea'))

#directory containing tiger shapefiles downloaded with ftpdownloadroads.r
#roadfilepath<-'c:/users/brad griffith/documents'

#roadfilepath<-'D:/BigDatasets/TIGER2012'
roadfilepath<-'/media/vijay/OS/Users/Sam/Documents/roads' 

roadfiles<-dir(path=roadfilepath,pattern='shp')
roadfiles<-roadfiles[-grep('.xml',roadfiles)] 
roadfiles<-gsub('.shp','',roadfiles)

roadextent.list<-list()
for(j in 1:length(roadfiles))
{
  road.fast<-read.shp(paste(roadfilepath,paste(roadfiles[j],'.shp',sep=''),sep='/'),format='table')
  xmin=min(road.fast$x)
  xmax=max(road.fast$x)
  ymin=min(road.fast$y)
  ymax=max(road.fast$y)
  roadextent.list[[j]]=extent(xmin,xmax,ymin,ymax)
}

lakeextent.list<-list()
for(j in 1:dim(bigbuffer.nad83)[1])
{
  lakebuffer.bbox<-summary(bigbuffer.nad83[j,])$bbox
  xmin=lakebuffer.bbox[1,1]
  xmax=lakebuffer.bbox[1,2]
  ymin=lakebuffer.bbox[2,1]
  ymax=lakebuffer.bbox[2,2]
  lakeextent.list[[j]]=extent(xmin,xmax,ymin,ymax)
}

lake.road.overlap<-list()
for(i in c(1:length(lakeextent.list)))
{
  lake.road.overlap[[i]] = vector()
  for(j in 1:length(roadextent.list))
  {
    if(!is.null(intersect(roadextent.list[[j]],lakeextent.list[[i]])))
      lake.road.overlap[[i]]<-c(lake.road.overlap[[i]],j)
  }
}

#now you have a list with the same number of elements as there are lakes
#each element returns a vector of road files to compare against. These roadfiles have overlapping extents with the lake in question.


#nested loop to compare each roadfile with each point.
#set up so it can be broken apart by roadfile or by lake.
library(sp)
library(shapefiles)
library(rgeos)
library(rgdal)
library(maptools)
library(raster)
library(fastshp)
library(foreach)
  numroadfiles=length(roadfiles)
  lakes<-bigbuffer.nad83$SITEID
  numlakes=length(lakes)
  
  options(warn=-1)
 

library(snow)
library(Rmpi)
lakes<-as.character(lakes)
roadproxpoints<-spTransform(roadproxpoints,CRS('+proj=cea'))
distance.df<-data.frame(SITEID='',mean.min.dist.to.road=0,num.points=0)
write.csv(distance.df,'~/Ubuntu One/gleon/roadProximityResultsWatershed_cluster.csv')
setwd(roadfilepath)
roadmindist<-function(j)
  {
    lake.points<-subset(roadproxpoints,roadproxpoints$SITE_ID == lakes[j]) #subset of points shapefile
    numpoints<-dim(lake.points)[1]
    distances<-vector('numeric',length=numpoints)
       print(j/numlakes)
	buffer.sub<-subset(bigbuffer,bigbuffer$SITEID==lakes[j])
    roadfile.indices=lake.road.overlap[[j]]
    for(r in roadfile.indices)
    {
      try(rm('roads'),silent=T)
      roads<-readShapeLines(paste(roadfiles[r],'.shp',sep=''),proj4string=CRS("+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0"))
      roads<-spTransform(roads,CRS(proj4string(roadproxpoints)))
      roads<-gIntersection(roads,buffer.sub)

        for(i in 1:numpoints)
        {
          mindist<-gDistance(lake.points[i,],roads)
          distances[i]<-ifelse(mindist<distances[i] | distances[i] == 0,mindist,distances[i]) #replacing the minimum distance to road at each point if a closer road is found
        }

    }
    distance.df<-data.frame(SITEID=lakes[j],mean.min.dist.to.road=mean(distances),num.points=numpoints)
    write.table(distance.df,'~/Ubuntu One/gleon/roadProximityResultsWatershed_cluster.csv',quote=F,row.names=F,col.names=F,sep=',',append=T)
  }

cl<-makeCluster(6,type='MPI')
clusterExport(cl,ls())

j=1:length(lakes)
clusterApplyLB(cl,j,roadmindist)
stopCluster(cl)





