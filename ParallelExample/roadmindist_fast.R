#simplified script to calculate distance to the nearest road for points spaced at 50m intervals around the perimeters of NLA lakes.
#script does not 
#set wd to NLA Analyses
library(sp)
library(shapefiles)
library(rgeos)
library(rgdal)
library(maptools)
library(raster)
library(fastshp)

roadproxpoints<-readOGR('data/shapefiles/nlanhd','roadproximitypoints')
bigbuffer.nad83<-readOGR('data/shapefiles/nlaBuffers','NLA10kmBuff')
bigbuffer<-spTransform(bigbuffer.nad83,CRS(proj4string(roadproxpoints)))

 #directory containing tiger shapefiles downloaded with ftpdownloadroads.r
roadfilepath<-'.'
roadfiles<-dir(pattern='shp')
roadfiles<-roadfiles[-grep('.xml',roadfiles)] 
roadfiles<-gsub('.shp','',roadfiles)

roadextent.list<-list()
for(i in 1:length(roadfiles))
{
  road.fast<-read.shp(paste(roadfiles[i],'.shp',sep=''),format='table')
  xmin=min(road.fast$x)
  xmax=max(road.fast$x)
  ymin=min(road.fast$y)
  ymax=max(road.fast$y)
  roadextent.list[[i]]=extent(xmin,xmax,ymin,ymax)
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
for(i in 1:length(lakeextent.list))
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

  numroadfiles=length(roadfiles)

  lakes<-bigbuffer.nad83$SITE_ID
  numlakes=length(lakes)
  
  options(warn=-1)
  for(i in 1:numlakes)
  {

    lake.points<-roadproxpoints[roadproxpoints$SITE_ID == lakes[i],] #subset of points shapefile
    numpoints<-dim(lake.points)[1]
    distances<-vector('numeric',length=numpoints)
    
    roadfile.indices=lake.road.overlap[[i]]
    for(r in roadfile.indices)
    {
      roads<-readOGR('.',roadfiles[r],p4s="+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0")
      roads<-spTransform(roads,CRS(proj4string(roadproxpoints)))

        for(j in 1:numpoints)
        {
          mindist<-gDistance(lake.points[j,],roads)
          distances[j]<-ifelse(mindist<distances[j],mindist,distances[j]) #replacing the minimum distance to road at each point if a closer road is found
        }

    }
    distance.df<-data.frame(SITEID=lakes[i],mean.min.dist.to.road=mean(distances),num.points=numpoints)
    write.table(distance.df,'roadProximityResults.csv',quote=F,row.names=F,col.names=F,sep=',',append=T)
  }
}


#after comparing all road files to all lakepoints, the data can be combined to find the true minimum distance to nearest road for each point and calculated averages for each lake.
distances<-read.csv('roadProximityResults.csv',header=F)
names(distances)=c('SITE_ID','dist.nearest.road','point.num','roadfile.num')
distances<-tapply(distances$dist.nearest.road,list(distances$SITE_ID,distances$point.num),min)
mean.mindistance<-apply(distances,1,function(x) mean(x[!is.na(x)]))
mean.mindistance<-data.frame(SITE_ID=names(mean.mindistance),mean.roadproximity=mean.mindistance)

