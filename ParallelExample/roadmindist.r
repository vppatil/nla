#simplified script to calculate distance to the nearest road for points spaced at 50m intervals around the perimeters of NLA lakes.
#script does not 


library(sp)
library(shapefiles)
library(rgeos)
library(rgdal)
library(maptools)
library(raster)

roadproxpoints<-readShapePoints('data/shapefiles/nlaNHD.roadproximitypoints.shp',proj4string=CRS('+proj=cea'))
roadproxpoints<-readOGR('data/shapefiles/nlaNHD','roadproximitypoints')


roadfilepath<-'C:/users/brad griffith/Documents/roads/' #directory containing tiger shapefiles downloaded with ftpdownloadroads.r
roadfiles<-dir(path=roadfilepath,pattern='shp')
roadfiles<-roadfiles[-grep('.xml',roadfiles)] 
#roadfiles<-gsub('.shp','',roadfiles)

#nested loop to compare each roadfile with each point.
#set up so it can be broken apart by roadfile or by lake.

numroadfiles=length(roadfiles)
for(r in 1:numroadfiles)
{
	roads<-readShapeLines(paste(roadfilepath,roadfiles[r],sep=''),proj4string=CRS("+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0"))
	roads<-spTransform(roads,CRS(proj4string(roadproxpoints)))

	lakes<-unique(roadproxpoints$SITE_ID)
	numlakes=length(lakes)


	for(i in 1:numlakes)
	{
		lake.points<-roadproxpoints[roadproxpoints$SITE_ID == lakes[i],]
		numpoints<-dim(lake.points)[1]
		distances<-vector('numeric',length=numpoints)
		for(j in 1:numpoints)
		{
			distances[j]<-gDistance(lake.points[j,],roads)
		}
		distance.df<-data.frame(SITEID=lakes[i],min.dist.to.road=distances,point.num=1:numpoints,roadfilenum=r)
		write.table(distance.df,'roadProximityResults.csv',quote=F,row.names=F,col.names=F,sep=',',append=T)
    print(100*i/numlakes)
	}
}


#after comparing all road files to all lakepoints, the data can be combined to find the true minimum distance to nearest road for each point and calculated averages for each lake.
distances<-read.csv('roadProximityResults.csv',header=F)
names(distances)=c('SITE_ID','dist.nearest.road','point.num','roadfile.num')
distances<-tapply(distances$dist.nearest.road,list(distances$SITE_ID,distances$point.num),min)
mean.mindistance<-apply(distances,1,function(x) mean(x[!is.na(x)]))
mean.mindistance<-data.frame(SITE_ID=names(mean.mindistance),mean.roadproximity=mean.mindistance)


