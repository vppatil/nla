#vijay patil
# april 2013

#code to estimate road density in nla watersheds
#roadlen function returns total length of a series of lines represented by their endpoint coordinates
#road.lencalc is a wrapper that calls roadlen for a spatialLines shapefile, such as the TIGER road census files.

enableJIT(1)
roadlen<-function(coords)
{
  
  len<-function(startx,starty,endx,endy)
  {
    len<-sqrt((endy-starty)^2+(endx-startx)^2)
    return(len)
  }  
  
  c.dim<-dim(coords)
  if(c.dim[1]==2){starts = coords[1,];ends=coords[2,];
                  dist=sqrt((ends[1]-starts[1])^2+(ends[2]-starts[2])^2)} else{	
                    starts<-coords[1:c.dim[1]-1,]
                    ends<-coords[2:c.dim[1],]
                    dists<-len(starts[,1],starts[,2],ends[,1],ends[,2])
                    dist<-sum(dists)}
  return(dist)
}

road.lencalc<-function(roadsshp) #wrapper for shapefiles
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
library(raster)
library(fastshp)
library(compiler)

#make sure to start in nla anlayses directory
watersheds<-readOGR('data/shapefiles/nlaWatersheds','NLA_Sites_Sampled_DrainageBasins')
watersheds.proj<-spTransform(watersheds,CRS('+proj=cea')) #convert to metric units

roadfilepath="/media/vijay/OS/Users/Sam/Documents/roads"
#path to directory with downloaded road shapefiles



options(warn=-1)
files<-dir(path=roadfilepath,pattern='.shp')
files<-files[-c(grep('.xml',files))]
num.files=length(files)
watershed.num=dim(watersheds)[1]

roadextent.list<-list()
for(i in 1:length(files))
{
  road.fast<-read.shp(paste(roadfilepath,files[i],sep='/'),format='table')
  xmin=min(road.fast$x)
  xmax=max(road.fast$x)
  ymin=min(road.fast$y)
  ymax=max(road.fast$y)
  roadextent.list[[i]]=extent(xmin,xmax,ymin,ymax)
}

watershedextent.list<-list()
for(j in 1:dim(watersheds)[1])
{
  watershedbuffer.bbox<-summary(watersheds[j,])$bbox
  xmin=watershedbuffer.bbox[1,1]
  xmax=watershedbuffer.bbox[1,2]
  ymin=watershedbuffer.bbox[2,1]
  ymax=watershedbuffer.bbox[2,2]
  watershedextent.list[[j]]=extent(xmin,xmax,ymin,ymax)
}


watershed.road.overlap<-list()
for(i in c(1:length(watershedextent.list)))
{
  watershed.road.overlap[[i]] = vector()
  for(j in 1:length(roadextent.list))
  {
    if(!is.null(intersect(roadextent.list[[j]],watershedextent.list[[i]])))
      watershed.road.overlap[[i]]<-c(watershed.road.overlap[[i]],j)
  }
}



#iterating through each road and watershed, extracting lengths of roads.
#watersheds done one at a time to save time.
#maptools readShapeLines function reads road files ~4 times faster than readOGR.

#spatial overlay performed with over() function.
#had to turn off errors to keep the loop from breaking.

watershed.road<-data.frame(SITEID='',roadlen=0)
watershed.road$SITEID<-as.character(watershed.road$SITEID)
watershed.road<-watershed.road[-1,]


badroads<-vector()

   for(j in 954:956)
  {
	 watershed.sub<-watersheds.proj[j,]
    roadfile.indices=watershed.road.overlap[[j]]
    print(paste("watershed",j))
    for(i in roadfile.indices)
    {
        print(paste("road file",i))
        shpname<-files[i]
       # shpname<-gsub('.shp','',shpname)
        roads<-readShapeLinestable(roadfilepath,shpname)
        roads<-spTransform(roads,CRS('+proj=cea'))
      
  			try(rm('watershed.over'),silent=T)
  			watershed.over<-try(gIntersection(watershed.sub,roads),silent=T)
        if(class(watershed.over)=='try-error')
        {
          badroads<-c(badroads,shpname)
          #roads=gSimplify(roads,tol=0.01)
          watershed.over<-NULL
        }
  			if(!is.null(watershed.over))
  			{
   				len<-road.lencalc(watershed.over) 
  				watershed.road<-rbind(watershed.road,data.frame(SITEID=as.character(watershed.sub$SITEID),roadlen=len))
  				write.table(watershed.road,'~/Ubuntu One/gleon/roadlengthsByWatershed12.csv',col.names=T,quote=F,row.names=F,sep=',',append=F)
  			}
    
		  }
	  }	
 
write.csv(badroads,'~/Ubuntu One/badroads3.csv')
  
watershed.roads<-read.csv('~/Ubuntu One/gleon/roadlengthsByWatershed.csv')[,-1]
watershed.roads<-tapply(watershed.roads$roadlen,watershed.roads$SITEID,sum)/1000 #in km
watershed.roads<-data.frame(SITEID=names(watershed.roads),roadlen=watershed.roads)
watershed.areas<-gArea(watersheds,byid=T)/1000000 #in km2
watershed.areas<-data.frame(area=watershed.areas,SITEID=watersheds$SITEID)
watershed.areas<-merge(watershed.areas,watershed.roads)
watershed.areas$roadDens<-watershed.areas$roadlen/watershed.areas$area
write.csv(watershed.road,'~/Ubuntu One/gleon/roadDensityByWatershed.csv')


#### working with NLCD impervious surface.
library(raster)
nlcd.imp<-raster('c:/users/brad griffith/desktop/marmotTemp/nlcd2006_impervious_5-4-11.img')

watersheds.proj<-spTransform(watersheds,CRS(proj4string(nlcd.imp)))
watershed.num<-dim(watersheds.proj)[1]
impsurf.df<-data.frame(SITEID='',mean.pct.imp=0,pixels.w.impsurf=0,stringsAsFactors=FALSE)


#was going really slowly trying to do the whole extraction at once, so I broke it out into a loop.
for( i in 1:watershed.num)
{
	print(paste(i/watershed.num,'% complete'))
	ex=extent(watersheds.proj[i,])
	nlcd.crop<-crop(nlcd.imp,watersheds.proj[i,])
	nla.watershed.impSurf<-extract(nlcd.crop,watersheds.proj[i,])
	mean.pct.imp<-mean(nla.watershed.impSurf[[1]])
	pixels.w.impsurf<-sum(table(nla.watershed.impSurf[[1]]>0)[2])/length(nla.watershed.impSurf[[1]])
	impsurf.df[i,]=data.frame(SITEID=as.character(watersheds.proj$SITEID[i]),mean.pct.imp=mean.pct.imp,pixels.w.impsurf=pixels.w.impsurf,stringsAsFactors=FALSE)
	write.csv(impsurf.df,'../../../output/nlabuffers_imperviousSurface.csv',row.names=FALSE)
}

nlanhd<-subset(read.dbf('../nlaNHD/combine_lakes_fulldata.dbf')$dbf,select=c('SITE_ID','Permanent_'))
impsurf.df2<-merge(impsurf.df,nlanhd,by='Permanent_',all.x=T)
write.csv(impsurf.df2,'c:/users/sam/dropbox/nla analyses/output/nlabuffers_imperviousSurface.csv',row.names=FALSE)



