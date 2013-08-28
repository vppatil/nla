#file to extract road length measurements from all nla buffers
#updated 4/22- now uses the gIntersection function instead of over()
#also incorporates relative paths so you shouldn't have to keep resetting them.

#set the working directory to nla analyses/ in the dropbox folder

#finally, uses 1182 clipped road length files instead of the road merge file so the gIntersection function doesn't break
#includes error trapping and overlap checking so it shouldn't waste time comparing buffers to road layers that don't intersect.

#still need to sum road lengths by lake id, link to nla names, and divide by area of nla buffers to get density. (use gArea(byid=T)


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

#roadfilepath=('C:/users/brad griffith/documents/roads')
roadfilepath=('/media/vijay/OS/Users/Sam/Documents/roads' )

setwd('~/Dropbox/NLA analyses/data/shapefiles/nlaNHD')
roadbuffer<-fastshp::read.shp('roadbufferjoin.shp',format='list')
road.dbf<-read.dbf('roadbufferjoin.dbf')$dbf

buffer.road<-subset(road.dbf,select=c('Permanent_'))
buffer.road$roadlen=-99999999

for(i in 1:length(roadbuffer))
{
	road.coords<-cbind(roadbuffer[[i]]$x,roadbuffer[[i]]$y)
	road.coords<-project(road.coords,'+proj=cea')
	road.dist<-roadlen(road.coords)
	buffer.road$roadlen[i]=road.dist
}	

	
miss.vec<-read.csv('c:/users/brad griffith/Dropbox/missvec.csv',header=T)


library(raster)

setwd('~/Dropbox/NLA analyses/data/shapefiles/nlaBuffers')
nlabuffer.old<-readOGR('.','NLA200mbuf')
nlabuffer<-spTransform(nlabuffer.old,CRS('+proj=cea'))

nla.num=dim(nlabuffer)[1] #number of lakes to go through

roadclippath=roadfilepath
  setwd(roadclippath)
clipfiles<-dir(pattern='tl_')
clipfiles<-clipfiles[grep('.shp',clipfiles)]
#clipfiles<-gsub('.shp','',clipfiles);  
clipfiles=clipfiles[-grep('.xml',clipfiles)]
num.clips=length(clipfiles)

miss.vec=c(miss.vec$x)

options(warn=-1)
library(fastshp)
  library(raster)
  library(snow)
  library(Rmpi)
num.lakes=dim(nlabuffer)[1]

#swith to 1:num.lakes after cleanup
miss.vec<-vector()
#HAVE TO BE CAREFUL WITH NULL OVERLAPS, NULL CLIPPED ROAD FILES.


j=missing.name.ind
nlabuffi=1:num.clips

########################getting list of overlapping road files for each buffer
  roadextent.list<-list()
  for(i in 1:length(clipfiles))
  {
    road.fast<-fastshp::read.shp(paste(roadfilepath,clipfiles[i],sep='/'),format='table')
    xmin=min(road.fast$x)
    xmax=max(road.fast$x)
    ymin=min(road.fast$y)
    ymax=max(road.fast$y)
    roadextent.list[[i]]=extent(xmin,xmax,ymin,ymax)
  }
  
  bufferextent.list<-list()
  for(j in 1:dim(nlabuffer.old)[1])
  {
    bufferbuffer.bbox<-summary(nlabuffer.old[j,])$bbox
    xmin=bufferbuffer.bbox[1,1]
    xmax=bufferbuffer.bbox[1,2]
    ymin=bufferbuffer.bbox[2,1]
    ymax=bufferbuffer.bbox[2,2]
    bufferextent.list[[j]]=extent(xmin,xmax,ymin,ymax)
  }
  
  buffer.road.overlap<-list()
  for(i in c(1:length(bufferextent.list)))
  {
    buffer.road.overlap[[i]] = vector()
    for(j in 1:length(roadextent.list))
    {
      if(!is.null(intersect(roadextent.list[[j]],bufferextent.list[[i]])))
      {print(j)
       buffer.road.overlap[[i]]<-c(buffer.road.overlap[[i]],j)}
    }
  }
  ##############################################################################################

  
  bufferRoadDens.func<-function(j)
   {
		print(j)
		nla.sub<-nlabuffer[j,]
		nla.extent.old=extent(nlabuffer.old[j,])
		nla.extent=extent(nla.sub)
	  num.clips=buffer.road.overlap[[j]]
	road.ext.list=list()
  if(length(num.clips)>0)  
	{
      for(i in 1:num.clips)
    	{
    		roadfile<-paste(roadclippath,clipfiles[i],sep='/')
    		roads<-try(fastshp::read.shp(roadfile,format='table'),silent=T)
    		if(!(class(roads)=='try-error'))
    		{		
    			road.ext=extent(roads)
    			if(!is.null(intersect(road.ext,nla.extent.old)))
    			{
    		
    				roads<-readShapeLines(roadfile,proj4string= CRS("+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0"),repair=T)
    				if(!is.null(dim(roads)))
    				{	
    								roads<-spTransform(roads,CRS('+proj=cea'))
    								try(rm('nla.roadclip'),silent=T)
    								nla.roadclip<-try(gIntersection(nla.sub,roads),silent=T)
    								if(!is.null(nla.roadclip))
    								{
    									len<-road.lencalc(nla.roadclip)
    									buffer.road<-rbind(buffer.road,data.frame(Permanent_=as.character(nlabuffer$Permanent_[j]),roadlen=len))
    							
    								}
    						write.csv(buffer.road,'~/Desktop/roadlengthsByNLAbuff_cluster.csv',row.names=F)
    
    				}
    			}
    		}
    	}	
  } else buffer.road<-rbind(buffer.road,data.frame(Permanent_=as.character(nlabuffer$Permanent_[j]),roadlen=0))
    
        return(buffer.road)
 }
 
  
  ###########################################################################
  buffer.road<-data.frame(SITE_ID='',roadlen=0)
  buffer.road$SITE_ID<-as.character(buffer.road$SITE_ID)
  buffer.road<-buffer.road[-1,]
  
  cl<-makeCluster(6,type='MPI')
  j=as.array(1:1057)
  clusterExport(cl,ls())
  buffer.road<-clusterApplyLB(cl,j,bufferRoadDens.func)
  
  stopCluster(cl)
  
buffer.road<-tapply(buffer.road$roadlen,buffer.road$Permanent_,sum)/1000 #in km
buffer.road<-data.frame(SITE_ID=names(buffer.road),roadlen=buffer.road)
nlabuffer.areas<-gArea(nlabuffer,byid=T)/1000000 #in km2
nlabuffer.areas<-data.frame(area=nlabuffer.areas,Permanent_=nlabuffer$Permanent_)
nlabuffer.areas<-merge(nlabuffer.areas,buffer.road)
nlabuffer.areas$roadDens<-nlabuffer.areas$roadlen/nlabuffer.areas$area
write.csv(nlabuffer.areas,'~/Desktop/roadDensityBynlabuffer1_975.csv')

lakedbf<-read.dbf('../nlanhd/combine_lakes_fulldata.dbf')$dbf
lakedbf<-subset(lakedbf,select=c('SITE_ID','SITE_ID'))

nlabuffer.roaddens<-merge(nlabuffer.areas,lakedbf)

  
  
#### working with NLCD impervious surface.
library(raster)
nlcd.imp<-raster('c:/users/brad griffith/desktop/marmottemp/nlcd2006_impervious_5-4-11.img')

nlabuffer.proj<-spTransform(nlabuffer,CRS(proj4string(nlcd.imp)))
nlabuffer.num<-dim(nlabuffer.proj)[1]
impsurf.df<-data.frame(SITE_ID='',mean.pct.imp=0,pixels.w.impsurf=0,stringsAsFactors=FALSE)

#was going really slowly trying to do the whole extraction at once, so I broke it out into a loop.
for( i in 1:nlabuffer.num)
{
	print(paste((i/nlabuffer.num),'% complete'))
	nlcd.crop<-crop(nlcd.imp,nlabuffer.proj[i,])
	nla.buffer.impSurf<-extract(nlcd.crop,nlabuffer.proj[i,])
	mean.pct.imp<-mean(nla.buffer.impSurf[[1]])
	pixels.w.impsurf<-sum(table(nla.buffer.impSurf[[1]]>0)[2])/length(nla.buffer.impSurf[[1]])
	pixels.w.impsurf<-ifelse(is.na(pixels.w.impsurf),0,pixels.w.impsurf)
	impsurf.df[i,]=data.frame(SITEID=as.character(nlabuffer.proj$SITE_ID[i]),mean.pct.imp=mean.pct.imp,pixels.w.impsurf=pixels.w.impsurf,stringsAsFactors=FALSE)
	write.csv(impsurf.df,'c:/users/brad griffith/desktop/buffers_nlcd_imperviousSurface.csv',row.names=FALSE)
}

nlanhd<-subset(read.dbf('../nlaNHD/combine_lakes_fulldata.dbf')$dbf,select=c('SITE_ID','SITE_ID'))
impsurf.df2<-merge(impsurf.df,nlanhd,by='SITE_ID',all.x=T)
write.csv(impsurf.df2,'c:/users/brad griffith/desktop/buffers_nlcd_imperviousSurface.csv',row.names=FALSE)
load(
