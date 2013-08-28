#GENERATES A SHAPEFILE WITH POINTS SPACED AT 50M INTERVALS AROUND THE PERIMETER OF EACH NLA LAKE


library(sp)
library(shapefiles)
library(rgeos)
library(rgdal)
library(maptools)
library(raster)

setwd('nla analyses/data/shapefiles/nlanhd/')
lakes<-readShapePoly('combine_lakes_fulldata.shp',proj4string= CRS("+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0"))
lakes<-spTransform(lakes,CRS('+proj=cea'))
lake.lines<-as(lakes,"SpatialLines")

num.lakes<-dim(lakes)[1]
interval.points.allLakes<-data.frame(SITE_ID='',X=0,Y=0,stringsAsFactors=FALSE)
interval.points.allLakes<-interval.points.allLakes[-1,]

for(i in 1:num.lakes){

	lake<-lake.lines[i,]
	coords<-coordinates(lake)[[1]][[1]]
	num.coords<-dim(coords)[1]
	#start.point<-brad griffithple(1:num.coords,1)
	#coords.shuffle<-coords[c(start.point:num.coords,1:start.point),]
	dists<-len.vector(coords)
	
	interval.points<-data.frame(X=coords[1,1],Y=coords[1,2],stringsAsFactors=FALSE)
	cumul.dist=0
	for(j in 1:(num.coords-1))
	{
		if(cumul.dist + dists[j]<50) #if segment is shorter than 50, add to distance tally and move to next point
		{	
			cumul.dist= cumul.dist + dists[j]
		} else #there is a new point within the next line segment
		{
			s=coords[j,];e=coords[j+1,] #get endpoints of current line segment
			remaining.dist = 50-cumul.dist #distance to travel along line segment to next point
			new.point = c((remaining.dist/dists[j])*(e[1]-s[1])+s[1],(remaining.dist/dists[j])*(e[2]-s[2])+s[2])
			interval.points<-rbind(interval.points,new.point)
			segment.remaining = len(new.point[1],new.point[2],e[1],e[2])	#check if there is another point in this segment		
			while(segment.remaining >=50)
			{
				s = new.point
				
				remaining.dist = 50 #distance to travel along line segment to next point
				new.point = c((remaining.dist/segment.remaining)*(e[1]-s[1])+s[1],((remaining.dist/segment.remaining)*(e[2]-s[2])+s[2]))
				interval.points<-rbind(interval.points,new.point)
				segment.remaining = len(new.point[1],new.point[2],e[1],e[2])
				# now figure out if there is another 50m left in the brad griffithe line segment
			}
			 cumul.dist=segment.remaining

		}	
			
	}
	interval.points$SITE_ID=lakes$SITE_ID[i]
	interval.points.allLakes<-rbind(interval.points.allLakes,interval.points)
}

intervalpoints.id<-data.frame(Id=row.names(interval.points.allLakes),SITE_ID = interval.points.allLakes$SITE_ID)

intervalpoints.sp<-SpatialPoints(interval.points.allLakes[,1:2],proj4string=CRS("+proj=cea"))
intervalpoints.spldf<-SpatialPointsDataFrame(intervalpoints.sp,data=intervalpoints.id)
writeOGR(intervalpoints.spldf,dsn="roadproximitypoints.shp", layer="combined", driver="ESRI Shapefile")

#compare with roadbuffermerge.shp- all roads w/in 10km  buffer of an nla lake.
#run gDistance, get min for each point. will take a long time.

roadbuffer<-readShapeLines('c:/users/sam/documents/roadclip/roadbuffermerge.shp')

  
  len<-function(startx,starty,endx,endy)
  {
    len<-sqrt((endy-starty)^2+(endx-startx)^2)
    return(len)
  }  


len.vector<-function(coords)
{   
  c.dim<-dim(coords)

				
            starts<-coords[1:c.dim[1]-1,]
            ends<-coords[2:c.dim[1],]
            dists<-len(starts[,1],starts[,2],ends[,1],ends[,2])
			
  return(dists)
}

road.lencalc<-function(roadsshp)
{
  coords<-coordinates(roadsshp)
  coords<-coords[[1]]
  
  roadsum<-sapply(coords,roadlen)
  roadsum<-sum(roadsum)
  return(roadsum)
}

