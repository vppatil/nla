#Vijay Patil
#Jan 31, 2013

#NLA 200m buffer creation, modified from luke's skills sharing exercise

#right now it creates all the buffer files individually, outputs them to shapefiles, then reads them back in to merge them
#this would be WAY more efficient to do in one step, but I was having trouble getting that to work.

#Read in shape
library(sp)
library(rgeos)
library(rgdal)
library(shapefiles)

setwd('C:/users/sam/dropbox/nla analyses/data/shapefiles/nlanhd')
nla = readOGR('.','combine_lakes_fulldata')
nla<-spTransform(nla,CRS('+proj=cea'))

setwd('c:/users/sam/documents/gleon buffers')

for(i in 1:dim(nla)[1])
{
	nlasub=nla[i,]
	nlaDat = nlasub@data
	buf=gBuffer(nlasub,width=200)

	newHoles = nlasub@polygons[[1]]@Polygons

	#Reverse each to make holes filled and vice-versa
	
	  newHoles[[1]]@hole = !newHoles[[1]]@hole
	  #reverse direction of coordinates (also means hole/not hole)
	  newHoles[[1]]@coords = apply(newHoles[[1]]@coords,2,rev)
	

	#Create a new SpatialPolygons object and keep proj4string (projection) info
	donut = SpatialPolygons(
	  list(Polygons(c(newHoles,buf@polygons[[1]]@Polygons),ID=row.names(nlaDat))),
	  proj4string=nlasub@proj4string)


	#Here is our buffer around nlasub!
	#plot(donut,col="Green",add=T)

	nlaDat = nlaDat[,names(nlaDat) %in% c('ComID','Permanent_','FDate','Resolution','GNIS_ID','GNIS_Name','ReachCode','FType','FCode')]

	#I guess it needs to be in this data structure
	donut = SpatialPolygonsDataFrame(donut,nlaDat)

	#Write!
	writeOGR(donut,'.',paste('nlaDonut',i,sep='_'),'ESRI Shapefile',overwrite_layer=T)
}

#now to add them all together again. 


files=list.files(pattern='shp')
ord<-order(as.character(1:length(files))

files<-files[order(as.numeric(ord))]

dbfs<-dir(pattern='.dbf')
dbfs<-dbfs[order(as.numeric(ord))]

#read in first shapefile and data file
data.first<-readOGR(files[1],gsub(".shp","",files[1]))
poly.temp<-slot(data.first,"polygons")
dbf.temp<-read.dbf(dbfs[1])$dbf

endf = length(files)
for(i in 2:endf){
  data.temp<-readOGR(files[i],gsub(".shp","",files[i]))
  poly.temp<-c(poly.temp,slot(data.temp,"polygons"))
  dbf2<-read.dbf(dbfs[i])$dbf
  dbf2<-dbf2[,na.omit(match(names(dbf.temp),names(dbf2)))]
  dbf.temp<-rbind(dbf.temp,dbf2)
}

for(i in 1:length(poly.temp)){
    slot(poly.temp[[i]],"ID") <-paste(i)
}

spatialPolygons.merge<-SpatialPolygons(poly.temp,proj4string=CRS(proj4string(data.first)))
spldf<-SpatialPolygonsDataFrame(spatialPolygons.merge,dbf.temp)


#haven't figured out how to add projection information here yet, but this is easily added when 
#reading the shapefile back into R
setwd('c:/users/sam/dropbox/nla analyses/data/shapefiles/nlabuffers')
writeOGR(spldf, dsn="NLA200mbuf.shp", layer="combined", driver="ESRI Shapefile",)


#clean up pieces
setwd('c:/users/sam/documents/gleon buffers')
rmfiles=dir(pattern = 'nlaDonut')
for(i in 1:length(rmfiles))
{
	file.remove(rmfiles[i])
}
