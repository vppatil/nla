#Vijay Patil
#this is a 2 part script. The first part downloads geodatabases containing nhd lake shapefiles for 
#the entire lower 48

#at the moment, these databases can't be accessed directly from R
#however, once the shapefiles are extracted, they can be clipped
#to include only NLA lakes

#the second part of this script does the clipping, and creates a single shapefile
#with lake polygons for all 1000+ nla lakes

#the gdb->shapefile extraction is currently dealt with in Arc gis software using the 
#gdbtoshp.py python script.

#I can post individual shapefiles on dropbox if you email me.


setwd('C:/Users/Sam/Desktop/gdbs')
zip.files=read.table('filenames.txt',header=FALSE,stringsAsFactor=FALSE)[,1]
zip.files=zip.files[grep('v210',zip.files)]
url = "ftp://nhdftp.usgs.gov/DataSets/Staged/SubRegions/PersonalGDB/HighResolution/"
shell.exec(url)


l=length(zip.files) #or whatever else
for(i in 214:l)
{
    download.url=paste(url,zip.files[i],sep='')
    filename=as.character(zip.files[i])
    download.file(download.url,filename)
    unzip(filename) #use with caution- unzips as it goes
}


#in the middle here, all the gdbs have to be extracted to shapefiles by arc. see the gdptoshp.py file

#####################################################################################################


#                                     PART TWO                                                      #


#####################################################################################################

#the second part of this script takes all the extracted shapefiles, overlays them with nla, and spits out a new overlaid shapefile
#first read in the nla data
library(maps)
library(maptools) #maptools seems better at reading/plotting shapefiles than the shapefiles package. don't know why.
library(sp)
library(rgeos)
library(rgdal)
library(proj4)
library(shapefiles)
library(raster)


#NOTE: I am inconsistent in the functions used to open and write shapefiles
#as of feb 2013, I think the readOGR() function in the sp package is probably the best.

setwd('C:/Users/sam/dropbox/NLA analyses/data/shapefiles/nlaNHD')
nla<-readOGR('.','nlapoints') #this reads in the file and gives it projection information
merge.dat<-readOGR('c:/users/sam/dropbox/nla analyses/data/shapefiles/nlaNHD','combine_lakes_fulldata')

nla<-nla[nla$SITE_ID %in% merge.dat$SITE_ID == FALSE,]
writeOGR(nla, dsn="missing_data.shp", layer="combined", driver="ESRI Shapefile",)


#this generates a list of all the nhd shapefiles saved on my harddrive
#setwd('D:/gleon gis/nhdShps')
setwd('c:/users/sam/desktop/vijay/nhdShps')

files=dir(pattern='.shp')
files=files[-grep('shp.',files)]
files=gsub('.shp','',files)

#finally, the third part merges all these shapefiles together 
for(i in 1:length(files))
{
	nhd.water<-readShapePoly(files[i],proj4string=CRS('+proj=latlong +datum=NAD83'))
	nhd.water<-spTransform(nhd.water,proj4string(nla))
	nhd.nla<-nhd.water[nla,]
	overlay<-over(nla,nhd.water)#find polygons that contain the coordinates from nla dataset
	new.dat=cbind(nla@data,overlay) #new.dat merges the nla and nhd data tables for all nla lakes
	new.dat=new.dat[!is.na(new.dat$Permanent_),] #remove na's, leaving the overlapping lake subset
	if(dim(new.dat)[1]>0)
	{
		new.dat.match=new.dat[match(nhd.nla$Permanent_,new.dat$Permanent_),]
		
		#now this new data table can be merged into the shapefile
		nhd.nla@data = new.dat.match #the @ operator accesses the attribute table in the shapefile

		#now I can write this new shapefile out to a file
		writeSpatialShape(nhd.nla,paste('nhdNLA',i,sep='_'))
	}
}
setwd('C:/Users/sam/desktop/vijay/nhdnla')


#the final step is to merge all the lake shapefiles into 1.
#first all the spatially joined nla/nhd shapefiles have to be listed:


#this step deletes all the unnecessary temporary files
rmfiles=dir(pattern = 'Waterbody')
for(i in 1:length(rmfiles))
{
	file.remove(rmfiles[i])
}

files=list.files(pattern='shp')
files=files[-grep('xml',files)]
files=files[grep('nlaNHD',files)]

dbfs<-dir(pattern='.dbf')
dbfs=dbfs[grep('nlaNHD',dbfs)]



#read in first shapefile and data file
data.first<-readOGR(files[1],gsub(".shp","",files[1]))
poly.temp<-slot(data.first,"polygons")
dbf.temp<-read.dbf(dbfs[1])$dbf

for(i in 2:length(files)){
  data.temp<-readOGR(files[i],gsub(".shp","",files[i]))
  poly.temp<-c(slot(data.temp,"polygons"),poly.temp)
  dbf2<-read.dbf(dbfs[i])$dbf
  dbf2<-dbf2[,na.omit(match(names(dbf.temp),names(dbf2)))]
  dbf.temp<-rbind(dbf.temp,dbf2)
}

for(i in 1:length(poly.temp)){
    slot(poly.temp[[i]],"ID") <-paste(i)
}

spatialPolygons.merge<-SpatialPolygons(poly.temp)
spldf<-SpatialPolygonsDataFrame(spatialPolygons.merge,dbf.temp)


#haven't figured out how to add projection information here yet, but this is easily added when 
#reading the shapefile back into R

writeOGR(spldf, dsn="combined_lakes.shp", layer="combined", driver="ESRI Shapefile",)
combined_proj<-readShapePoly('combined_lakes',proj4string=CRS(proj4string(nla)))
writeSpatialShape(combined_proj,'combined_lakes')


