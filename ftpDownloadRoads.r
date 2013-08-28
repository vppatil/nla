#this script does two things

#1) it extracts a list of downloadable files from the census bureau county road shapefile repository, downloads them all, and unzips them

#2) since these are all county road map shapefiles, it imports and merges them all into a larger shapefile,
#which it writes out to a file.


#step 1: source a package that lets you read filenames off a url
install.packages('stringr')
library(stringr)

#note that this should work for any url without changing the rest of the code as long as you have access to it.
library(stringr)
url = "http://www2.census.gov/geo/tiger/TIGER2012/ROADS/"

html <- paste(readLines(url), collapse="\n")
links <- str_match_all(html, "<a href=\"(.*?)\"") #anyone understand regular expressions? I just copied this off a website
links <- links[[1]][,2] #this gets a list of all links on page


zip.files<-links[grep('.zip',links)] #just get the zip files

#modify the download path as appropriate!
# setwd('c:/users/sam/documents/roads')
setwd("D:/BigDatasets/TIGER2012")

#the loop cycles through filenames and downloads them sequentially
#it also unzips them

l=length(zip.files) #set the number of files to download/unzip

for(i in 1:l)
{
    download.url=paste(url,zip.files[i],sep='')
    filename=zip.files[i]
    download.file(download.url,filename)
    unzip(filename) #use with caution- unzips as it goes
	unlink(filename)
}

###############the fully merged road layer is absurdly big, so I've only worked with 500 counties at once (out of 3000+)
setwd('c:/users/sam/documents/roads')
library(shapefiles)
library(rgdal)
library(maptools)
library(rgeos)

l=500
files=list.files(pattern='shp')
files=files[-grep('xml',files)]
files=files[-grep('combined',files)]
dbfs<-list.files(pattern='dbf')
dbfs<-dbfs[-grep('combined',dbfs)]

#read in first shapefile and data file
data.first<-readOGR(files[1],gsub(".shp","",files[1]))
lines.temp<-slot(data.first,"lines")
dbf.temp<-read.dbf(dbfs[1])$dbf


for(i in 2:l){
  data.temp<-readOGR(files[i],gsub(".shp","",files[i]))
  lines.temp<-c(slot(data.temp,"lines"),lines.temp)
  dbf.temp<-rbind(dbf.temp,read.dbf(dbfs[i])$dbf)
}

for(i in 1:length(lines.temp)){
    slot(lines.temp[[i]],"ID") <-paste(i)
}

#the following code creates the new shapefile, retaining projection information from the county files
spatialLines.merge<-SpatialLines(lines.temp,proj4string=CRS(proj4string(data.first)))
spldf<-SpatialLinesDataFrame(spatialLines.merge,dbf.temp)

writeOGR(spldf, dsn="combined_road1.shp", layer="combined", driver="ESRI Shapefile")
###############################################################################
